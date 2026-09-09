(* Token recognition (IEEE Std 1003.1-2017, XCU 2.3) and quoting (2.2).

   The rules of 2.3 are not those of an ordinary lexer: whether a
   character begins a new token depends on the quoting in force, and a
   here-document's body is not where its redirection is, but after the
   next newline.  So this pass produces tokens whose words are still
   *raw*, quotes and all, and reads here-document bodies as it crosses
   each newline; Word.parse then takes a raw word apart.  Scanning the
   quoting twice costs little and keeps each pass to one job.

   A word ends at an unquoted blank or operator character.  Inside it,
   '...' runs to the next quote, "..." honours backslash, and $(...),
   ${...} and $(( )) nest, so the scanner has to follow those to find
   where the word really ends: `echo "$(ls | wc -l)"' is one word. *)

type token =
  | Word of string                   (* raw text, quoting intact *)
  | Op of string
  | Newline
  | Io_number of int                 (* digits touching a < or > *)
  | Heredoc of string * bool         (* body; true if it expands *)
  | Eof

type lexed = { tok : token; line : int }

exception Error of string * int

let err line fmt = Printf.ksprintf (fun s -> raise (Error (s, line))) fmt

(* the operators of 2.10.2, longest first so that ';;' is not two ';' *)
let operators =
  [ "<<-"; "&&"; "||"; ";;"; "<<"; ">>"; "<&"; ">&"; "<>"; ">|";
    "&"; "|"; ";"; "<"; ">"; "("; ")" ]

let is_blank c = c = ' ' || c = '\t'
let is_op_start c = String.contains "&|;<>()" c
let is_name_start c = Posix.Regex.is_alpha c || c = '_'
let is_name_char c = Posix.Regex.is_alnum c || c = '_'

(* ---------- following the quoting ---------- *)

(* These skip functions all take the index of the construct's first
   character and return the index just past it.  They are shared with
   Word.parse, which needs the same nesting rules. *)

let rec skip_single src i n line =
  (* '...' : the only character that is not literal is the closing quote *)
  if i >= n then err line "unmatched '"
  else if src.[i] = '\'' then i + 1
  else skip_single src (i + 1) n line

let rec skip_double src i n line =
  if i >= n then err line "unmatched \""
  else
    match src.[i] with
    | '"' -> i + 1
    | '\\' when i + 1 < n -> skip_double src (i + 2) n line
    | '$' | '`' -> skip_double src (skip_expansion src i n line) n line
    | _ -> skip_double src (i + 1) n line

(* $... : a parameter, a command substitution or an arithmetic one.  Also
   used for a backquoted substitution, whose only escape is backslash. *)
and skip_expansion src i n line =
  if src.[i] = '`' then begin
    let rec go j =
      if j >= n then err line "unmatched `"
      else if src.[j] = '\\' && j + 1 < n then go (j + 2)
      else if src.[j] = '`' then j + 1
      else go (j + 1) in
    go (i + 1)
  end
  else if i + 1 >= n then i + 1                     (* a trailing '$' is literal *)
  else
    match src.[i + 1] with
    | '(' when i + 2 < n && src.[i + 2] = '(' -> skip_nested src (i + 3) n line 2 '(' ')'
    | '(' -> skip_nested src (i + 2) n line 1 '(' ')'
    | '{' -> skip_nested src (i + 2) n line 1 '{' '}'
    | c when is_name_start c ->
        let rec go j = if j < n && is_name_char src.[j] then go (j + 1) else j in
        go (i + 2)
    (* a single digit or one special character, so two characters in all *)
    | c when Posix.Regex.is_digit c -> i + 2
    | '@' | '*' | '#' | '?' | '-' | '$' | '!' -> i + 2
    | _ -> i + 1

(* skip to the matching close, counting nesting and honouring quotes *)
and skip_nested src i n line depth op cl =
  if depth = 0 then i
  else if i >= n then err line "unmatched %c" op
  else
    match src.[i] with
    | '\'' when op = '(' -> skip_nested src (skip_single src (i + 1) n line) n line depth op cl
    | '"' -> skip_nested src (skip_double src (i + 1) n line) n line depth op cl
    | '\\' when i + 1 < n -> skip_nested src (i + 2) n line depth op cl
    | '$' | '`' -> skip_nested src (skip_expansion src i n line) n line depth op cl
    | c when c = op -> skip_nested src (i + 1) n line (depth + 1) op cl
    | c when c = cl -> skip_nested src (i + 1) n line (depth - 1) op cl
    | _ -> skip_nested src (i + 1) n line depth op cl

(* ---------- the scanner ---------- *)

type state = {
  src : string;
  mutable i : int;
  mutable line : int;
  mutable toks : lexed array;
  mutable ntok : int;
  (* here-documents whose body has not been read: the delimiter, whether
     leading tabs are stripped, and where the body token goes *)
  mutable pending : (string * bool * int) list;
  mutable expect_heredoc : bool option;   (* Some strip, once << is seen *)
  mutable finished : bool;                (* the end of input has been reached *)
}

let emit st tok =
  if st.ntok = Array.length st.toks then begin
    let bigger = Array.make (2 * st.ntok + 16) { tok = Eof; line = 0 } in
    Array.blit st.toks 0 bigger 0 st.ntok;
    st.toks <- bigger
  end;
  st.toks.(st.ntok) <- { tok; line = st.line };
  st.ntok <- st.ntok + 1;
  st.ntok - 1

(* Is the raw delimiter quoted anywhere?  If it is, the body of the
   here-document is literal (2.7.4). *)
let delimiter raw =
  let b = Buffer.create (String.length raw) in
  let quoted = ref false in
  let n = String.length raw in
  let rec go i =
    if i < n then
      match raw.[i] with
      | '\'' ->
          quoted := true;
          let j = skip_single raw (i + 1) n 0 in
          Buffer.add_string b (String.sub raw (i + 1) (j - i - 2)); go j
      | '"' ->
          quoted := true;
          let j = skip_double raw (i + 1) n 0 in
          Buffer.add_string b (String.sub raw (i + 1) (j - i - 2)); go j
      | '\\' when i + 1 < n -> quoted := true; Buffer.add_char b raw.[i + 1]; go (i + 2)
      | c -> Buffer.add_char b c; go (i + 1) in
  go 0;
  (Buffer.contents b, not !quoted)

(* Read the bodies of the here-documents whose redirections have been
   seen, starting at the current position, which is just after a newline. *)
let read_heredocs st =
  let n = String.length st.src in
  List.iter (fun (delim, strip, slot) ->
      let body = Buffer.create 256 in
      let finished = ref false in
      while not !finished do
        if st.i >= n then finished := true      (* end of input ends the body *)
        else begin
          let stop = match String.index_from_opt st.src st.i '\n' with Some k -> k | None -> n in
          let raw = String.sub st.src st.i (stop - st.i) in
          let text = if strip then
              (let k = ref 0 in
               while !k < String.length raw && raw.[!k] = '\t' do incr k done;
               String.sub raw !k (String.length raw - !k))
            else raw in
          st.i <- if stop < n then stop + 1 else n;
          st.line <- st.line + 1;
          if text = delim then finished := true
          else (Buffer.add_string body text; Buffer.add_char body '\n')
        end
      done;
      let expand = match st.toks.(slot).tok with Heredoc (_, e) -> e | _ -> true in
      st.toks.(slot) <- { st.toks.(slot) with tok = Heredoc (Buffer.contents body, expand) })
    (List.rev st.pending);
  st.pending <- []

(* ---------- reading a line at a time ---------- *)

(* The scanner is asked for one line at a time rather than for the whole
   input at once.  That is how a shell reads (2.10.2 complete_command):
   it reads a line, runs what the line says, and only then reads the
   next, so a command early in a file runs even though a later line will
   not lex or parse.  A line's tokens are complete when [feed] returns,
   here-document bodies included, because a body is read as the newline
   after its redirection is crossed. *)

type t = state

let open_text src =
  { src; i = 0; line = 1; toks = Array.make 64 { tok = Eof; line = 0 };
    ntok = 0; pending = []; expect_heredoc = None; finished = false }

let count st = st.ntok
let nth st k = st.toks.(k)
let at_end st = st.finished

let feed st =
  let src = st.src in
  let n = String.length src in
  let finished = ref st.finished in
  while not !finished do
    (* blanks and comments separate tokens but are not tokens (2.3) *)
    let rec skip_blanks () =
      if st.i < n && is_blank src.[st.i] then (st.i <- st.i + 1; skip_blanks ())
      else if st.i + 1 < n && src.[st.i] = '\\' && src.[st.i + 1] = '\n' then
        (* a backslash-newline is a line continuation and disappears *)
        (st.i <- st.i + 2; st.line <- st.line + 1; skip_blanks ())
      else if st.i < n && src.[st.i] = '#' then begin
        while st.i < n && src.[st.i] <> '\n' do st.i <- st.i + 1 done;
        skip_blanks ()
      end in
    skip_blanks ();
    if st.i >= n then begin
      (* input that ends without a newline still owes its
         here-document bodies *)
      if st.pending <> [] then read_heredocs st;
      ignore (emit st Eof);
      st.finished <- true;
      finished := true
    end
    else if src.[st.i] = '\n' then begin
      st.i <- st.i + 1;
      ignore (emit st Newline);
      st.line <- st.line + 1;
      if st.pending <> [] then read_heredocs st;
      (* one line of tokens is enough for now *)
      finished := true
    end
    else if is_op_start src.[st.i] then begin
      let op = List.find (fun o ->
          let l = String.length o in
          st.i + l <= n && String.sub src st.i l = o) operators in
      st.i <- st.i + String.length op;
      ignore (emit st (Op op));
      if op = "<<" then st.expect_heredoc <- Some false
      else if op = "<<-" then st.expect_heredoc <- Some true
    end
    else begin
      (* a word: to the next unquoted blank, operator or newline *)
      let start = st.i in
      let stop = ref false in
      while not !stop do
        if st.i >= n then stop := true
        else
          match src.[st.i] with
          | c when is_blank c || c = '\n' || is_op_start c -> stop := true
          | '\'' -> st.i <- skip_single src (st.i + 1) n st.line
          | '"' -> st.i <- skip_double src (st.i + 1) n st.line
          | '\\' when st.i + 1 < n && src.[st.i + 1] = '\n' ->
              (* a continuation inside a word also disappears *)
              st.i <- st.i + 2; st.line <- st.line + 1
          | '\\' when st.i + 1 < n -> st.i <- st.i + 2
          | '$' | '`' -> st.i <- skip_expansion src st.i n st.line
          | _ -> st.i <- st.i + 1
      done;
      let raw = String.sub src start (st.i - start) in
      match st.expect_heredoc with
      | Some strip ->
          st.expect_heredoc <- None;
          let (delim, expand) = delimiter raw in
          let slot = emit st (Heredoc ("", expand)) in
          st.pending <- (delim, strip, slot) :: st.pending
      | None ->
          (* an IO number is digits that touch the redirection operator *)
          let all_digits = raw <> "" && String.for_all Posix.Regex.is_digit raw in
          if all_digits && st.i < n && (src.[st.i] = '<' || src.[st.i] = '>')
          then ignore (emit st (Io_number (int_of_string raw)))
          else ignore (emit st (Word raw))
    end
  done

(* the whole input at once, for the -n option, which reads a program to
   check it and runs none of it *)
let scan src =
  let st = open_text src in
  while not st.finished do feed st done;
  Array.sub st.toks 0 st.ntok

(* The tokens of a fragment of text, without the Eof that ends it: an
   alias's value, which is spliced into the stream where the alias name
   stood.  Every token is given the line the name was on, so that a
   diagnostic points at the line the script wrote rather than into the
   alias. *)
let scan_fragment ~line:ln text =
  let toks = scan text in
  let out = ref [] in
  Array.iter (fun t -> if t.tok <> Eof then out := { t with line = ln } :: !out) toks;
  List.rev !out
