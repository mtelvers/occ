(* The shell grammar (IEEE Std 1003.1-2017, XCU 2.10), by recursive
   descent over the tokens.

   Two rules of 2.10.1 shape the code.  A reserved word ("if", "then",
   "do", "}"...) is only a reserved word where a command could begin, so
   `echo then' prints "then"; here that falls out of the structure,
   because a word list is only ever examined for reserved words at the
   head of a command or just after a separator.  And a redirection may
   appear anywhere among the words of a simple command, before them or
   after, so both are collected in one loop. *)

open Ast

exception Error of string * int

let err line fmt = Printf.ksprintf (fun s -> raise (Error (s, line))) fmt

type state = { toks : Lex.lexed array; mutable k : int }

let peek st = st.toks.(st.k).Lex.tok
let peek_at st d = if st.k + d < Array.length st.toks then st.toks.(st.k + d).Lex.tok else Lex.Eof
let line st = st.toks.(st.k).Lex.line
let advance st = if st.k < Array.length st.toks - 1 then st.k <- st.k + 1

let at_word st w = match peek st with Lex.Word x -> x = w | _ -> false
let at_op st o = match peek st with Lex.Op x -> x = o | _ -> false

let describe = function
  | Lex.Word w -> Printf.sprintf "word `%s'" w
  | Lex.Op o -> Printf.sprintf "`%s'" o
  | Lex.Newline -> "newline"
  | Lex.Io_number n -> Printf.sprintf "`%d'" n
  | Lex.Heredoc _ -> "here-document"
  | Lex.Eof -> "end of file"

let expect_word st w =
  if at_word st w then advance st else err (line st) "expected `%s' but found %s" w (describe (peek st))

let expect_op st o =
  if at_op st o then advance st else err (line st) "expected `%s' but found %s" o (describe (peek st))

let rec skip_newlines st = if peek st = Lex.Newline then (advance st; skip_newlines st)

(* the reserved words of 2.4 *)
let reserved = [ "!"; "{"; "}"; "case"; "do"; "done"; "elif"; "else"; "esac";
                 "fi"; "for"; "if"; "in"; "then"; "until"; "while" ]

let is_name s =
  s <> "" && Word.is_name_start s.[0] && String.for_all Word.is_name_char s

let redirect_ops = [ "<"; ">"; ">>"; "<<"; "<<-"; "<&"; ">&"; "<>"; ">|" ]

let at_redirect st =
  match peek st with
  | Lex.Io_number _ -> true
  | Lex.Op o -> List.mem o redirect_ops
  | _ -> false

(* ---------- the pieces ---------- *)

let rec program st stops =
  let out = ref [] in
  let rec go () =
    skip_newlines st;
    if not (at_stop st stops) then begin
      let ao = and_or st in
      let async = ref false in
      (match peek st with
       | Lex.Op "&" -> async := true; advance st
       | Lex.Op ";" -> advance st
       | Lex.Newline -> advance st
       | _ -> ());
      out := { ao; async = !async } :: !out;
      go ()
    end in
  go ();
  List.rev !out

(* Where a list ends: at the end of input, at a ')' or ';;' which belong
   to an enclosing construct, or at one of the reserved words the caller
   is waiting for. *)
and at_stop st stops =
  match peek st with
  | Lex.Eof -> true
  | Lex.Op (")" | ";;") -> true
  | Lex.Word w -> List.mem w stops
  | _ -> false

and and_or st =
  let first = pipeline st in
  let rest = ref [] in
  let rec go () =
    match peek st with
    | Lex.Op "&&" -> advance st; skip_newlines st; rest := (true, pipeline st) :: !rest; go ()
    | Lex.Op "||" -> advance st; skip_newlines st; rest := (false, pipeline st) :: !rest; go ()
    | _ -> () in
  go ();
  { first; rest = List.rev !rest }

and pipeline st =
  let negate = if at_word st "!" then (advance st; true) else false in
  let parts = ref [ command st ] in
  while at_op st "|" do
    advance st; skip_newlines st;
    parts := command st :: !parts
  done;
  { negate; parts = List.rev !parts }

and command st =
  match peek st with
  | Lex.Word "{" ->
      advance st;
      let body = program st [ "}" ] in
      expect_word st "}";
      Group (body, redirects st)
  | Lex.Op "(" ->
      advance st;
      let body = program st [] in
      expect_op st ")";
      Subshell (body, redirects st)
  | Lex.Word "for" -> for_clause st
  | Lex.Word "case" -> case_clause st
  | Lex.Word "if" -> if_clause st
  | Lex.Word "while" -> loop_clause st false
  | Lex.Word "until" -> loop_clause st true
  | Lex.Word name when is_name name && not (List.mem name reserved)
                       && peek_at st 1 = Lex.Op "(" && peek_at st 2 = Lex.Op ")" ->
      (* a function definition (2.9.5) *)
      advance st; advance st; advance st;
      skip_newlines st;
      Funcdef (name, command st)
  | _ -> simple st

and redirects st =
  let out = ref [] in
  while at_redirect st do out := redirect st :: !out done;
  List.rev !out

and redirect st =
  let ln = line st in
  let explicit = match peek st with Lex.Io_number k -> advance st; Some k | _ -> None in
  let op = match peek st with
    | Lex.Op o when List.mem o redirect_ops -> advance st; o
    | t -> err ln "expected a redirection but found %s" (describe t) in
  let default, rop = match op with
    | ">" -> 1, Out
    | ">|" -> 1, Out_force
    | ">>" -> 1, Append
    | "<" -> 0, In
    | "<>" -> 0, In_out
    | ">&" -> 1, Dup_out
    | "<&" -> 0, Dup_in
    | _ -> 0, Here true in
  let rword =
    match op with
    | "<<" | "<<-" ->
        (match peek st with
         | Lex.Heredoc (body, expand) ->
             advance st;
             (* An expanding body behaves as if double-quoted: one field,
                no splitting, but parameters and substitutions happen.  A
                quoted delimiter makes the whole body literal (2.7.4). *)
             if expand then [ Double (Word.heredoc body) ] else [ Single body ]
         | t -> err ln "missing here-document body: %s" (describe t))
    | _ ->
        (match peek st with
         | Lex.Word raw -> advance st; Word.parse raw
         | t -> err ln "expected a file name after `%s' but found %s" op (describe t)) in
  let rop = match op with "<<" -> Here true | "<<-" -> Here true | _ -> rop in
  { rfd = (match explicit with Some k -> k | None -> default); rop; rword }

and word_of st =
  match peek st with
  | Lex.Word raw -> advance st; Word.parse raw
  | t -> err (line st) "expected a word but found %s" (describe t)

and simple st =
  let ln = line st in
  let assigns = ref [] and words = ref [] and redirs = ref [] in
  let rec go () =
    if at_redirect st then (redirs := redirect st :: !redirs; go ())
    else
      match peek st with
      | Lex.Word raw ->
          (* an assignment only counts before the command name (2.9.1) *)
          (match (if !words = [] then Word.assignment raw else None) with
           | Some a -> advance st; assigns := a :: !assigns; go ()
           | None -> advance st; words := Word.parse raw :: !words; go ())
      | _ -> () in
  go ();
  if !assigns = [] && !words = [] && !redirs = [] then
    err ln "unexpected %s" (describe (peek st));
  Simple { assigns = List.rev !assigns; words = List.rev !words;
           redirs = List.rev !redirs; line = ln }

and for_clause st =
  advance st;
  let name = match peek st with
    | Lex.Word raw when is_name raw -> advance st; raw
    | t -> err (line st) "`for' wants a name, not %s" (describe t) in
  skip_newlines st;
  let items =
    if at_word st "in" then begin
      advance st;
      let ws = ref [] in
      let rec collect () =
        match peek st with
        | Lex.Word w when not (List.mem w [ "do"; "done" ]) ->
            ws := word_of st :: !ws; collect ()
        | _ -> () in
      collect ();
      Some (List.rev !ws)
    end else None in
  (match peek st with Lex.Op ";" | Lex.Newline -> advance st | _ -> ());
  skip_newlines st;
  expect_word st "do";
  let body = program st [ "done" ] in
  expect_word st "done";
  For (name, items, body, redirects st)

and case_clause st =
  advance st;
  let subject = word_of st in
  skip_newlines st;
  expect_word st "in";
  skip_newlines st;
  let items = ref [] in
  while not (at_word st "esac") && peek st <> Lex.Eof do
    if at_op st "(" then advance st;
    let pats = ref [] in
    let rec pat () =
      (match peek st with
       | Lex.Word raw -> advance st; pats := Word.parse raw :: !pats
       | t -> err (line st) "expected a pattern but found %s" (describe t));
      if at_op st "|" then (advance st; pat ()) in
    pat ();
    expect_op st ")";
    let body = program st [ "esac" ] in
    if at_op st ";;" then (advance st; skip_newlines st);
    items := (List.rev !pats, body) :: !items;
    skip_newlines st
  done;
  expect_word st "esac";
  Case (subject, List.rev !items, redirects st)

and if_clause st =
  advance st;
  let branches = ref [] in
  let cond = program st [ "then" ] in
  expect_word st "then";
  let body = program st [ "fi"; "else"; "elif" ] in
  branches := [ (cond, body) ];
  let rec more () =
    if at_word st "elif" then begin
      advance st;
      let c = program st [ "then" ] in
      expect_word st "then";
      let b = program st [ "fi"; "else"; "elif" ] in
      branches := !branches @ [ (c, b) ];
      more ()
    end in
  more ();
  let orelse =
    if at_word st "else" then begin
      advance st;
      Some (program st [ "fi" ])
    end else None in
  expect_word st "fi";
  If (!branches, orelse, redirects st)

and loop_clause st until =
  advance st;
  let cond = program st [ "do" ] in
  expect_word st "do";
  let body = program st [ "done" ] in
  expect_word st "done";
  Loop { until; cond; body; lredirs = redirects st }

(* ---------- entry point ---------- *)

let parse text =
  let toks = Lex.scan text in
  let st = { toks; k = 0 } in
  let p = program st [] in
  (match peek st with
   | Lex.Eof -> ()
   | t -> err (line st) "unexpected %s" (describe t));
  p

(* Word needs the parser for command substitution, and the parser needs
   Word for its words; this closes the circle. *)
let () = Word.program := parse
