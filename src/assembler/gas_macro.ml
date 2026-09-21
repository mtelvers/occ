(* GNU as's macro facility (GNU as manual, "Macro"), which the OCaml
   runtime's RISC-V half is built out of: twenty-four macros, some of
   them taking parameters.

       .macro SWITCH_OCAML_STACKS old_stack, new_stack
               sd      sp, Stack_sp(\old_stack)
               ...
       .endm

   Expansion is textual and happens before anything is parsed, which is
   what gas does too: a body is kept as the lines it was written as and
   re-read at each use, with \param replaced by the argument.  Doing it
   here rather than in the parser is what makes a macro able to hold
   labels, directives and instructions alike without the parser knowing
   anything about macros.

   The pieces of the facility this implements are the ones assembly in
   the wild uses: parameters with or without defaults, arguments given
   positionally or by name, \param and \() in the body, and macros that
   use other macros.  Anything else -- .exitm, .purgem, \@ -- is left
   alone, so a file that wants it fails at parse time rather than
   quietly assembling as something else. *)

type macro = {
  params : (string * string) list;   (* name and default, "" for none *)
  body : string list;
}

let is_ident_char c =
  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c = '_' || c = '.' || c = '$'

(* the first word of a line, and what follows it *)
let split_word line =
  let n = String.length line in
  let i = ref 0 in
  while !i < n && (line.[!i] = ' ' || line.[!i] = '\t') do incr i done;
  let start = !i in
  while !i < n && is_ident_char line.[!i] do incr i done;
  (String.sub line start (!i - start), String.sub line !i (n - !i))

(* Arguments and parameters are separated by commas, and gas also allows
   spaces; either way the pieces are trimmed. *)
let split_args s =
  String.split_on_char ',' s
  |> List.concat_map (fun piece -> String.split_on_char ' ' piece)
  |> List.concat_map (fun piece -> String.split_on_char '\t' piece)
  |> List.filter_map (fun piece ->
      let piece = String.trim piece in
      if piece = "" then None else Some piece)

let parse_params s =
  List.map (fun p ->
      match String.index_opt p '=' with
      | Some k -> String.sub p 0 k, String.sub p (k + 1) (String.length p - k - 1)
      | None -> p, "")
    (split_args s)

(* Substitute in one line of a body: "\name" becomes the argument, and
   "\()" is a separator that disappears, which is how a body glues a
   parameter to what follows it. *)
let substitute (bindings : (string * string) list) line =
  let b = Buffer.create (String.length line) in
  let n = String.length line in
  let i = ref 0 in
  while !i < n do
    if line.[!i] = '\\' && !i + 1 < n then begin
      if line.[!i + 1] = '(' && !i + 2 < n && line.[!i + 2] = ')' then i := !i + 3
      else begin
        let start = !i + 1 in
        let j = ref start in
        while !j < n && is_ident_char line.[!j] do incr j done;
        let name = String.sub line start (!j - start) in
        match List.assoc_opt name bindings with
        | Some v -> Buffer.add_string b v; i := !j
        | None -> Buffer.add_char b '\\'; incr i
      end
    end else begin Buffer.add_char b line.[!i]; incr i end
  done;
  Buffer.contents b

(* the arguments of one use, matched to the parameters: positional until
   a "name=value" appears, as gas takes them *)
let bind (m : macro) args =
  let positional = ref [] and named = ref [] in
  List.iter (fun a ->
      match String.index_opt a '=' with
      | Some k when k > 0 && List.mem_assoc (String.sub a 0 k) m.params ->
          named := (String.sub a 0 k, String.sub a (k + 1) (String.length a - k - 1)) :: !named
      | _ -> positional := a :: !positional)
    args;
  let positional = List.rev !positional in
  List.mapi (fun i (name, default) ->
      match List.assoc_opt name !named with
      | Some v -> name, v
      | None -> if i < List.length positional then name, List.nth positional i else name, default)
    m.params

let depth_limit = 100

(* The whole point of the care below: a line of the input must still be
   that line of the input after expansion, or every message and every
   .loc afterwards names the wrong place.  So a definition leaves as many
   blank lines as it took, and a use becomes one line holding its body's
   statements separated by semicolons, which is the other separator the
   assembler's syntax has. *)
let expand text =
  let macros : (string, macro) Hashtbl.t = Hashtbl.create 16 in
  let out = Buffer.create (String.length text) in
  let lines = Array.of_list (String.split_on_char '\n' text) in
  (* the statements one use of a macro stands for, expanded through any
     macros they use in turn *)
  let rec statements depth line =
    if depth > depth_limit then failwith "a macro that uses itself without end";
    let word, tail = split_word line in
    match Hashtbl.find_opt macros word with
    | None -> [ line ]
    | Some m ->
        let bindings = bind m (split_args tail) in
        List.concat_map (fun l -> statements (depth + 1) (substitute bindings l)) m.body in
  let i = ref 0 in
  while !i < Array.length lines do
    let word, tail = split_word lines.(!i) in
    if word = ".macro" then begin
      let name, params = split_word tail in
      let start = !i in
      incr i;
      let body = ref [] in
      let ended = ref false in
      while not !ended do
        if !i >= Array.length lines then failwith (".macro " ^ name ^ " without .endm");
        let w, _ = split_word lines.(!i) in
        if w = ".endm" then ended := true else body := lines.(!i) :: !body;
        incr i
      done;
      Hashtbl.replace macros name { params = parse_params params; body = List.rev !body };
      (* as many blank lines as the definition occupied *)
      for _ = start to !i - 1 do Buffer.add_char out '\n' done
    end else begin
      (* A label may share its line with the statement that follows it,
         and that statement may be a macro: "1: RESTORE_ALL_REGS" is how
         the OCaml runtime writes one. *)
      let prefix, rest =
        let after = String.trim tail in
        if after <> "" && after.[0] = ':' then
          word ^ ": ", String.sub after 1 (String.length after - 1)
        else "", lines.(!i) in
      Buffer.add_string out prefix;
      (match statements 0 rest with
       | [ one ] -> Buffer.add_string out one
       | many -> Buffer.add_string out (String.concat " ; " (List.map String.trim many)));
      Buffer.add_char out '\n';
      incr i
    end
  done;
  Buffer.contents out
