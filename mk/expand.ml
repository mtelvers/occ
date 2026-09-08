(* Variable and function expansion (GNU make manual, chapters 6 and 8).

   Expansion rewrites a string by replacing every "$(...)" or "${...}"
   (and single-character "$x") with its value: a variable's value, or the
   result of one of make's built-in functions.  A recursively expanded
   variable's value is expanded again here at the point of use, which is
   what makes "$(call ...)" and deferred references work.

   The scanner finds the matching close paren itself so that nested
   references and commas inside sub-expressions are handled before the
   enclosing function splits its arguments. *)

let buf_create () = Buffer.create 64

exception Make_error of string

(* [eval] needs to parse and evaluate makefile text into the database,
   which lives in Eval; the hook is set there to break the cycle. *)
let eval_hook : (Value.t -> string -> unit) ref = ref (fun _ _ -> ())

module Expand_util = struct
  (* split [s] on top-level whitespace into words (make's notion of a list) *)
  let words s =
    List.filter (fun w -> w <> "")
      (String.split_on_char ' ' (String.map (fun c -> if c = '\t' || c = '\n' then ' ' else c) s))
  let unwords = String.concat " "
end
let words = Expand_util.words
let unwords = Expand_util.unwords

(* A function's expander takes the database, the argument-expander (for
   functions that expand lazily, like if and foreach) and the raw,
   comma-separated but not-yet-expanded arguments. *)
type context = {
  db : Value.t;
  call_stack : (string * string array) list;  (* $(call) frames: name and $1.. *)
  expanding : (string, unit) Hashtbl.t;        (* variables mid-expansion, to catch self-reference *)
}

let expand_depth = ref 0

let rec expand ctx (s : string) : string =
  incr expand_depth;
  if !expand_depth > 100000 then (decr expand_depth; raise (Make_error "expansion nested too deeply (recursive variable?)"));
  let r = expand_body ctx s in
  decr expand_depth; r

and expand_body ctx (s : string) : string =
  let b = buf_create () in
  let n = String.length s in
  let i = ref 0 in
  while !i < n do
    if s.[!i] = '$' && !i + 1 < n then begin
      let c = s.[!i + 1] in
      if c = '$' then (Buffer.add_char b '$'; i := !i + 2)
      else if c = '(' || c = '{' then begin
        let close = if c = '(' then ')' else '}' in
        let body, next = scan_ref s (!i + 2) c close in
        Buffer.add_string b (reference ctx body);
        i := next
      end else begin
        (* single-character variable, $x *)
        Buffer.add_string b (variable ctx (String.make 1 c));
        i := !i + 2
      end
    end else (Buffer.add_char b s.[!i]; incr i)
  done;
  Buffer.contents b

(* read up to the matching close, tracking nested ( ) or { } *)
and scan_ref s start openc closec =
  let n = String.length s in
  let depth = ref 1 and i = ref start in
  let b = buf_create () in
  while !i < n && !depth > 0 do
    let c = s.[!i] in
    if c = openc then (incr depth; Buffer.add_char b c)
    else if c = closec then (decr depth; if !depth > 0 then Buffer.add_char b c)
    else (Buffer.add_char b c);
    incr i
  done;
  Buffer.contents b, !i

(* A reference body: a function call "name args", or a variable name
   possibly with a substitution "$(VAR:a=b)". *)
and reference ctx body =
  match split_function body with
  | Some (name, args) when is_function name -> call_function ctx name args
  | _ ->
      (* $(VAR) or $(VAR:pat=repl) or $(VAR:suffix=repl) *)
      (match String.index_opt body ':' with
       | Some k when String.contains body '=' ->
           let name = String.sub body 0 k in
           let subst = String.sub body (k + 1) (String.length body - k - 1) in
           let value = variable ctx (expand ctx name) in
           (match String.index_opt subst '=' with
            | Some e ->
                let pat = String.sub subst 0 e and repl = String.sub subst (e + 1) (String.length subst - e - 1) in
                let pat = expand ctx pat and repl = expand ctx repl in
                unwords (List.map (subst_ref pat repl) (words value))
            | None -> value)
       | _ -> variable ctx (expand ctx body))

(* "$(VAR:.o=.c)" style: if [pat] has a %, it is a pattern; otherwise it
   is a suffix replacement. *)
and subst_ref pat repl word =
  if String.contains pat '%' then
    (match Func.pattern_match pat word with Some stem -> Func.pattern_subst repl stem | None -> word)
  else if Func.ends_with pat word then String.sub word 0 (String.length word - String.length pat) ^ repl
  else word

(* the value of a variable name, honouring automatic $(1).. inside a call
   and expanding recursively-flavoured values *)
and variable ctx name =
  match int_of_string_opt name with
  | Some k when ctx.call_stack <> [] ->
      let _, args = List.hd ctx.call_stack in
      if k >= 1 && k <= Array.length args then args.(k - 1) else ""
  | _ ->
      match Value.find ctx.db name with
      | Some { value; flavour = Value.Recursive; _ } ->
          (* a recursively expanded variable that refers to itself would
             loop forever; GNU make reports it, we break the cycle (6.2) *)
          if Hashtbl.mem ctx.expanding name then ""
          else begin
            Hashtbl.replace ctx.expanding name ();
            let r = expand ctx value in
            Hashtbl.remove ctx.expanding name; r
          end
      | Some { value; flavour = Value.Simple; _ } -> value
      | None -> ""

(* "name args" if the head is a known function name followed by a space *)
and split_function body =
  let n = String.length body in
  let j = ref 0 in
  while !j < n && (let c = body.[!j] in (c >= 'a' && c <= 'z') || c = '-') do incr j done;
  if !j > 0 && !j < n && (body.[!j] = ' ' || body.[!j] = '\t') then
    Some (String.sub body 0 !j, String.sub body (!j + 1) (n - !j - 1))
  else None

and is_function name = List.mem name Func.names

(* split a function's argument string on top-level commas (commas inside
   nested $(...) belong to sub-references) *)
and split_args ?(max = max_int) s =
  let n = String.length s in
  let parts = ref [] and start = ref 0 and depth = ref 0 and i = ref 0 and count = ref 0 in
  while !i < n do
    let c = s.[!i] in
    if c = '(' || c = '{' then incr depth
    else if c = ')' || c = '}' then decr depth
    else if c = ',' && !depth = 0 && !count < max - 1 then begin
      parts := String.sub s !start (!i - !start) :: !parts; start := !i + 1; incr count
    end;
    incr i
  done;
  parts := String.sub s !start (n - !start) :: !parts;
  List.rev !parts

(* ---- the built-in functions (8) ------------------------------------------------- *)

and call_function ctx name argstr =
  (* most functions expand their arguments; if, or, and, foreach and call
     expand lazily, so they split first and expand themselves *)
  let one () = expand ctx argstr in
  let split ?max () = List.map (expand ctx) (split_args ?max argstr) in
  let raw ?max () = split_args ?max argstr in
  let list_fn f = Expand_util.unwords (f (Expand_util.words (one ()))) in
  match name with
  | "subst" -> (match split () with [ from; into; text ] -> Func.subst from into text | _ -> "")
  | "patsubst" ->
      (match split () with
       | [ pat; repl; text ] ->
           Expand_util.unwords (List.map (fun w -> match Func.pattern_match pat w with Some st -> Func.pattern_subst repl st | None -> w) (Expand_util.words text))
       | _ -> "")
  | "strip" -> Expand_util.unwords (Expand_util.words (one ()))
  | "findstring" -> (match split () with [ find; text ] -> (if substring find text then find else "") | _ -> "")
  | "filter" -> (match split () with [ pats; text ] -> filter ~keep:true pats text | _ -> "")
  | "filter-out" -> (match split () with [ pats; text ] -> filter ~keep:false pats text | _ -> "")
  | "sort" -> Expand_util.unwords (List.sort_uniq compare (Expand_util.words (one ())))
  | "word" -> (match split () with [ n; text ] -> (match int_of_string_opt (String.trim n) with Some i -> (try List.nth (Expand_util.words text) (i - 1) with _ -> "") | None -> "") | _ -> "")
  | "wordlist" ->
      (match split () with
       | [ a; b; text ] ->
           (match int_of_string_opt (String.trim a), int_of_string_opt (String.trim b) with
            | Some s, Some e -> Expand_util.unwords (List.filteri (fun i _ -> i + 1 >= s && i + 1 <= e) (Expand_util.words text))
            | _ -> "")
       | _ -> "")
  | "words" -> string_of_int (List.length (Expand_util.words (one ())))
  | "firstword" -> (match Expand_util.words (one ()) with w :: _ -> w | [] -> "")
  | "lastword" -> (match List.rev (Expand_util.words (one ())) with w :: _ -> w | [] -> "")
  | "dir" -> list_fn (List.map Func.dir)
  | "notdir" -> list_fn (List.map Func.notdir)
  | "suffix" -> list_fn (List.filter_map (fun w -> match Func.suffix w with "" -> None | s -> Some s))
  | "basename" -> list_fn (List.map Func.basename)
  | "addsuffix" -> (match split () with [ suf; text ] -> Expand_util.unwords (List.map (fun w -> w ^ suf) (Expand_util.words text)) | _ -> "")
  | "addprefix" -> (match split () with [ pre; text ] -> Expand_util.unwords (List.map (fun w -> pre ^ w) (Expand_util.words text)) | _ -> "")
  | "join" -> (match split () with [ a; b ] -> join (Expand_util.words a) (Expand_util.words b) | _ -> "")
  | "wildcard" -> Expand_util.unwords (List.concat_map wildcard (Expand_util.words (one ())))
  | "realpath" | "abspath" -> list_fn (List.map abspath)
  | "if" ->
      (match raw () with
       | cond :: rest ->
           if String.trim (expand ctx cond) <> "" then (match rest with t :: _ -> expand ctx t | [] -> "")
           else (match rest with _ :: e :: _ -> expand ctx e | _ -> "")
       | [] -> "")
  | "or" -> or_and ctx (raw ()) ~stop_nonempty:true
  | "and" -> or_and ctx (raw ()) ~stop_nonempty:false
  | "foreach" ->
      (match raw ~max:3 () with
       | [ var; list; body ] ->
           let var = expand ctx var in
           Expand_util.unwords (List.filter_map (fun w ->
               Value.set ctx.db ~flavour:Value.Simple ~origin:Value.Automatic var w;
               let r = expand ctx body in if r = "" then None else Some r) (Expand_util.words (expand ctx list)))
       | _ -> "")
  | "call" ->
      (match raw () with
       | fn :: params ->
           let fn = String.trim (expand ctx fn) in
           let args = Array.of_list (List.map (expand ctx) params) in
           (match Value.find ctx.db fn with
            | Some { value; _ } -> expand { ctx with call_stack = (fn, args) :: ctx.call_stack } value
            | None -> "")
       | [] -> "")
  | "value" -> (match Value.find ctx.db (String.trim (one ())) with Some v -> v.value | None -> "")
  | "origin" ->
      (match Value.find ctx.db (String.trim (one ())) with
       | None -> "undefined"
       | Some v -> (match v.origin with
           | Value.Default -> "default" | Value.Environment -> "environment" | Value.File -> "file"
           | Value.Command_line -> "command line" | Value.Override -> "override" | Value.Automatic -> "automatic"))
  | "flavor" ->
      (match Value.find ctx.db (String.trim (one ())) with
       | None -> "undefined" | Some { flavour = Value.Recursive; _ } -> "recursive" | Some { flavour = Value.Simple; _ } -> "simple")
  | "eval" -> !eval_hook ctx.db (one ()); ""
  | "error" -> raise (Make_error (one ()))
  | "warning" -> Printf.eprintf "occmake: %s\n" (one ()); ""
  | "info" -> print_string (one ()); print_newline (); ""
  | "shell" -> shell (one ())
  | _ -> ""

and substring needle hay =
  let ln = String.length needle and lh = String.length hay in
  if ln = 0 then true
  else let rec go i = i + ln <= lh && (String.sub hay i ln = needle || go (i + 1)) in go 0

and filter ~keep pats text =
  let pats = Expand_util.words pats in
  Expand_util.unwords (List.filter (fun w ->
      let matched = List.exists (fun p -> Func.pattern_match p w <> None) pats in
      matched = keep) (Expand_util.words text))

and join a b =
  let rec go a b = match a, b with
    | x :: a, y :: b -> (x ^ y) :: go a b
    | x :: a, [] -> x :: go a []
    | [], y :: b -> y :: go [] b
    | [], [] -> [] in
  Expand_util.unwords (go a b)

and or_and ctx args ~stop_nonempty =
  let rec go last = function
    | [] -> if stop_nonempty then "" else last
    | a :: rest ->
        let v = String.trim (expand ctx a) in
        if stop_nonempty then (if v <> "" then v else go last rest)
        else (if v = "" then "" else go v rest) in
  go (if stop_nonempty then "" else "x") args

and wildcard pat =
  (* only a single '*' in the last component is supported, which is all the
     OCaml build uses *)
  if not (String.contains pat '*') then (if Sys.file_exists pat then [ pat ] else [])
  else begin
    let d = Func.dir pat and base = Func.notdir pat in
    let dir = if d = "./" then "." else d in
    match Sys.readdir dir with
    | entries ->
        let matched = List.filter (fun e -> Func.pattern_match (Func.subst "*" "%" base) e <> None) (Array.to_list entries) in
        List.sort compare (List.map (fun e -> if d = "./" then e else d ^ e) matched)
    | exception _ -> []
  end

and abspath p =
  let p = if Filename.is_relative p then Filename.concat (Sys.getcwd ()) p else p in
  (* collapse . and .. *)
  let parts = List.filter (fun s -> s <> "" && s <> ".") (String.split_on_char '/' p) in
  let rec norm acc = function
    | [] -> List.rev acc
    | ".." :: rest -> norm (match acc with _ :: t -> t | [] -> []) rest
    | x :: rest -> norm (x :: acc) rest in
  "/" ^ String.concat "/" (norm [] parts)

and shell cmd =
  let ic = Unix.open_process_in cmd in
  let b = buf_create () in
  (try while true do Buffer.add_channel b ic 1 done with End_of_file -> ());
  ignore (Unix.close_process_in ic);
  (* trailing newlines become spaces, others too (8.13) *)
  let out = Buffer.contents b in
  let out = if out <> "" && out.[String.length out - 1] = '\n' then String.sub out 0 (String.length out - 1) else out in
  String.map (fun c -> if c = '\n' then ' ' else c) out
