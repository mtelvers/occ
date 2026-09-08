(* find: walk a file hierarchy testing an expression (IEEE Std
   1003.1-2017, XCU find).

   The operands after the starting points are an expression, not
   options: primaries like -name and -type are tests, -print and -exec
   are actions, and they combine with -a, -o and ! and group with
   parentheses.  An expression with no action gets -print, which is why
   `find . -name x' prints anything it finds. *)

open Util

type expr =
  | True
  | Name of string
  | Path of string
  | Kind of char
  | Newer of float
  | Empty
  | Print of char                     (* the character that ends each name *)
  | Exec of string list * bool        (* the words, with {} standing for the name; true if ended by + *)
  | Prune
  | Not of expr
  | And of expr * expr
  | Or of expr * expr

type context = {
  mutable prune : bool;               (* set by -prune for the entry in hand *)
  mutable acted : bool;               (* an action was seen, so no default print *)
}

let kind_char = function
  | Unix.S_REG -> 'f' | Unix.S_DIR -> 'd' | Unix.S_LNK -> 'l'
  | Unix.S_CHR -> 'c' | Unix.S_BLK -> 'b' | Unix.S_FIFO -> 'p' | Unix.S_SOCK -> 's'

(* the expression, by recursive descent over the remaining operands *)
let parse ctx words =
  let a = Array.of_list words in
  let n = Array.length a in
  let pos = ref 0 in
  let peek () = if !pos < n then Some a.(!pos) else None in
  let take () = let v = a.(!pos) in incr pos; v in
  let need what =
    if !pos >= n then die 1 "%s: missing argument" what else take () in
  let rec disjunction () =
    let left = conjunction () in
    match peek () with
    | Some ("-o" | "-or") -> ignore (take ()); Or (left, disjunction ())
    | _ -> left
  and conjunction () =
    let left = negation () in
    match peek () with
    | Some ("-a" | "-and") -> ignore (take ()); And (left, conjunction ())
    | Some (")" ) | None -> left
    | Some ("-o" | "-or") -> left
    | Some _ -> And (left, conjunction ())      (* juxtaposition means -a *)
  and negation () =
    match peek () with
    | Some ("!" | "-not") -> ignore (take ()); Not (negation ())
    | _ -> primary ()
  and primary () =
    match peek () with
    | Some "(" ->
        ignore (take ());
        let e = disjunction () in
        if peek () <> Some ")" then die 1 "expected `)'";
        ignore (take ());
        e
    | Some "-name" -> ignore (take ()); Name (need "-name")
    | Some "-path" | Some "-wholename" -> ignore (take ()); Path (need "-path")
    | Some "-type" ->
        ignore (take ());
        let t = need "-type" in
        Kind (if t = "" then 'f' else t.[0])
    | Some "-newer" ->
        ignore (take ());
        let f = need "-newer" in
        (match Unix.stat f with
         | s -> Newer s.Unix.st_mtime
         | exception e -> die 1 "%s" (sys_message e))
    | Some "-empty" -> ignore (take ()); Empty
    | Some "-print" -> ignore (take ()); ctx.acted <- true; Print '\n'
    | Some "-print0" -> ignore (take ()); ctx.acted <- true; Print '\000'
    | Some "-prune" -> ignore (take ()); Prune
    | Some "-exec" | Some "-execdir" ->
        ignore (take ());
        ctx.acted <- true;
        let words = ref [] in
        let plus = ref false in
        let finished = ref false in
        while not !finished do
          if !pos >= n then die 1 "-exec: missing `;'"
          else match take () with
            | ";" -> finished := true
            | "+" -> plus := true; finished := true
            | w -> words := w :: !words
        done;
        Exec (List.rev !words, !plus)
    | Some ("-depth" | "-follow" | "-mount" | "-xdev" | "-nowarn") ->
        ignore (take ()); True
    | Some ("-maxdepth" | "-mindepth") -> ignore (take ()); ignore (need "-maxdepth"); True
    | Some w -> die 1 "unknown predicate `%s'" w
    | None -> True in
  if n = 0 then True
  else begin
    let e = disjunction () in
    if !pos <> n then die 1 "unexpected `%s'" a.(!pos);
    e
  end

let rec evaluate ctx e path (st : Unix.stats) =
  match e with
  | True -> true
  | Name pat -> Posix.Fnmatch.matches pat (Filename.basename path)
  | Path pat -> Posix.Fnmatch.matches pat path
  | Kind c -> kind_char st.Unix.st_kind = c
  | Newer t -> st.Unix.st_mtime > t
  | Empty ->
      (match st.Unix.st_kind with
       | Unix.S_DIR -> (match Sys.readdir path with a -> Array.length a = 0 | exception _ -> false)
       | _ -> st.Unix.st_size = 0)
  | Print term -> emit path; emit (String.make 1 term); true
  | Exec (words, _) ->
      let argv = List.map (fun w ->
          (* {} stands for the name found, wherever it appears *)
          if w = "{}" then path
          else if String.length w > 1 then begin
            let b = Buffer.create (String.length w) in
            let i = ref 0 in
            while !i < String.length w do
              if !i + 1 < String.length w && w.[!i] = '{' && w.[!i + 1] = '}'
              then (Buffer.add_string b path; i := !i + 2)
              else (Buffer.add_char b w.[!i]; incr i)
            done;
            Buffer.contents b
          end else w) words in
      (match argv with
       | [] -> true
       | name :: _ ->
           flush_out ();
           (match Unix.fork () with
            | 0 ->
                (try Unix.execvp name (Array.of_list argv)
                 with _ -> Printf.eprintf "find: %s: not found\n" name; exit 127)
            | pid -> (match Unix.waitpid [] pid with
                | (_, Unix.WEXITED 0) -> true
                | _ -> false)))
  | Prune -> ctx.prune <- true; true
  | Not e -> not (evaluate ctx e path st)
  | And (a, b) -> evaluate ctx a path st && evaluate ctx b path st
  | Or (a, b) -> evaluate ctx a path st || evaluate ctx b path st

let main _argv opts operands =
  let follow = Posix.Getopt.has opts "L" in
  let depth_first = Posix.Getopt.has opts "d" in
  (* the starting points come before the first operand that looks like
     part of the expression *)
  let rec split starts = function
    | w :: rest when w <> "" && (w.[0] = '-' || w = "!" || w = "(") -> (List.rev starts, w :: rest)
    | w :: rest -> split (w :: starts) rest
    | [] -> (List.rev starts, []) in
  let (starts, expression) = split [] operands in
  let starts = if starts = [] then [ "." ] else starts in
  let ctx = { prune = false; acted = false } in
  let e = parse ctx expression in
  let e = if ctx.acted then e else And (e, Print '\n') in
  let status = ref 0 in
  let rec walk path =
    match (if follow then Unix.stat path else Unix.lstat path) with
    | exception err -> warn "%s" (sys_message err); status := 1
    | st ->
        let visit () =
          ctx.prune <- false;
          ignore (evaluate ctx e path st) in
        if depth_first then begin
          if st.Unix.st_kind = Unix.S_DIR then descend path;
          visit ()
        end else begin
          visit ();
          if st.Unix.st_kind = Unix.S_DIR && not ctx.prune then descend path
        end
  and descend path =
    match Sys.readdir path with
    | items ->
        Array.sort compare items;
        Array.iter (fun item ->
            walk (if path = "/" then "/" ^ item else path ^ "/" ^ item)) items
    | exception err -> warn "%s" (sys_message err); status := 1 in
  List.iter walk starts;
  flush_out ();
  !status
