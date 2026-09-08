(* env, which, uname, hostname, ls, expr, sleep, id and date (IEEE Std
   1003.1-2017, XCU).

   These are the utilities configure calls to learn about the machine it
   is running on, plus expr, which is a small language of its own: an
   arithmetic and string expression whose operators are separate
   arguments, so that the shell's own quoting decides where each one
   begins. *)

open Util

(* ---------- env ---------- *)

let env argv opts operands =
  ignore argv;
  let ignore_env = Posix.Getopt.has opts "i" in
  let unset = Posix.Getopt.all opts "u" in
  (* the assignments come first, then the command *)
  let rec split assigns = function
    | a :: rest when String.contains a '=' ->
        let k = String.index a '=' in
        split ((String.sub a 0 k, String.sub a (k + 1) (String.length a - k - 1)) :: assigns) rest
    | rest -> (List.rev assigns, rest) in
  let (assigns, command) = split [] operands in
  let base =
    if ignore_env then []
    else
      List.filter_map (fun kv ->
          match String.index_opt kv '=' with
          | Some k ->
              let name = String.sub kv 0 k in
              if List.mem name unset then None
              else Some (name, String.sub kv (k + 1) (String.length kv - k - 1))
          | None -> None)
        (Array.to_list (Unix.environment ())) in
  let merged =
    List.fold_left (fun acc (n, v) -> (n, v) :: List.remove_assoc n acc) base assigns in
  match command with
  | [] ->
      List.iter (fun (n, v) -> emit_line (n ^ "=" ^ v)) (List.sort compare merged);
      flush_out ();
      0
  | name :: _ ->
      flush_out ();
      let environment = Array.of_list (List.map (fun (n, v) -> n ^ "=" ^ v) merged) in
      let path = if String.contains name '/' then Some name
        else List.find_map (fun d ->
            let p = Filename.concat d name in
            if Sys.file_exists p then Some p else None)
            (String.split_on_char ':'
               (match List.assoc_opt "PATH" merged with Some p -> p | None -> "/usr/bin:/bin")) in
      (match path with
       | None -> warn "%s: No such file or directory" name; 127
       | Some p ->
           (match Unix.execve p (Array.of_list command) environment with
            | _ -> 126
            | exception e -> warn "%s" (sys_message e); 126))

(* ---------- which ---------- *)

let which _argv opts operands =
  let all = Posix.Getopt.has opts "a" in
  let path = match Sys.getenv_opt "PATH" with Some p -> p | None -> "/usr/bin:/bin" in
  let dirs = String.split_on_char ':' path in
  let status = ref 0 in
  List.iter (fun name ->
      let hits = List.filter_map (fun d ->
          let p = if d = "" then name else Filename.concat d name in
          match Unix.access p [ Unix.X_OK ] with
          | () -> if Sys.is_directory p then None else Some p
          | exception _ -> None) dirs in
      match hits with
      | [] -> status := 1
      | first :: _ -> if all then List.iter emit_line hits else emit_line first)
    operands;
  flush_out ();
  !status

(* ---------- uname ---------- *)

let uname _argv opts operands =
  ignore operands;
  (* The standard library has no uname, so the kernel's own names are
     read from where this system publishes them.  The machine name is the
     one place a utility states the target: occ is an x86-64 compiler and
     its assembler and linker emit nothing else. *)
  let read f = match In_channel.with_open_text f In_channel.input_all with
    | s -> String.trim s
    | exception _ -> "unknown" in
  let fields = [
    "s", read "/proc/sys/kernel/ostype";
    "n", Unix.gethostname ();
    "r", read "/proc/sys/kernel/osrelease";
    "v", read "/proc/sys/kernel/version";
    "m", "x86_64";
    "p", "unknown";
    "i", "unknown";
    "o", "GNU/Linux";
  ] in
  let has c = Posix.Getopt.has opts c in
  let all = has "a" in
  (* -a gives everything the standard names, but not the two that GNU
     itself omits unless they are known *)
  let order = if all then [ "s"; "n"; "r"; "v"; "m"; "p"; "i"; "o" ]
    else List.filter has [ "s"; "n"; "r"; "v"; "m"; "p"; "i"; "o" ] in
  let values = List.filter_map (fun k ->
      match List.assoc_opt k fields with
      | Some v when not (all && v = "unknown") -> Some v
      | _ -> None) order in
  emit_line (if values = [] then List.assoc "s" fields else String.concat " " values);
  flush_out ();
  0

let hostname _argv _opts _operands =
  emit_line (Unix.gethostname ());
  flush_out ();
  0

(* ---------- ls ---------- *)

let ls _argv opts operands =
  let long = Posix.Getopt.has opts "l" in
  let all = Posix.Getopt.has opts "a" in
  let dir_itself = Posix.Getopt.has opts "d" in
  let inode = Posix.Getopt.has opts "i" in
  let follow = Posix.Getopt.has opts "L" in
  let names = match operands with [] -> [ "." ] | l -> l in
  let status = ref 0 in
  let kind_char k = match k with
    | Unix.S_DIR -> 'd' | Unix.S_LNK -> 'l' | Unix.S_CHR -> 'c'
    | Unix.S_BLK -> 'b' | Unix.S_FIFO -> 'p' | Unix.S_SOCK -> 's' | Unix.S_REG -> '-' in
  let perm_string m =
    let b = Buffer.create 9 in
    List.iter (fun (r, w, x) ->
        Buffer.add_char b (if m land r <> 0 then 'r' else '-');
        Buffer.add_char b (if m land w <> 0 then 'w' else '-');
        Buffer.add_char b (if m land x <> 0 then 'x' else '-'))
      [ (0o400, 0o200, 0o100); (0o040, 0o020, 0o010); (0o004, 0o002, 0o001) ];
    Buffer.contents b in
  let show path display =
    match (if follow then Unix.stat path else Unix.lstat path) with
    | exception e -> warn "%s" (sys_message e); status := 1
    | st ->
        let line =
          if long then
            Printf.sprintf "%c%s %d %d %d %d %s"
              (kind_char st.Unix.st_kind) (perm_string st.Unix.st_perm)
              st.Unix.st_nlink st.Unix.st_uid st.Unix.st_gid st.Unix.st_size display
          else display in
        let line = if inode then Printf.sprintf "%d %s" st.Unix.st_ino line else line in
        emit_line line in
  List.iter (fun name ->
      if dir_itself || not (match Unix.stat name with
          | { Unix.st_kind = Unix.S_DIR; _ } -> true | _ -> false | exception _ -> false)
      then show name name
      else begin
        match Sys.readdir name with
        | items ->
            let items = Array.to_list items in
            let items = if all then "." :: ".." :: items
              else List.filter (fun e -> e = "" || e.[0] <> '.') items in
            List.iter (fun e -> show (Filename.concat name e) e) (List.sort compare items)
        | exception e -> warn "%s" (sys_message e); status := 1
      end)
    names;
  flush_out ();
  !status

(* ---------- expr ---------- *)

(* The grammar of XCU expr, whose precedence from lowest to highest is
   |, &, the comparisons, + and -, * / %, then the string operators.  A
   value is a string that may also read as an integer. *)
exception Expr_error of string

let expr_num s =
  match int_of_string_opt (String.trim s) with
  | Some v -> v
  | None -> raise (Expr_error (Printf.sprintf "non-integer argument"))

let truthy s = s <> "" && s <> "0"

let expr _argv _opts operands =
  let a = Array.of_list operands in
  let n = Array.length a in
  let pos = ref 0 in
  let peek () = if !pos < n then Some a.(!pos) else None in
  let eat op = if peek () = Some op then (incr pos; true) else false in
  let rec disjunction () =
    let left = ref (conjunction ()) in
    while peek () = Some "|" do
      incr pos;
      let right = conjunction () in
      if not (truthy !left) then left := (if truthy right then right else "0")
    done;
    !left
  and conjunction () =
    let left = ref (comparison ()) in
    while peek () = Some "&" do
      incr pos;
      let right = comparison () in
      if not (truthy !left) || not (truthy right) then left := "0"
    done;
    !left
  and comparison () =
    let left = ref (additive ()) in
    let rec go () =
      match peek () with
      | Some (("=" | "!=" | "<" | "<=" | ">" | ">=") as op) ->
          incr pos;
          let right = additive () in
          let both_numeric =
            int_of_string_opt (String.trim !left) <> None
            && int_of_string_opt (String.trim right) <> None in
          let c = if both_numeric then compare (expr_num !left) (expr_num right)
            else compare !left right in
          let v = match op with
            | "=" -> c = 0 | "!=" -> c <> 0 | "<" -> c < 0
            | "<=" -> c <= 0 | ">" -> c > 0 | _ -> c >= 0 in
          left := (if v then "1" else "0");
          go ()
      | _ -> () in
    go ();
    !left
  and additive () =
    let left = ref (multiplicative ()) in
    let rec go () =
      match peek () with
      | Some "+" -> incr pos; let r = multiplicative () in
          left := string_of_int (expr_num !left + expr_num r); go ()
      | Some "-" -> incr pos; let r = multiplicative () in
          left := string_of_int (expr_num !left - expr_num r); go ()
      | _ -> () in
    go ();
    !left
  and multiplicative () =
    let left = ref (matching ()) in
    let rec go () =
      match peek () with
      | Some "*" -> incr pos; let r = matching () in
          left := string_of_int (expr_num !left * expr_num r); go ()
      | Some "/" -> incr pos; let r = matching () in
          let d = expr_num r in
          if d = 0 then raise (Expr_error "division by zero");
          left := string_of_int (expr_num !left / d); go ()
      | Some "%" -> incr pos; let r = matching () in
          let d = expr_num r in
          if d = 0 then raise (Expr_error "division by zero");
          left := string_of_int (expr_num !left mod d); go ()
      | _ -> () in
    go ();
    !left
  (* `s : re' anchors a basic regular expression at the start of s and
     gives either the first subexpression or the length matched *)
  and matching () =
    let left = ref (primary ()) in
    while peek () = Some ":" do
      incr pos;
      let pattern = primary () in
      let re = Posix.Regex.compile ("^" ^ pattern) in
      left := (match Posix.Regex.search re !left 0 with
          | Some groups ->
              if Array.length groups > 1 then
                (match groups.(1) with
                 | (a, b) when a >= 0 -> String.sub !left a (b - a)
                 | _ -> "")
              else string_of_int (snd groups.(0) - fst groups.(0))
          | None -> if Posix.Regex.ngroups re > 0 then "" else "0")
    done;
    !left
  and primary () =
    match peek () with
    | None -> raise (Expr_error "syntax error")
    | Some "(" ->
        incr pos;
        let v = disjunction () in
        if not (eat ")") then raise (Expr_error "syntax error: expected )");
        v
    | Some v -> incr pos; v in
  match operands with
  | [] -> die 2 "usage: expr expression"
  | _ ->
      (match disjunction () with
       | v ->
           if !pos <> n then (warn "syntax error"; 2)
           else (emit_line v; flush_out (); if truthy v then 0 else 1)
       | exception Expr_error msg -> warn "%s" msg; 2
       | exception Posix.Regex.Error msg -> warn "%s" msg; 2)

(* ---------- sleep ---------- *)

let sleep _argv _opts operands =
  match operands with
  | [] -> die 1 "usage: sleep seconds"
  | s :: _ ->
      (match float_of_string_opt s with
       | Some v -> (try Unix.sleepf v with _ -> ()); 0
       | None -> die 1 "%s: invalid time interval" s)
