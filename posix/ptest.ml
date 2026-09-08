(* The conditional expression of test (IEEE Std 1003.1-2017, XCU test).

   test has an unusual grammar: its operators are ordinary words, so
   whether `-f' is an operator or a string depends on what follows it.
   The standard settles the readings for one, two, three and four
   arguments before the general rules apply, and the rule that matters is
   that a binary operator wins: in `test -f = -f' the `=' is the
   operator.  The parser below looks one word past a would-be unary
   operator for that reason.

   Both the shell's built-in test and the test utility use this. *)

exception Error of string

let err fmt = Printf.ksprintf (fun s -> raise (Error s)) fmt

let stat p = match Unix.stat p with s -> Some s | exception _ -> None
let lstat p = match Unix.lstat p with s -> Some s | exception _ -> None
let permitted p mode = match Unix.access p [ mode ] with () -> true | exception _ -> false

external fd_of_int : int -> Unix.file_descr = "%identity"

let unary op arg =
  let kind k = match stat arg with Some s -> s.Unix.st_kind = k | None -> false in
  match op with
  | "-b" -> kind Unix.S_BLK
  | "-c" -> kind Unix.S_CHR
  | "-d" -> kind Unix.S_DIR
  | "-e" -> stat arg <> None
  | "-f" -> kind Unix.S_REG
  | "-g" -> (match stat arg with Some s -> s.Unix.st_perm land 0o2000 <> 0 | None -> false)
  | "-h" | "-L" -> (match lstat arg with Some s -> s.Unix.st_kind = Unix.S_LNK | None -> false)
  | "-k" -> (match stat arg with Some s -> s.Unix.st_perm land 0o1000 <> 0 | None -> false)
  | "-p" -> kind Unix.S_FIFO
  | "-r" -> permitted arg Unix.R_OK
  | "-S" -> kind Unix.S_SOCK
  | "-s" -> (match stat arg with Some s -> s.Unix.st_size > 0 | None -> false)
  | "-t" ->
      (match int_of_string_opt arg with
       | Some fd -> (match Unix.isatty (fd_of_int fd) with b -> b | exception _ -> false)
       | None -> false)
  | "-u" -> (match stat arg with Some s -> s.Unix.st_perm land 0o4000 <> 0 | None -> false)
  | "-w" -> permitted arg Unix.W_OK
  | "-x" -> permitted arg Unix.X_OK
  | "-n" -> arg <> ""
  | "-z" -> arg = ""
  | _ -> err "%s: unknown operator" op

let unary_ops =
  [ "-b"; "-c"; "-d"; "-e"; "-f"; "-g"; "-h"; "-k"; "-L"; "-p"; "-r"; "-S";
    "-s"; "-t"; "-u"; "-w"; "-x"; "-n"; "-z" ]

let binary_ops =
  [ "="; "=="; "!="; "<"; ">"; "-eq"; "-ne"; "-lt"; "-le"; "-gt"; "-ge";
    "-nt"; "-ot"; "-ef" ]

let integer s =
  match int_of_string_opt (String.trim s) with
  | Some v -> v
  | None -> err "%s: integer expression expected" s

let binary a op b =
  let compare_int f = f (integer a) (integer b) in
  match op with
  | "=" | "==" -> a = b
  | "!=" -> a <> b
  | "<" -> a < b
  | ">" -> a > b
  | "-eq" -> compare_int ( = )
  | "-ne" -> compare_int ( <> )
  | "-lt" -> compare_int ( < )
  | "-le" -> compare_int ( <= )
  | "-gt" -> compare_int ( > )
  | "-ge" -> compare_int ( >= )
  | "-nt" ->
      (match stat a, stat b with
       | Some x, Some y -> x.Unix.st_mtime > y.Unix.st_mtime
       | Some _, None -> true
       | _ -> false)
  | "-ot" ->
      (match stat a, stat b with
       | Some x, Some y -> x.Unix.st_mtime < y.Unix.st_mtime
       | None, Some _ -> true
       | _ -> false)
  | "-ef" ->
      (match stat a, stat b with
       | Some x, Some y -> x.Unix.st_dev = y.Unix.st_dev && x.Unix.st_ino = y.Unix.st_ino
       | _ -> false)
  | _ -> err "%s: unknown operator" op

let evaluate args =
  let a = Array.of_list args in
  let n = Array.length a in
  let pos = ref 0 in
  let peek () = if !pos < n then Some a.(!pos) else None in
  let take () = let v = a.(!pos) in incr pos; v in
  let rec disjunction () =
    let left = conjunction () in
    if peek () = Some "-o" then (ignore (take ()); let right = disjunction () in left || right)
    else left
  and conjunction () =
    let left = negation () in
    if peek () = Some "-a" then (ignore (take ()); let right = conjunction () in left && right)
    else left
  and negation () =
    if peek () = Some "!" then (ignore (take ()); not (negation ()))
    else primary ()
  and primary () =
    match peek () with
    | None -> err "argument expected"
    | Some "(" ->
        ignore (take ());
        let v = disjunction () in
        if peek () <> Some ")" then err "`)' expected";
        ignore (take ());
        v
    | Some op when List.mem op unary_ops && !pos + 1 < n
                   && not (!pos + 2 < n && List.mem a.(!pos + 1) binary_ops) ->
        ignore (take ()); unary op (take ())
    | Some _ ->
        let first = take () in
        (match peek () with
         | Some op when List.mem op binary_ops -> ignore (take ()); binary first op (take ())
         | _ -> first <> "") in
  match args with
  | [] -> false
  | _ ->
      let v = disjunction () in
      if !pos <> n then err "%s: unexpected operator" a.(!pos);
      v
