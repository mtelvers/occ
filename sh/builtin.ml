(* The built-in utilities (IEEE Std 1003.1-2017, XCU 2.14 and the utility
   descriptions).

   Two groups, and the difference matters.  A *special* built-in (2.14)
   runs in the current shell whatever else is going on, an error in it
   ends a non-interactive shell, and variable assignments on its command
   line stay in effect afterwards.  A *regular* built-in is a utility
   that happens not to need a new process.

   `eval', `.' and `command' need to run whole programs, which is the
   executor's work, so they reach it through the hooks below; Exec fills
   them in. *)

open State

let eval_hook : (State.t -> string -> int) ref =
  ref (fun _ _ -> failwith "Builtin.eval_hook: not set")

(* run a command name with arguments, without looking at functions *)
let exec_hook : (State.t -> string list -> int) ref =
  ref (fun _ _ -> failwith "Builtin.exec_hook: not set")

(* look a command name up on PATH *)
let search_hook : (State.t -> string -> string option) ref =
  ref (fun _ _ -> None)

let out s = print_string s
let outl s = print_string s; print_char '\n'

let error name fmt =
  Printf.ksprintf (fun s -> Printf.eprintf "%s: %s\n" name s; flush stderr) fmt

(* ---------- echo ---------- *)

(* The escapes of XCU echo, which this shell's echo always interprets, as
   /bin/sh does; the only option it knows is -n. *)
let echo_escapes s =
  let b = Buffer.create (String.length s) in
  let n = String.length s in
  let stop = ref false in
  let i = ref 0 in
  while not !stop && !i < n do
    if s.[!i] = '\\' && !i + 1 < n then begin
      (match s.[!i + 1] with
       | 'a' -> Buffer.add_char b '\007'; i := !i + 2
       | 'b' -> Buffer.add_char b '\b'; i := !i + 2
       | 'c' -> stop := true
       | 'f' -> Buffer.add_char b '\012'; i := !i + 2
       | 'n' -> Buffer.add_char b '\n'; i := !i + 2
       | 'r' -> Buffer.add_char b '\r'; i := !i + 2
       | 't' -> Buffer.add_char b '\t'; i := !i + 2
       | 'v' -> Buffer.add_char b '\011'; i := !i + 2
       | '\\' -> Buffer.add_char b '\\'; i := !i + 2
       | '0' ->
           let k = ref (!i + 2) and v = ref 0 and digits = ref 0 in
           while !digits < 3 && !k < n && s.[!k] >= '0' && s.[!k] <= '7' do
             v := !v * 8 + (Char.code s.[!k] - 48); incr k; incr digits
           done;
           Buffer.add_char b (Char.chr (!v land 255)); i := !k
       | c -> Buffer.add_char b '\\'; Buffer.add_char b c; i := !i + 2)
    end else (Buffer.add_char b s.[!i]; incr i)
  done;
  (Buffer.contents b, !stop)

let echo _st args =
  let newline, args = match args with
    | "-n" :: rest -> false, rest
    | _ -> true, args in
  let cut = ref false in
  List.iteri (fun i a ->
      if not !cut then begin
        if i > 0 then out " ";
        let (text, stop) = echo_escapes a in
        out text;
        if stop then cut := true
      end) args;
  if newline && not !cut then out "\n";
  flush stdout;
  0

(* ---------- printf ---------- *)

let printf_escapes s =
  let b = Buffer.create (String.length s) in
  let n = String.length s in
  let i = ref 0 in
  while !i < n do
    if s.[!i] = '\\' && !i + 1 < n then
      (match s.[!i + 1] with
       | 'a' -> Buffer.add_char b '\007'; i := !i + 2
       | 'b' -> Buffer.add_char b '\b'; i := !i + 2
       | 'f' -> Buffer.add_char b '\012'; i := !i + 2
       | 'n' -> Buffer.add_char b '\n'; i := !i + 2
       | 'r' -> Buffer.add_char b '\r'; i := !i + 2
       | 't' -> Buffer.add_char b '\t'; i := !i + 2
       | 'v' -> Buffer.add_char b '\011'; i := !i + 2
       | '\\' -> Buffer.add_char b '\\'; i := !i + 2
       | c when c >= '0' && c <= '7' ->
           let k = ref (!i + 1) and v = ref 0 and digits = ref 0 in
           while !digits < 4 && !k < n && s.[!k] >= '0' && s.[!k] <= '7' do
             v := !v * 8 + (Char.code s.[!k] - 48); incr k; incr digits
           done;
           Buffer.add_char b (Char.chr (!v land 255)); i := !k
       | c -> Buffer.add_char b '\\'; Buffer.add_char b c; i := !i + 2)
    else (Buffer.add_char b s.[!i]; incr i)
  done;
  Buffer.contents b

(* a numeric argument: a C integer constant, or a character preceded by a
   quote, whose value is that character (XCU printf) *)
let printf_number arg =
  if arg = "" then 0
  else if arg.[0] = '\'' || arg.[0] = '"' then
    (if String.length arg > 1 then Char.code arg.[1] else 0)
  else
    let neg = arg.[0] = '-' in
    let body = if neg || arg.[0] = '+' then String.sub arg 1 (String.length arg - 1) else arg in
    let v =
      match int_of_string_opt body with
      | Some v -> v
      | None ->
          (* the leading digits, as the other printf does *)
          let k = ref 0 in
          while !k < String.length body && Posix.Regex.is_digit body.[!k] do incr k done;
          (match int_of_string_opt (String.sub body 0 !k) with Some v -> v | None -> 0) in
    if neg then -v else v

let pad ~left ~zero ~width s =
  let n = String.length s in
  if n >= width then s
  else if left then s ^ String.make (width - n) ' '
  else if zero then
    (* the sign, if any, stays in front of the zeros *)
    if n > 0 && (s.[0] = '-' || s.[0] = '+')
    then String.make 1 s.[0] ^ String.make (width - n) '0' ^ String.sub s 1 (n - 1)
    else String.make (width - n) '0' ^ s
  else String.make (width - n) ' ' ^ s

let to_unsigned v = if v >= 0 then Printf.sprintf "%u" v else Printf.sprintf "%u" v

let printf_builtin _st args =
  match args with
  | [] -> error "printf" "usage: printf format [argument...]"; 2
  | fmt :: rest ->
      let b = Buffer.create 256 in
      let queue = ref rest in
      let used = ref false in
      let next () =
        match !queue with
        | [] -> ""
        | a :: t -> queue := t; used := true; a in
      let n = String.length fmt in
      let status = ref 0 in
      let rec pass () =
        let i = ref 0 in
        while !i < n do
          if fmt.[!i] = '\\' then begin
            (* one escape at a time, so that \c can stop the output *)
            let k = ref (!i + 1) in
            while !k < n && fmt.[!k] <> '\\' && fmt.[!k] <> '%' do incr k done;
            Buffer.add_string b (printf_escapes (String.sub fmt !i (!k - !i)));
            i := !k
          end
          else if fmt.[!i] = '%' then begin
            if !i + 1 < n && fmt.[!i + 1] = '%' then (Buffer.add_char b '%'; i := !i + 2)
            else begin
              let k = ref (!i + 1) in
              let left = ref false and zero = ref false and plus = ref false
              and space = ref false and alt = ref false in
              let flag = ref true in
              while !flag && !k < n do
                (match fmt.[!k] with
                 | '-' -> left := true | '0' -> zero := true | '+' -> plus := true
                 | ' ' -> space := true | '#' -> alt := true
                 | _ -> flag := false);
                if !flag then incr k
              done;
              let number () =
                if !k < n && fmt.[!k] = '*' then (incr k; printf_number (next ()))
                else begin
                  let start = !k in
                  while !k < n && Posix.Regex.is_digit fmt.[!k] do incr k done;
                  if !k = start then -1 else int_of_string (String.sub fmt start (!k - start))
                end in
              let width = number () in
              let prec =
                if !k < n && fmt.[!k] = '.' then (incr k; let p = number () in if p < 0 then 0 else p)
                else -1 in
              (* length modifiers are accepted and have no effect *)
              while !k < n && (match fmt.[!k] with 'l' | 'h' | 'L' | 'q' | 'j' | 'z' | 't' -> true | _ -> false) do incr k done;
              if !k >= n then (error "printf" "%s: missing conversion character" fmt; status := 1; i := n)
              else begin
                let conv = fmt.[!k] in
                let text =
                  match conv with
                  | 'd' | 'i' ->
                      let v = printf_number (next ()) in
                      let s = string_of_int v in
                      let s = if v >= 0 && !plus then "+" ^ s else if v >= 0 && !space then " " ^ s else s in
                      s
                  | 'u' -> to_unsigned (printf_number (next ()))
                  | 'o' ->
                      let s = Printf.sprintf "%o" (printf_number (next ())) in
                      if !alt && (s = "" || s.[0] <> '0') then "0" ^ s else s
                  | 'x' ->
                      let s = Printf.sprintf "%x" (printf_number (next ())) in
                      if !alt then "0x" ^ s else s
                  | 'X' ->
                      let s = Printf.sprintf "%X" (printf_number (next ())) in
                      if !alt then "0X" ^ s else s
                  | 'c' -> let a = next () in if a = "" then "" else String.make 1 a.[0]
                  | 's' ->
                      let a = next () in
                      if prec >= 0 && prec < String.length a then String.sub a 0 prec else a
                  | 'b' ->
                      let a = printf_escapes (next ()) in
                      if prec >= 0 && prec < String.length a then String.sub a 0 prec else a
                  | 'e' | 'E' | 'f' | 'g' | 'G' ->
                      let a = next () in
                      let v = match float_of_string_opt a with Some v -> v | None -> 0.0 in
                      let p = if prec < 0 then 6 else prec in
                      (match conv with
                       | 'f' -> Printf.sprintf "%.*f" p v
                       | 'e' -> Printf.sprintf "%.*e" p v
                       | 'E' -> String.uppercase_ascii (Printf.sprintf "%.*e" p v)
                       | 'g' -> Printf.sprintf "%.*g" p v
                       | _ -> String.uppercase_ascii (Printf.sprintf "%.*g" p v))
                  | c -> error "printf" "%%%c: invalid conversion" c; status := 1; "" in
                let text =
                  if conv = 'd' || conv = 'i' || conv = 'u' || conv = 'o' || conv = 'x' || conv = 'X'
                  then (if prec >= 0 && String.length text < prec
                        then String.make (prec - String.length text) '0' ^ text else text)
                  else text in
                Buffer.add_string b (pad ~left:!left ~zero:!zero ~width:(max width 0) text);
                i := !k + 1
              end
            end
          end
          else (Buffer.add_char b fmt.[!i]; incr i)
        done;
        (* the format is used again while arguments remain (XCU printf) *)
        if !queue <> [] && !used then (used := false; pass ()) in
      pass ();
      out (Buffer.contents b);
      flush stdout;
      !status

(* ---------- test ---------- *)

(* The conditional expression of XCU test.  Its grammar is unusual: the
   operators are words, so the parse depends on how many arguments there
   are, and the standard fixes the reading for one, two, three and four
   arguments before the general rules apply. *)
exception Test_error of string

let file_stat p = match Unix.stat p with s -> Some s | exception _ -> None
let file_lstat p = match Unix.lstat p with s -> Some s | exception _ -> None

let unary op arg =
  let stat_is kind = match file_stat arg with Some s -> s.Unix.st_kind = kind | None -> false in
  match op with
  | "-b" -> stat_is Unix.S_BLK
  | "-c" -> stat_is Unix.S_CHR
  | "-d" -> stat_is Unix.S_DIR
  | "-e" -> file_stat arg <> None
  | "-f" -> stat_is Unix.S_REG
  | "-g" -> (match file_stat arg with Some s -> s.Unix.st_perm land 0o2000 <> 0 | None -> false)
  | "-h" | "-L" -> (match file_lstat arg with Some s -> s.Unix.st_kind = Unix.S_LNK | None -> false)
  | "-p" -> stat_is Unix.S_FIFO
  | "-r" -> (match Unix.access arg [ Unix.R_OK ] with () -> true | exception _ -> false)
  | "-S" -> stat_is Unix.S_SOCK
  | "-s" -> (match file_stat arg with Some s -> s.Unix.st_size > 0 | None -> false)
  | "-t" -> (match int_of_string_opt arg with
      | Some fd -> (try Unix.isatty (Obj.magic fd : Unix.file_descr) with _ -> false)
      | None -> false)
  | "-u" -> (match file_stat arg with Some s -> s.Unix.st_perm land 0o4000 <> 0 | None -> false)
  | "-w" -> (match Unix.access arg [ Unix.W_OK ] with () -> true | exception _ -> false)
  | "-x" -> (match Unix.access arg [ Unix.X_OK ] with () -> true | exception _ -> false)
  | "-n" -> arg <> ""
  | "-z" -> arg = ""
  | _ -> raise (Test_error (op ^ ": unknown operator"))

let is_unary op =
  List.mem op [ "-b"; "-c"; "-d"; "-e"; "-f"; "-g"; "-h"; "-L"; "-p"; "-r";
                "-S"; "-s"; "-t"; "-u"; "-w"; "-x"; "-n"; "-z" ]

let integer name s =
  match int_of_string_opt (String.trim s) with
  | Some v -> v
  | None -> raise (Test_error (Printf.sprintf "%s: %s: integer expected" name s))

let binary a op b =
  match op with
  | "=" | "==" -> a = b
  | "!=" -> a <> b
  | "<" -> a < b
  | ">" -> a > b
  | "-eq" -> integer op a = integer op b
  | "-ne" -> integer op a <> integer op b
  | "-lt" -> integer op a < integer op b
  | "-le" -> integer op a <= integer op b
  | "-gt" -> integer op a > integer op b
  | "-ge" -> integer op a >= integer op b
  | "-nt" ->
      (match file_stat a, file_stat b with
       | Some x, Some y -> x.Unix.st_mtime > y.Unix.st_mtime
       | Some _, None -> true
       | _ -> false)
  | "-ot" ->
      (match file_stat a, file_stat b with
       | Some x, Some y -> x.Unix.st_mtime < y.Unix.st_mtime
       | None, Some _ -> true
       | _ -> false)
  | "-ef" ->
      (match file_stat a, file_stat b with
       | Some x, Some y -> x.Unix.st_dev = y.Unix.st_dev && x.Unix.st_ino = y.Unix.st_ino
       | _ -> false)
  | _ -> raise (Test_error (op ^ ": unknown operator"))

let is_binary op =
  List.mem op [ "="; "=="; "!="; "<"; ">"; "-eq"; "-ne"; "-lt"; "-le"; "-gt";
                "-ge"; "-nt"; "-ot"; "-ef" ]

let test_expr args =
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
    | None -> raise (Test_error "argument expected")
    | Some "(" ->
        ignore (take ());
        let v = disjunction () in
        if peek () <> Some ")" then raise (Test_error "`)' expected");
        ignore (take ());
        v
    | Some op when is_unary op && !pos + 1 < n
                   && not (!pos + 2 < n && is_binary a.(!pos + 1)) ->
        ignore (take ()); unary op (take ())
    | Some _ ->
        let first = take () in
        (match peek () with
         | Some op when is_binary op -> ignore (take ()); binary first op (take ())
         | _ -> first <> "") in
  (* the fixed readings of one to four arguments come out of the grammar
     above except for the two-argument unary case, which is why the unary
     test looks ahead for a binary operator *)
  let v = disjunction () in
  if !pos <> n then raise (Test_error (a.(!pos) ^ ": unexpected operator"));
  v

let test name args =
  let args =
    if name = "[" then
      match List.rev args with
      | "]" :: rest -> List.rev rest
      | _ -> error "[" "missing `]'"; raise (Test_error "missing `]'")
    else args in
  match args with
  | [] -> 1
  | _ ->
      (match test_expr args with
       | true -> 0
       | false -> 1
       | exception Test_error msg -> error name "%s" msg; 2)

(* ---------- the environment ---------- *)

let cd st args =
  let target =
    match args with
    | [] -> (match State.get st "HOME" with
        | Some h when h <> "" -> Some h
        | _ -> error "cd" "HOME not set"; None)
    | "-" :: _ -> (match State.get st "OLDPWD" with
        | Some p when p <> "" -> outl p; Some p
        | _ -> error "cd" "OLDPWD not set"; None)
    | d :: _ -> Some d in
  match target with
  | None -> 1
  | Some dir ->
      (* CDPATH is searched for a relative name that is not . or .. *)
      let candidates =
        if Filename.is_relative dir && dir <> "" && dir.[0] <> '.' then
          match State.get st "CDPATH" with
          | Some p when p <> "" ->
              List.map (fun d -> if d = "" then dir else Filename.concat d dir)
                (String.split_on_char ':' p) @ [ dir ]
          | _ -> [ dir ]
        else [ dir ] in
      let rec attempt = function
        | [] ->
            error "cd" "%s: %s" dir (Unix.error_message Unix.ENOENT); 1
        | d :: rest ->
            (match Unix.chdir d with
             | () ->
                 let old = State.get_or st "PWD" "" in
                 if old <> "" then State.set st "OLDPWD" old;
                 State.set st "PWD" (Unix.getcwd ());
                 if rest <> [] && d <> dir then outl (Unix.getcwd ());
                 0
             | exception Unix.Unix_error (e, _, _) ->
                 if rest = [] then (error "cd" "%s: %s" d (Unix.error_message e); 1)
                 else attempt rest) in
      attempt candidates

let pwd st args =
  ignore args;
  (* the logical name, which keeps the symbolic links the user walked
     through, unless it no longer names this directory *)
  let logical = State.get_or st "PWD" "" in
  let physical = match Unix.getcwd () with d -> d | exception _ -> "" in
  let same =
    logical <> "" &&
    (match Unix.stat logical, Unix.stat physical with
     | a, b -> a.Unix.st_dev = b.Unix.st_dev && a.Unix.st_ino = b.Unix.st_ino
     | exception _ -> false) in
  outl (if same then logical else physical);
  flush stdout;
  0

let export_or_readonly st which args =
  let mark = if which = "export" then State.export else State.readonly in
  let args = List.filter (fun a -> a <> "-p") args in
  if args = [] then begin
    (* print what is marked, in a form that can be read back *)
    let names = ref [] in
    Hashtbl.iter (fun name v ->
        let marked = if which = "export" then v.exported else v.readonly in
        if marked then names := (name, v.value) :: !names) st.vars;
    List.iter (fun (name, value) ->
        match value with
        | Some v -> outl (Printf.sprintf "%s %s=%s" which name (Filename.quote v))
        | None -> outl (Printf.sprintf "%s %s" which name))
      (List.sort compare !names);
    flush stdout;
    0
  end else begin
    let status = ref 0 in
    List.iter (fun a ->
        match String.index_opt a '=' with
        | Some i ->
            let name = String.sub a 0 i in
            (match State.set st name (String.sub a (i + 1) (String.length a - i - 1)) with
             | () -> mark st name
             | exception State.Error msg -> error which "%s" msg; status := 1)
        | None -> (match mark st a with
            | () -> ()
            | exception State.Error msg -> error which "%s" msg; status := 1)) args;
    !status
  end

let unset_builtin st args =
  let functions = List.mem "-f" args and vars_only = List.mem "-v" args in
  let names = List.filter (fun a -> a <> "-f" && a <> "-v") args in
  let status = ref 0 in
  List.iter (fun name ->
      if functions && not vars_only then st.funcs <- List.remove_assoc name st.funcs
      else begin
        (match State.unset st name with
         | () -> ()
         | exception State.Error msg -> error "unset" "%s" msg; status := 1);
        if not vars_only then st.funcs <- List.remove_assoc name st.funcs
      end) names;
  !status

let shift st args =
  let k = match args with [] -> 1 | a :: _ ->
    (match int_of_string_opt a with Some v -> v | None -> error "shift" "%s: bad number" a; -1) in
  if k < 0 then 1
  else if k > List.length st.params then (error "shift" "can't shift that many"; 1)
  else begin
    let rec drop n l = if n = 0 then l else match l with [] -> [] | _ :: t -> drop (n - 1) t in
    st.params <- drop k st.params;
    0
  end

(* ---------- set ---------- *)

let option_letters = [
  'e', (fun o v -> o.errexit <- v); 'u', (fun o v -> o.nounset <- v);
  'x', (fun o v -> o.xtrace <- v); 'f', (fun o v -> o.noglob <- v);
  'v', (fun o v -> o.verbose <- v); 'n', (fun o v -> o.noexec <- v);
  'C', (fun o v -> o.noclobber <- v); 'a', (fun o v -> o.allexport <- v);
  'm', (fun o v -> o.monitor <- v);
]

let option_names = [
  "errexit", 'e'; "nounset", 'u'; "xtrace", 'x'; "noglob", 'f';
  "verbose", 'v'; "noexec", 'n'; "noclobber", 'C'; "allexport", 'a';
  "monitor", 'm'; "ignoreeof", ' '; "vi", ' '; "emacs", ' ';
]

let option_value o c =
  match c with
  | 'e' -> o.errexit | 'u' -> o.nounset | 'x' -> o.xtrace | 'f' -> o.noglob
  | 'v' -> o.verbose | 'n' -> o.noexec | 'C' -> o.noclobber | 'a' -> o.allexport
  | 'm' -> o.monitor | _ -> false

let set_builtin st args =
  match args with
  | [] ->
      let names = ref [] in
      Hashtbl.iter (fun name v ->
          match v.value with Some x -> names := (name, x) :: !names | None -> ()) st.vars;
      List.iter (fun (n, v) -> outl (n ^ "=" ^ v)) (List.sort compare !names);
      flush stdout;
      0
  | _ ->
      let status = ref 0 in
      let rec go = function
        | [] -> ()
        | "--" :: rest -> st.params <- rest
        | "-o" :: name :: rest when List.mem_assoc name option_names ->
            (match List.assoc name option_names with
             | ' ' -> ()
             | c -> (List.assoc c option_letters) st.opts true);
            go rest
        | "+o" :: name :: rest when List.mem_assoc name option_names ->
            (match List.assoc name option_names with
             | ' ' -> ()
             | c -> (List.assoc c option_letters) st.opts false);
            go rest
        | [ "-o" ] ->
            List.iter (fun (name, c) ->
                if c <> ' ' then
                  outl (Printf.sprintf "%-12s%s" name (if option_value st.opts c then "on" else "off")))
              option_names;
            flush stdout
        | [ "+o" ] ->
            List.iter (fun (name, c) ->
                if c <> ' ' then
                  outl (Printf.sprintf "set %co %s" (if option_value st.opts c then '-' else '+') name))
              option_names;
            flush stdout
        | a :: rest when String.length a > 1 && (a.[0] = '-' || a.[0] = '+') ->
            let on = a.[0] = '-' in
            String.iter (fun c ->
                if c <> '-' && c <> '+' then
                  match List.assoc_opt c option_letters with
                  | Some f -> f st.opts on
                  | None -> error "set" "%c: bad option" c; status := 1)
              (String.sub a 1 (String.length a - 1));
            go rest
        | rest -> st.params <- rest in
      go args;
      !status

(* ---------- read ---------- *)

let read_builtin st args =
  let raw = List.mem "-r" args in
  let names = List.filter (fun a -> a <> "-r") args in
  let names = if names = [] then [ "REPLY" ] else names in
  (* one line, honouring the backslash continuation unless -r *)
  let b = Buffer.create 128 in
  let eof = ref false in
  let finished = ref false in
  while not !finished do
    match input_char stdin with
    | '\n' -> finished := true
    | '\\' when not raw ->
        (match input_char stdin with
         | '\n' -> ()                         (* a continuation: read on *)
         | c -> Buffer.add_char b c
         | exception End_of_file -> Buffer.add_char b '\\'; eof := true; finished := true)
    | c -> Buffer.add_char b c
    | exception End_of_file -> eof := true; finished := true
  done;
  let line = Buffer.contents b in
  (* split into as many fields as there are names, the last taking the rest *)
  let ifs = State.ifs st in
  let is_ifs c = String.contains ifs c in
  let is_ws c = is_ifs c && (c = ' ' || c = '\t' || c = '\n') in
  let n = String.length line in
  let rec assign names i =
    match names with
    | [] -> ()
    | [ last ] ->
        (* the remainder, with leading and trailing IFS white space gone *)
        let a = ref i and z = ref n in
        while !a < !z && is_ws line.[!a] do incr a done;
        while !z > !a && is_ws line.[!z - 1] do decr z done;
        State.set st last (String.sub line !a (!z - !a))
    | name :: rest ->
        let a = ref i in
        while !a < n && is_ws line.[!a] do incr a done;
        let start = !a in
        while !a < n && not (is_ifs line.[!a]) do incr a done;
        State.set st name (String.sub line start (!a - start));
        (* one delimiter, with the white space around it *)
        let k = ref !a in
        while !k < n && is_ws line.[!k] do incr k done;
        if !k < n && is_ifs line.[!k] && not (is_ws line.[!k]) then incr k;
        assign rest !k in
  assign names 0;
  if !eof && line = "" then 1 else 0

(* ---------- trap ---------- *)

let trap_builtin st args =
  match args with
  | [] ->
      List.iter (fun (name, action) ->
          outl (Printf.sprintf "trap -- %s %s" (Filename.quote action) name)) st.traps;
      flush stdout;
      0
  | first :: rest when rest = [] && State.signal_name first <> None ->
      (* `trap SIG' is not defined by the standard; treat it as a query *)
      (match State.trap_of st (Option.get (State.signal_name first)) with
       | Some a -> outl a
       | None -> ());
      0
  | action :: signals ->
      let status = ref 0 in
      let reset = action = "-" in
      List.iter (fun s ->
          match State.signal_name s with
          | None -> error "trap" "%s: bad signal" s; status := 1
          | Some name ->
              if reset then State.clear_trap st name
              else State.set_trap st name action) signals;
      !status

(* ---------- others ---------- *)

let colon _st _args = 0
let true_builtin _st _args = 0
let false_builtin _st _args = 1

let eval_builtin st args =
  match args with
  | [] -> 0
  | _ -> !eval_hook st (String.concat " " args)

let dot st args =
  match args with
  | [] -> error "." "filename argument required"; 2
  | file :: params ->
      let path =
        if String.contains file '/' then Some file
        else match !search_hook st file with
          | Some p -> Some p
          | None -> if Sys.file_exists file then Some file else None in
      (match path with
       | None -> error "." "%s: not found" file; 1
       | Some p ->
           (match In_channel.with_open_bin p In_channel.input_all with
            | text ->
                (* arguments to `.' replace the positional parameters for
                   the duration of the file, as dash allows *)
                let saved = st.params in
                if params <> [] then st.params <- params;
                let restore () = if params <> [] then st.params <- saved in
                (match !eval_hook st text with
                 | s -> restore (); s
                 | exception e -> restore (); raise e)
            | exception Sys_error msg -> error "." "%s" msg; 1))

let command_builtin st args =
  let rec strip = function
    | "-p" :: rest -> strip rest
    | rest -> rest in
  match strip args with
  | "-v" :: name :: _ | "-V" :: name :: _ ->
      let verbose = List.mem "-V" args in
      if List.mem_assoc name st.funcs then
        (outl (if verbose then name ^ " is a shell function" else name); flush stdout; 0)
      else if List.mem name [ "cd"; "echo"; "printf"; "test"; "["; "read"; "pwd";
                              "true"; "false"; "command"; "type"; "umask"; "wait"; "hash" ]
              || List.mem name [ ":"; "."; "break"; "continue"; "eval"; "exec"; "exit";
                                 "export"; "readonly"; "return"; "set"; "shift"; "times";
                                 "trap"; "unset"; "local" ]
      then (outl (if verbose then name ^ " is a shell builtin" else name); flush stdout; 0)
      else (match !search_hook st name with
          | Some p -> outl (if verbose then name ^ " is " ^ p else p); flush stdout; 0
          | None -> if verbose then error "command" "%s: not found" name; 1)
  | [] -> 0
  | argv -> !exec_hook st argv

let type_builtin st args =
  let status = ref 0 in
  List.iter (fun name ->
      if List.mem_assoc name st.funcs then outl (name ^ " is a function")
      else if List.mem name [ ":"; "."; "break"; "continue"; "eval"; "exec"; "exit";
                              "export"; "readonly"; "return"; "set"; "shift"; "times";
                              "trap"; "unset"; "local"; "cd"; "echo"; "printf"; "test";
                              "["; "read"; "pwd"; "true"; "false"; "command"; "type";
                              "umask"; "wait"; "hash" ]
      then outl (name ^ " is a shell builtin")
      else match !search_hook st name with
        | Some p -> outl (name ^ " is " ^ p)
        | None -> Printf.eprintf "type: %s: not found\n" name; status := 1) args;
  flush stdout;
  !status

let umask_builtin _st args =
  match List.filter (fun a -> a <> "-S") args with
  | [] ->
      let m = Unix.umask 0 in
      ignore (Unix.umask m);
      outl (Printf.sprintf "%04o" m);
      flush stdout;
      0
  | a :: _ ->
      (match int_of_string_opt ("0o" ^ a) with
       | Some m -> ignore (Unix.umask m); 0
       | None -> error "umask" "%s: bad mask" a; 1)

let times_builtin _st _args =
  let t = Unix.times () in
  outl (Printf.sprintf "%dm%.3fs %dm%.3fs"
          (int_of_float t.Unix.tms_utime / 60) (Float.rem t.Unix.tms_utime 60.)
          (int_of_float t.Unix.tms_stime / 60) (Float.rem t.Unix.tms_stime 60.));
  outl (Printf.sprintf "%dm%.3fs %dm%.3fs"
          (int_of_float t.Unix.tms_cutime / 60) (Float.rem t.Unix.tms_cutime 60.)
          (int_of_float t.Unix.tms_cstime / 60) (Float.rem t.Unix.tms_cstime 60.));
  flush stdout;
  0

let hash_builtin _st _args = 0

let wait_builtin _st args =
  let one pid =
    match Unix.waitpid [] pid with
    | (_, Unix.WEXITED c) -> c
    | (_, Unix.WSIGNALED s) -> 128 + s
    | (_, Unix.WSTOPPED s) -> 128 + s
    | exception _ -> 127 in
  match args with
  | [] ->
      (* every child, and the status of the last one *)
      let last = ref 0 in
      let rec go () =
        match Unix.waitpid [] (-1) with
        | (_, Unix.WEXITED c) -> last := c; go ()
        | (_, Unix.WSIGNALED s) -> last := 128 + s; go ()
        | (_, Unix.WSTOPPED s) -> last := 128 + s; go ()
        | exception _ -> () in
      go ();
      !last
  | pids ->
      List.fold_left (fun _ p ->
          match int_of_string_opt (if p <> "" && p.[0] = '%' then String.sub p 1 (String.length p - 1) else p) with
          | Some pid -> one pid
          | None -> error "wait" "%s: bad process id" p; 127) 0 pids

let local_builtin st args =
  match st.locals with
  | [] -> error "local" "not in a function"; 1
  | frame :: rest ->
      let frame = ref frame in
      List.iter (fun a ->
          let name = match String.index_opt a '=' with
            | Some i -> String.sub a 0 i
            | None -> a in
          (* remember what was there so that returning can put it back *)
          if not (List.mem_assoc name !frame) then begin
            let previous = match Hashtbl.find_opt st.vars name with
              | Some v -> Some { v with value = v.value }
              | None -> None in
            frame := (name, previous) :: !frame
          end;
          match String.index_opt a '=' with
          | Some i -> State.set st name (String.sub a (i + 1) (String.length a - i - 1))
          | None -> Hashtbl.replace st.vars name { value = None; exported = false; readonly = false })
        args;
      st.locals <- !frame :: rest;
      0

(* ---------- control flow ---------- *)

let break_builtin st args =
  let k = match args with [] -> 1 | a :: _ -> (match int_of_string_opt a with Some v -> v | None -> 1) in
  if st.in_loop = 0 then 0 else raise (Break (max 1 k))

let continue_builtin st args =
  let k = match args with [] -> 1 | a :: _ -> (match int_of_string_opt a with Some v -> v | None -> 1) in
  if st.in_loop = 0 then 0 else raise (Continue (max 1 k))

let return_builtin st args =
  let k = match args with [] -> st.status | a :: _ -> (match int_of_string_opt a with Some v -> v | None -> 0) in
  raise (Return k)

let exit_builtin st args =
  let k = match args with [] -> st.status | a :: _ -> (match int_of_string_opt a with Some v -> v | None -> 0) in
  raise (Exit_shell k)

(* ---------- the tables ---------- *)

(* 2.14: an error in one of these ends a non-interactive shell, and
   assignments on its command line stay in effect *)
let special : (string * (State.t -> string list -> int)) list = [
  ":", colon;
  ".", dot;
  "break", break_builtin;
  "continue", continue_builtin;
  "eval", eval_builtin;
  "exit", exit_builtin;
  "export", (fun st args -> export_or_readonly st "export" args);
  "readonly", (fun st args -> export_or_readonly st "readonly" args);
  "return", return_builtin;
  "set", set_builtin;
  "shift", shift;
  "times", times_builtin;
  "trap", trap_builtin;
  "unset", unset_builtin;
]

let regular : (string * (State.t -> string list -> int)) list = [
  "cd", cd;
  "command", command_builtin;
  "echo", echo;
  "false", false_builtin;
  "hash", hash_builtin;
  "local", local_builtin;
  "printf", printf_builtin;
  "pwd", pwd;
  "read", read_builtin;
  "test", (fun _st args -> test "test" args);
  "[", (fun _st args -> test "[" args);
  "true", true_builtin;
  "type", type_builtin;
  "umask", umask_builtin;
  "wait", wait_builtin;
]

let find name =
  match List.assoc_opt name special with
  | Some f -> Some (`Special, f)
  | None -> (match List.assoc_opt name regular with
      | Some f -> Some (`Regular, f)
      | None -> None)
