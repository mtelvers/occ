(* The sh utility (IEEE Std 1003.1-2017, XCU sh).

   Three ways in: a command string after -c, a file named as an operand,
   or the standard input.  In each case the shell reads a program, runs
   it, and leaves with the status of the last command it ran, after the
   EXIT trap if one was set (2.11). *)

let usage () =
  prerr_endline "usage: sh [-abCefimnuvx] [-o option] [-c command | file] [argument...]";
  exit 2

let default_variables st =
  (* IFS is set afresh rather than inherited, so that a script cannot be
     changed by the environment it was started in *)
  State.set st "IFS" " \t\n";
  State.set st "PS1" "$ ";
  State.set st "PS2" "> ";
  State.set st "PS4" "+ ";
  State.set st "OPTIND" "1";
  State.set st "PPID" (string_of_int (Unix.getppid ()));
  (match Unix.getcwd () with
   | dir -> State.set st "PWD" dir; State.export st "PWD"
   | exception _ -> ());
  if State.get st "PATH" = None then State.set st "PATH" "/usr/bin:/bin"

(* the option letters of the sh utility, which are those of `set' *)
let apply_option st on letters =
  String.iter (fun c ->
      match List.assoc_opt c Builtin.option_letters with
      | Some f -> f st.State.opts on
      | None -> Printf.eprintf "sh: %c: bad option\n" c; exit 2)
    letters

let leave st status =
  (* The EXIT trap runs once, in the environment as it stands (2.11).  It
     may itself call exit, and then its status is the shell's: that is
     how configure's trap reports the error it was cleaning up after. *)
  let status =
    match State.trap_of st "EXIT" with
    | Some action when action <> "" ->
        State.clear_trap st "EXIT";
        (match Exec.run_text st action with
         | _ -> status
         | exception State.Exit_shell s -> s
         | exception State.Error msg -> Printf.eprintf "sh: %s\n" msg; status)
    | _ -> status in
  flush_all ();
  exit status

let run argv =
  let st = State.create () in
  State.import_environment st;
  default_variables st;
  let args = List.tl (Array.to_list argv) in
  let command = ref None in
  let stdin_mode = ref false in
  let rec options = function
    | [] -> []
    | "-c" :: rest ->
        (match rest with
         | text :: more -> command := Some text; more
         | [] -> Printf.eprintf "sh: -c requires an argument\n"; exit 2)
    | "-s" :: rest -> stdin_mode := true; options rest
    | "-i" :: rest -> options rest
    | "--" :: rest -> rest
    | ("-o" | "+o") :: name :: rest when List.mem_assoc name Builtin.option_names ->
        (match List.assoc name Builtin.option_names with
         | ' ' -> ()
         | c -> apply_option st true (String.make 1 c));
        options rest
    | a :: rest when String.length a > 1 && (a.[0] = '-' || a.[0] = '+') ->
        apply_option st (a.[0] = '-') (String.sub a 1 (String.length a - 1));
        options rest
    | rest -> rest in
  let operands = options args in
  let text, name, params =
    match !command with
    | Some text ->
        (match operands with
         | [] -> (Some text, "sh", [])
         | n :: rest -> (Some text, n, rest))
    | None ->
        if !stdin_mode || operands = [] then (None, "sh", operands)
        else
          match operands with
          | file :: rest ->
              (match In_channel.with_open_bin file In_channel.input_all with
               | body -> (Some body, file, rest)
               | exception Sys_error msg -> Printf.eprintf "sh: %s\n" msg; exit 127)
          | [] -> (None, "sh", []) in
  st.State.arg0 <- name;
  st.State.params <- params;
  let text = match text with
    | Some t -> t
    | None -> In_channel.input_all stdin in
  if st.State.opts.State.verbose then prerr_string text;
  if st.State.opts.State.noexec then begin
    (* -n reads the program and checks it without running it *)
    match Parse.parse text with
    | _ -> leave st 0
    | exception Parse.Error (msg, line) -> Printf.eprintf "sh: line %d: %s\n" line msg; exit 2
    | exception Lex.Error (msg, line) -> Printf.eprintf "sh: line %d: %s\n" line msg; exit 2
  end;
  let status =
    match Exec.run_text st text with
    | s -> s
    | exception State.Exit_shell s -> s
    | exception State.Error msg -> Printf.eprintf "sh: %s\n" msg; 2
    | exception State.Return s -> s
    | exception Expand.Error msg -> Printf.eprintf "sh: %s\n" msg; 2
    | exception Word.Error msg -> Printf.eprintf "sh: %s\n" msg; 2 in
  leave st status
