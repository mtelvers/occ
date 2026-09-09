(* Executing commands (IEEE Std 1003.1-2017, XCU 2.9), with redirection
   (2.7), the execution environment (2.12) and traps (2.11).

   The shape of this module follows 2.9.1's order for a simple command:
   the words are expanded, the redirections performed, and only then is
   the command name looked up -- as a function, then a special built-in,
   then a regular one, then a file on PATH.  Everything that needs a
   separate process (an external command, a subshell, one element of a
   pipeline, a command substitution) is a fork, because 2.12 defines a
   subshell as a copy of this environment that the parent does not see
   change, and a fork is exactly that.

   Redirections applied for a built-in have to be undone afterwards, so
   each one saves the descriptor it displaces; `exec' with no command is
   the case where they are deliberately not undone. *)

open Ast
open State

(* Unix.file_descr is a descriptor number, but the library gives no way to
   name one that it did not open, and `exec 5>>log' needs descriptor 5 by
   name.  These two conversions are the only place that is assumed. *)
external fd_of_int : int -> Unix.file_descr = "%identity"
external int_of_fd : Unix.file_descr -> int = "%identity"

exception Exit_loop

(* While a condition is being run -- of `if', `while', `until', the
   left-hand side of `&&' -- the -e option does not end the shell
   (2.8.1). *)
let condition_depth = ref 0

let in_condition f =
  incr condition_depth;
  Fun.protect ~finally:(fun () -> decr condition_depth) f

(* ---------- finding a command ---------- *)

let executable p =
  match Unix.stat p with
  | { Unix.st_kind = Unix.S_REG; _ } ->
      (match Unix.access p [ Unix.X_OK ] with () -> true | exception _ -> false)
  | _ -> false
  | exception _ -> false

let search st name =
  if String.contains name '/' then (if executable name then Some name else None)
  else
    let path = State.get_or st "PATH" "/usr/bin:/bin" in
    List.find_map (fun d ->
        let p = if d = "" then name else Filename.concat d name in
        if executable p then Some p else None)
      (String.split_on_char ':' path)

(* ---------- redirection (2.7) ---------- *)

type saved = { fd : int; backup : Unix.file_descr option }

type target = Use of Unix.file_descr | Close_fd

let heredoc_fd body =
  (* the body goes through a file, which has no size limit, unlike the
     pipe a shell could otherwise use *)
  let (name, oc) = Filename.open_temp_file ~mode:[ Open_binary ] "sh" "" in
  output_string oc body;
  close_out oc;
  let fd = Unix.openfile name [ Unix.O_RDONLY ] 0 in
  (try Unix.unlink name with _ -> ());
  fd

(* A redirection that cannot be made (2.8.1).  It is an error of the
   command, not of the shell, except with a special built-in: there the
   shell exits, which is what each caller below decides.  The wording
   is the reference shell's, since a script that reads the message reads
   that one. *)
exception Redirect of string

let cannot verb name e =
  raise (Redirect (Printf.sprintf "cannot %s %s: %s" verb name (Unix.error_message e)))

let open_for verb name flags =
  match Unix.openfile name flags 0o666 with
  | fd -> fd
  | exception Unix.Unix_error (e, _, _) -> cannot verb name e

let target_of st r =
  match r.rop with
  | Here _ -> Use (heredoc_fd (Expand.to_string st r.rword))
  | Dup_out | Dup_in ->
      let text = Expand.to_string st r.rword in
      if text = "-" then Close_fd
      else (match int_of_string_opt text with
          | Some k ->
              (match Unix.dup ~cloexec:false (fd_of_int k) with
               | fd -> Use fd
               | exception Unix.Unix_error (e, _, _) ->
                   raise (Redirect (text ^ ": " ^ Unix.error_message e)))
          | None ->
              (* a name, as in `>&file', which dash allows *)
              let flags = if r.rop = Dup_in then [ Unix.O_RDONLY ]
                else [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] in
              Use (open_for (if r.rop = Dup_in then "open" else "create") text flags))
  | op ->
      let name = Expand.to_string st r.rword in
      let flags = match op with
        | Out | Out_force -> [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ]
        | Append -> [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_APPEND ]
        | In -> [ Unix.O_RDONLY ]
        | In_out -> [ Unix.O_RDWR; Unix.O_CREAT ]
        | _ -> [ Unix.O_RDONLY ] in
      if op = Out && st.opts.noclobber && Sys.file_exists name then
        raise (Redirect (Printf.sprintf "cannot create %s: File exists" name));
      Use (open_for (if op = In then "open" else "create") name flags)

let restore saved =
  if saved <> [] then flush_all ();
  List.iter (fun s ->
      match s.backup with
      | Some b -> Unix.dup2 ~cloexec:false b (fd_of_int s.fd); Unix.close b
      | None -> (try Unix.close (fd_of_int s.fd) with _ -> ())) saved

let apply st redirs =
  let saved = ref [] in
  if redirs <> [] then flush_all ();
  let one r =
    let t = target_of st r in
    let backup = match Unix.dup ~cloexec:true (fd_of_int r.rfd) with
      | fd -> Some fd
      | exception _ -> None in
    saved := { fd = r.rfd; backup } :: !saved;
    match t with
    | Close_fd -> (try Unix.close (fd_of_int r.rfd) with _ -> ())
    | Use fd ->
        if int_of_fd fd <> r.rfd then begin
          Unix.dup2 ~cloexec:false fd (fd_of_int r.rfd);
          Unix.close fd
        end in
  (* One of them failing undoes the ones before it.  The command will
     not run, so the shell's own descriptors have to be as they were:
     otherwise `echo a > file 2> /nowhere' would leave everything the
     script printed afterwards going into the file. *)
  (match List.iter one redirs with
   | () -> ()
   | exception e -> restore !saved; raise e);
  !saved

(* A redirection error is the command's, not the shell's: the message,
   a status of 2, and the shell carries on.  The exception is a special
   built-in, and the callers that run one say so (2.8.1). *)
let redirect_failed st msg =
  Printf.eprintf "%s: %s\n" st.State.arg0 msg;
  flush stderr;
  2

let with_redirs st redirs f =
  match apply st redirs with
  | saved -> Fun.protect ~finally:(fun () -> restore saved) f
  | exception Redirect msg -> redirect_failed st msg

(* ---------- traps (2.11) ---------- *)

let sync_traps st =
  List.iter (fun (name, number) ->
      if name <> "EXIT" && number <> 9 && number <> 19 then
        match State.trap_of st name with
        | None -> (try Sys.set_signal number Sys.Signal_default with _ -> ())
        | Some "" -> (try Sys.set_signal number Sys.Signal_ignore with _ -> ())
        | Some _ ->
            (* the action runs between commands, not inside the handler *)
            (try Sys.set_signal number
                   (Sys.Signal_handle (fun _ -> Hashtbl.replace State.pending name ()))
             with _ -> ()))
    State.signals

(* ---------- tracing ---------- *)

let trace st fields =
  if st.opts.xtrace then begin
    let ps4 = State.get_or st "PS4" "+ " in
    prerr_string ps4;
    prerr_string (String.concat " " fields);
    prerr_newline ()
  end

(* ---------- running ---------- *)

let rec run_program st prog =
  List.fold_left (fun _ stmt -> run_stmt st stmt) 0 prog

and run_stmt st stmt =
  if stmt.async then begin
    flush_all ();
    match Unix.fork () with
    | 0 ->
        st.subshell <- true;
        st.traps <- [];
        (* Without job control, a command run asynchronously has SIGINT
           and SIGQUIT ignored (2.11), so that an interrupt meant for the
           script does not also stop what it started; a program that
           wants them installs its own handler and overrides this. *)
        (try Sys.set_signal Sys.sigint Sys.Signal_ignore with _ -> ());
        (try Sys.set_signal Sys.sigquit Sys.Signal_ignore with _ -> ());
        let status = try run_replaceable st stmt.ao with Exit_shell k -> k in
        exit status
    | pid -> st.last_bg <- pid; st.status <- 0; 0
  end else begin
    let status = run_and_or st stmt.ao in
    run_pending_traps st;
    status
  end

and run_and_or st ao =
  let last = ao.rest = [] in
  let status = ref (run_pipeline st ~final:last ao.first) in
  let rec go = function
    | [] -> ()
    | (is_and, p) :: rest ->
        let take = if is_and then !status = 0 else !status <> 0 in
        if take then status := run_pipeline st ~final:(rest = []) p;
        go rest in
  go ao.rest;
  !status

(* [final] says whether -e may act on this pipeline: only the last of an
   AND-OR list is tested (2.8.1). *)
and run_pipeline st ~final p =
  let status =
    match p.parts with
    | [ c ] ->
        if p.negate then in_condition (fun () -> run_command ~replace:false st c)
        else run_command ~replace:false st c
    | cmds -> run_pipe st cmds in
  let status = if p.negate then (if status = 0 then 1 else 0) else status in
  st.status <- status;
  if final && status <> 0 && st.opts.errexit && !condition_depth = 0 && not p.negate then
    raise (Exit_shell status);
  status

and run_pipe st cmds =
  let n = List.length cmds in
  let pids = ref [] in
  let carry = ref None in
  List.iteri (fun i c ->
      let (rd, wr) =
        if i < n - 1 then (let (r, w) = Unix.pipe ~cloexec:false () in (Some r, Some w))
        else (None, None) in
      flush_all ();
      match Unix.fork () with
      | 0 ->
          (match !carry with
           | Some fd -> Unix.dup2 ~cloexec:false fd Unix.stdin; Unix.close fd
           | None -> ());
          (match wr with
           | Some fd -> Unix.dup2 ~cloexec:false fd Unix.stdout; Unix.close fd
           | None -> ());
          (match rd with Some fd -> Unix.close fd | None -> ());
          st.subshell <- true;
          st.traps <- [];
          let status = try run_command ~replace:true st c with Exit_shell k -> k in
          exit status
      | pid ->
          pids := pid :: !pids;
          (match !carry with Some fd -> Unix.close fd | None -> ());
          (match wr with Some fd -> Unix.close fd | None -> ());
          carry := rd) cmds;
  (* the status of a pipeline is that of its last command *)
  let statuses = List.rev_map (fun pid -> wait_for pid) !pids in
  match List.rev statuses with last :: _ -> last | [] -> 0

and wait_for pid =
  match Unix.waitpid [] pid with
  | (_, Unix.WEXITED c) -> c
  | (_, Unix.WSIGNALED s) -> 128 + s
  | (_, Unix.WSTOPPED s) -> 128 + s
  | exception Unix.Unix_error (Unix.EINTR, _, _) -> wait_for pid
  | exception _ -> 127

(* A process that exists only to run one command lets that command
   replace it, rather than forking again: that is what makes $! the pid
   of `prog &' rather than of an intervening shell, which a script that
   signals the job it started depends on. *)
and run_replaceable st ao =
  match ao.rest, ao.first with
  | [], { negate = false; parts = [ c ] } -> run_command ~replace:true st c
  | _ -> run_and_or st ao

and run_command ~replace st cmd =
  match cmd with
  | Simple s -> run_simple ~replace st s
  | Group (prog, redirs) -> with_redirs st redirs (fun () -> run_program st prog)
  | Subshell (prog, redirs) -> run_subshell st prog redirs
  | Funcdef (name, body) ->
      st.funcs <- (name, body) :: List.remove_assoc name st.funcs;
      0
  | If (branches, orelse, redirs) ->
      with_redirs st redirs (fun () ->
          let rec go = function
            | [] -> (match orelse with Some p -> run_program st p | None -> 0)
            | (cond, body) :: rest ->
                if in_condition (fun () -> run_program st cond) = 0
                then run_program st body else go rest in
          go branches)
  | Case (subject, items, redirs) ->
      with_redirs st redirs (fun () ->
          let s = Expand.to_string st subject in
          let rec go = function
            | [] -> 0
            | (pats, body) :: rest ->
                if List.exists (fun p -> Posix.Fnmatch.matches (Expand.to_pattern st p) s) pats
                then run_program st body
                else go rest in
          go items)
  | For (name, items, body, redirs) ->
      with_redirs st redirs (fun () ->
          let values = match items with
            | Some ws -> Expand.words st ws
            | None -> st.params in         (* `for x; do' walks "$@" *)
          loop st (fun run_body ->
              let status = ref 0 in
              (try
                 List.iter (fun v ->
                     State.set st name v;
                     status := run_body ()) values
               with Exit_loop -> ());
              !status)
            body)
  | Loop l ->
      with_redirs st l.lredirs (fun () ->
          loop st (fun run_body ->
              let status = ref 0 in
              (try
                 let rec again () =
                   let c = in_condition (fun () -> run_program st l.cond) in
                   let go_on = if l.until then c <> 0 else c = 0 in
                   if go_on then (status := run_body (); again ()) in
                 again ()
               with Exit_loop -> ());
              !status)
            l.body)

(* The body of a loop, with `break' and `continue' caught: a count above
   one is passed outwards to the enclosing loop (2.14). *)
and loop st driver body =
  st.in_loop <- st.in_loop + 1;
  Fun.protect ~finally:(fun () -> st.in_loop <- st.in_loop - 1)
    (fun () ->
       driver (fun () ->
           match run_program st body with
           | s -> s
           | exception Continue 1 -> st.status
           | exception Continue k -> raise (Continue (k - 1))
           | exception Break 1 -> raise Exit_loop
           | exception Break k -> raise (Break (k - 1))))

and run_subshell st prog redirs =
  flush_all ();
  match Unix.fork () with
  | 0 ->
      st.subshell <- true;
      st.traps <- [];
      let status =
        try
          ignore (apply st redirs);
          (* a subshell holding one plain command also lets it replace
             the process *)
          match prog with
          | [ { ao; async = false } ] -> run_replaceable st ao
          | _ -> run_program st prog
        with
        | Exit_shell k -> k
        | State.Error msg -> Printf.eprintf "%s: %s\n" st.arg0 msg; 2 in
      exit status
  | pid -> wait_for pid

and run_simple ~replace st (s : simple) =
  State.set st "LINENO" (string_of_int s.line);
  let fields =
    match Expand.words st s.words with
    | f -> f
    | exception Expand.Error msg ->
        Printf.eprintf "%s: %s\n" st.arg0 msg;
        raise (Exit_shell 2) in
  if fields = [] then begin
    (* only assignments and redirections: both act on this shell, and the
       redirections are undone afterwards *)
    match apply st s.redirs with
    | exception Redirect msg -> redirect_failed st msg
    | saved ->
        Fun.protect ~finally:(fun () -> restore saved)
          (fun () ->
             List.iter (fun (n, w) ->
                 let v = Expand.to_string st w in
                 trace st [ n ^ "=" ^ v ];
                 State.set st n v) s.assigns;
             0)
  end else begin
    let name = List.hd fields in
    let args = List.tl fields in
    trace st fields;
    if name = "exec" then run_exec st args s.redirs
    else
      match List.assoc_opt name st.funcs with
      | Some body -> run_function st body args s.assigns s.redirs
      | None ->
          match Builtin.find name with
          | Some (kind, f) -> run_builtin st kind f name args s.assigns s.redirs
          | None -> run_external ~replace st name fields s.assigns s.redirs
  end

(* `exec': the redirections stay in force, and with a command the shell
   is replaced by it (XCU exec). *)
and run_exec st args redirs =
  (* exec is a special built-in, so a redirection it cannot make ends
     the shell (2.8.1) *)
  let redirect () =
    match apply st redirs with
    | saved -> ignore saved
    | exception Redirect msg -> ignore (redirect_failed st msg); raise (Exit_shell 2) in
  if args = [] then (redirect (); 0)
  else begin
    redirect ();
    flush_all ();
    match search st (List.hd args) with
    | None -> Printf.eprintf "%s: %s: not found\n" st.arg0 (List.hd args); exit 127
    | Some path ->
        (match Unix.execve path (Array.of_list args) (State.environment st) with
         | _ -> 127
         | exception Unix.Unix_error (e, _, _) ->
             Printf.eprintf "%s: %s: %s\n" st.arg0 (List.hd args) (Unix.error_message e);
             exit 126)
  end

and run_builtin st kind f name args assigns redirs =
  let values = List.map (fun (n, w) -> (n, Expand.to_string st w)) assigns in
  (* 2.14: assignments with a special built-in stay in effect; with a
     regular one they last only for the command *)
  let snapshot =
    if kind = `Special then []
    else List.map (fun (n, _) ->
        (n, match Hashtbl.find_opt st.vars n with
          | Some v -> Some { v with value = v.value }
          | None -> None)) values in
  match apply st redirs with
  | exception Redirect msg ->
      let status = redirect_failed st msg in
      (* with a special built-in the shell does not carry on *)
      if kind = `Special then raise (Exit_shell 2) else status
  | saved ->
  Fun.protect
    ~finally:(fun () ->
        restore saved;
        List.iter (fun (n, prev) ->
            match prev with
            | Some v -> Hashtbl.replace st.vars n v
            | None -> Hashtbl.remove st.vars n) snapshot)
    (fun () ->
       List.iter (fun (n, v) -> State.set st n v) values;
       match f st args with
       | status -> if name = "trap" then sync_traps st; status
       | exception State.Error msg ->
           (* 2.8.1: an error in a special built-in ends a shell that is
              not interactive; in a regular one the command fails and
              the shell carries on *)
           Printf.eprintf "%s: %s\n" st.arg0 msg;
           flush stderr;
           if kind = `Special then raise (Exit_shell 2) else 1)

and run_function st body args assigns redirs =
  let values = List.map (fun (n, w) -> (n, Expand.to_string st w)) assigns in
  let snapshot =
    List.map (fun (n, _) ->
        (n, match Hashtbl.find_opt st.vars n with
          | Some v -> Some { v with value = v.value }
          | None -> None)) values in
  let saved_params = st.params in
  match apply st redirs with
  | exception Redirect msg -> redirect_failed st msg
  | saved ->
  st.locals <- [] :: st.locals;
  st.params <- args;
  Fun.protect
    ~finally:(fun () ->
        (* what `local' displaced comes back, then the arguments, then
           the assignments made for the call *)
        (match st.locals with
         | frame :: rest ->
             List.iter (fun (n, prev) ->
                 match prev with
                 | Some v -> Hashtbl.replace st.vars n v
                 | None -> Hashtbl.remove st.vars n) frame;
             st.locals <- rest
         | [] -> ());
        st.params <- saved_params;
        restore saved;
        List.iter (fun (n, prev) ->
            match prev with
            | Some v -> Hashtbl.replace st.vars n v
            | None -> Hashtbl.remove st.vars n) snapshot)
    (fun () ->
       List.iter (fun (n, v) -> State.set st n v) values;
       match run_command ~replace:false st body with
       | s -> s
       | exception Return k -> k)

and run_external ~replace st name fields assigns redirs =
  let values = List.map (fun (n, w) -> (n, Expand.to_string st w)) assigns in
  flush_all ();
  (* [replace] means this process was made for this command alone *)
  match (if replace then 0 else Unix.fork ()) with
  | 0 ->
      (try
         List.iter (fun (n, v) -> State.set st ~export:true n v) values;
         ignore (apply st redirs);
         (* the traps of the shell are not the child's (2.11) *)
         List.iter (fun (nm, number) ->
             if nm <> "EXIT" && State.trap_of st nm <> None then
               try Sys.set_signal number Sys.Signal_default with _ -> ())
           State.signals;
         match search st name with
         | None -> Printf.eprintf "%s: %s: not found\n" st.arg0 name; exit 127
         | Some path -> Unix.execve path (Array.of_list fields) (State.environment st)
       with
       | Redirect msg -> exit (redirect_failed st msg)
       | Unix.Unix_error (e, _, _) ->
           Printf.eprintf "%s: %s: %s\n" st.arg0 name (Unix.error_message e); exit 126
       | State.Error msg -> Printf.eprintf "%s: %s\n" st.arg0 msg; exit 1)
  | pid -> wait_for pid

(* ---------- traps between commands ---------- *)

and run_pending_traps st =
  if Hashtbl.length State.pending > 0 then begin
    let names = Hashtbl.fold (fun k () acc -> k :: acc) State.pending [] in
    Hashtbl.reset State.pending;
    List.iter (fun name ->
        match State.trap_of st name with
        | Some action when action <> "" -> ignore (run_text st action)
        | _ -> ()) names
  end

(* Reading and running a program.

   The text is not parsed in one piece.  A shell reads a line, runs what
   the line says, and only then reads the next (2.10.2), and that is
   what this does: the lines before one that will not parse have
   already run, an alias or a function defined by one line is there for
   the next, and a here-document's body is read as its line is crossed.

   A syntax error ends a shell that is not interactive (2.8.1), so the
   diagnostic is followed by an exit rather than by a return, and that
   holds wherever the text came from -- the script, -c, eval, or dot. *)
and run_text st text =
  let stream = Parse.open_text text in
  (* text with no command in it -- an empty eval, a file of comments --
     has the status of a command that succeeded, not the status of
     whatever ran last *)
  let status = ref 0 in
  let fatal msg line =
    Printf.eprintf "%s: line %d: %s\n" st.arg0 line msg;
    flush stderr;
    raise (Exit_shell 2) in
  let stop = ref false in
  while not !stop do
    match Parse.next_line stream with
    | None -> stop := true
    | Some stmts -> List.iter (fun stmt -> status := run_stmt st stmt) stmts
    | exception Parse.Error (msg, line) -> fatal msg line
    | exception Lex.Error (msg, line) -> fatal msg line
    | exception Word.Error msg ->
        Printf.eprintf "%s: %s\n" st.arg0 msg; flush stderr;
        raise (Exit_shell 2)
  done;
  !status

(* ---------- command substitution ---------- *)

and command_output st prog =
  let (rd, wr) = Unix.pipe ~cloexec:false () in
  flush_all ();
  match Unix.fork () with
  | 0 ->
      Unix.close rd;
      Unix.dup2 ~cloexec:false wr Unix.stdout;
      Unix.close wr;
      st.subshell <- true;
      st.traps <- [];
      let status =
        try run_program st prog with
        | Exit_shell k -> k
        | State.Error msg -> Printf.eprintf "%s: %s\n" st.arg0 msg; 2 in
      exit status
  | pid ->
      Unix.close wr;
      let b = Buffer.create 1024 in
      let chunk = Bytes.create 65536 in
      let rec drain () =
        match Unix.read rd chunk 0 65536 with
        | 0 -> ()
        | k -> Buffer.add_subbytes b chunk 0 k; drain ()
        | exception Unix.Unix_error (Unix.EINTR, _, _) -> drain () in
      drain ();
      Unix.close rd;
      st.status <- wait_for pid;
      Buffer.contents b

(* ---------- closing the loops between the modules ---------- *)

let () = Expand.subst_hook := command_output
let () = Builtin.eval_hook := run_text
let () = Builtin.search_hook := (fun st name -> search st name)

let () =
  Builtin.exec_hook := (fun st argv ->
      match argv with
      | [] -> 0
      | name :: args ->
          (match Builtin.find name with
           | Some (_, f) -> f st args
           | None -> run_external ~replace:false st name argv [] []))
