(* occutils: every utility in one binary.

   The shell scripts call these programs by name, so the binary looks at
   the name it was invoked under (a link, as toolbin/ provides) and, when
   that is not one of them, at its first argument.  This is how one OCaml
   executable can stand in for a directory of C programs on PATH. *)

let usage () =
  prerr_endline "usage: occutils <utility> [argument...]";
  prerr_string "utilities:";
  List.iter (fun n -> prerr_string (" " ^ n)) (Ut.Table.names ());
  prerr_newline ();
  2

let () =
  let argv = Sys.argv in
  let base = Filename.basename argv.(0) in
  (* the name it was called under, or its first argument *)
  let pick, argv =
    match Ut.Table.find base with
    | Some entry -> (Some entry, argv)
    | None ->
        if Array.length argv < 2 then (None, argv)
        else
          (match Ut.Table.find argv.(1) with
           | Some entry -> (Some entry, Array.sub argv 1 (Array.length argv - 1))
           | None -> (None, argv)) in
  match pick with
  | None -> exit (usage ())
  | Some entry ->
      let status =
        match Ut.Table.run entry argv with
        | s -> s
        | exception Ut.Util.Fail s -> Ut.Util.flush_out (); s
        | exception Sys_error msg ->
            Ut.Util.flush_out ();
            Printf.eprintf "%s: %s\n" entry.Ut.Table.name msg; 2
        | exception Unix.Unix_error (e, _, arg) ->
            Ut.Util.flush_out ();
            Printf.eprintf "%s: %s%s\n" entry.Ut.Table.name
              (if arg = "" then "" else arg ^ ": ") (Unix.error_message e); 2 in
      exit status
