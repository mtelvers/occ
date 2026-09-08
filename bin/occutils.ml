(* occutils: every utility in one binary.

   The shell scripts call these programs by name, so the binary looks at
   the name it was invoked under (a link, as toolbin/ provides) and, when
   that is not one of them, at its first argument.  This is how one OCaml
   executable can stand in for a directory of C programs on PATH. *)

let utilities : (string * (string array -> int)) list = [
  "grep", Ut.Grep.main;
]

let usage () =
  prerr_endline "usage: occutils <utility> [argument...]";
  prerr_string "utilities:";
  List.iter (fun (n, _) -> prerr_string (" " ^ n)) utilities;
  prerr_newline ();
  2

let () =
  let argv = Sys.argv in
  let base = Filename.basename argv.(0) in
  let pick, argv =
    match List.assoc_opt base utilities with
    | Some f -> Some (base, f), argv
    | None ->
        if Array.length argv < 2 then (None, argv)
        else
          let name = argv.(1) in
          (match List.assoc_opt name utilities with
           | Some f -> Some (name, f), Array.sub argv 1 (Array.length argv - 1)
           | None -> (None, argv)) in
  match pick with
  | None -> exit (usage ())
  | Some (name, f) ->
      Ut.Util.prog := name;
      let status =
        match f argv with
        | s -> s
        | exception Ut.Util.Fail s -> Ut.Util.flush_out (); s
        | exception Sys_error msg ->
            Ut.Util.flush_out ();
            Printf.eprintf "%s: %s\n" name msg; 2 in
      exit status
