(* occmake: a make in the GNU dialect, enough of it to drive the OCaml
   build. *)
let () =
  try exit (Mk.Make.run Sys.argv)
  with
  | Mk.Expand.Make_error msg -> Printf.eprintf "occmake: *** %s.  Stop.\n" msg; exit 2
  | Sys_error msg -> Printf.eprintf "occmake: %s\n" msg; exit 2
