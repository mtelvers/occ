(* occas: an assembler with the command line of GNU as, for use as
   ocamlopt's ASM (configure AS=occas) and by occ's own driver. *)

let usage = "usage: occas [--64] [-o output] file.s...\n"

let () =
  let output = ref "a.out" and inputs = ref [] in
  let rec go = function
    | [] -> ()
    | "-o" :: f :: rest -> output := f; go rest
    | ("--64" | "-g" | "--gdwarf-2" | "--gdwarf-3" | "--gdwarf-4" | "--gdwarf-5" | "--noexecstack" | "-W" | "--fatal-warnings") :: rest -> go rest
    | "--version" :: _ -> print_string "occas (occ) 0.1\n"; exit 0
    | f :: _ when String.length f > 0 && f.[0] = '-' -> Printf.eprintf "occas: unknown option %s\n%s" f usage; exit 2
    | f :: rest -> inputs := f :: !inputs; go rest in
  go (List.tl (Array.to_list Sys.argv));
  if !inputs = [] then begin prerr_string usage; exit 2 end;
  try Occ.Assemble.files (List.rev !inputs) !output
  with Occ.Diag.Error (loc, msg) -> Occ.Diag.report loc msg; exit 1
