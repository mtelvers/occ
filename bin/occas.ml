(* occas: an assembler with the command line of GNU as, for use as
   ocamlopt's ASM (configure AS=occas) and by occ's own driver. *)

let usage = "usage: occas [--64] [-march=...] [-o output] file.s...\n"

let () =
  let output = ref "a.out" and inputs = ref [] in
  (* the machine, from the host unless asked otherwise: an assembler for
     one machine cannot guess from the text, since a mnemonic means
     different things on each *)
  Occ.Target.machine := Occ.Host.machine;
  let target name =
    match name with
    | "x86_64" | "amd64" | "x86-64" -> Occ.Target.machine := Occ.Target.Amd64
    | "riscv64" | "riscv" | "rv64" -> Occ.Target.machine := Occ.Target.Riscv64
    | _ -> Printf.eprintf "occas: unknown target %s\n" name; exit 2 in
  (match Sys.getenv_opt "OCC_TARGET" with Some t -> target t | None -> ());
  let rec go = function
    | [] -> ()
    | "-o" :: f :: rest -> output := f; go rest
    | ("--64" | "-g" | "--gdwarf-2" | "--gdwarf-3" | "--gdwarf-4" | "--gdwarf-5" | "--noexecstack" | "-W" | "--fatal-warnings") :: rest -> go rest
    (* gcc passes these when it drives the assembler for RISC-V; the
       extensions are what we already assume, and this assembler never
       relaxes, so both are accepted and dropped *)
    | ("-mno-relax" | "-mrelax") :: rest -> go rest
    | "--target" :: t :: rest -> target t; go rest
    | f :: rest when String.length f > 8 && String.sub f 0 9 = "--target=" ->
        target (String.sub f 9 (String.length f - 9)); go rest
    | f :: rest when String.length f > 6 && String.sub f 0 7 = "-march=" -> go rest
    | f :: rest when String.length f > 6 && String.sub f 0 7 = "-mabi=" -> go rest
    | "--version" :: _ ->
        Printf.printf "occas (occ) 0.1 for %s\n" (Occ.Target.name !Occ.Target.machine); exit 0
    | f :: _ when String.length f > 0 && f.[0] = '-' -> Printf.eprintf "occas: unknown option %s\n%s" f usage; exit 2
    | f :: rest -> inputs := f :: !inputs; go rest in
  go (List.tl (Array.to_list Sys.argv));
  if !inputs = [] then begin prerr_string usage; exit 2 end;
  try Occ.Assembler.Assemble.files (List.rev !inputs) !output
  with Occ.Diag.Error (loc, msg) -> Occ.Diag.report loc msg; exit 1
