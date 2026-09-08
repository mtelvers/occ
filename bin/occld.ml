(* occld: a static linker with the parts of ld's command line that gcc
   and the OCaml build use. *)

let usage = "usage: occld [-r] [-o output] [-L dir] [-l lib] [-e entry] files...\n"

let () =
  let output = ref "a.out" and entry = ref "_start" and search = ref [] and items = ref [] in
  (* -r asks for another relocatable object rather than an executable,
     which is a different job: see src/linker/partial.ml *)
  let relocatable = ref false in
  let rec go = function
    | [] -> ()
    | "-o" :: f :: rest -> output := f; go rest
    | ("-r" | "-i" | "--relocatable") :: rest -> relocatable := true; go rest
    | "-e" :: e :: rest -> entry := e; go rest
    | "-L" :: d :: rest -> search := !search @ [ d ]; go rest
    | "-l" :: l :: rest -> items := Occ.Link.Library l :: !items; go rest
    | ("-static" | "--start-group" | "--end-group" | "-E" | "--export-dynamic" | "--build-id" | "-z" | "--hash-style" | "--as-needed" | "-m") :: rest ->
        (match rest with
         | arg :: rest' when (List.mem (List.hd rest) [ "-z"; "--hash-style"; "-m" ]) -> ignore arg; go rest'
         | _ -> go rest)
    | a :: rest when String.length a > 2 && String.sub a 0 2 = "-L" -> search := !search @ [ String.sub a 2 (String.length a - 2) ]; go rest
    | a :: rest when String.length a > 2 && String.sub a 0 2 = "-l" -> items := Occ.Link.Library (String.sub a 2 (String.length a - 2)) :: !items; go rest
    | "--version" :: _ -> print_string "occld (occ) 0.1\n"; exit 0
    | a :: _ when String.length a > 0 && a.[0] = '-' -> Printf.eprintf "occld: unknown option %s\n%s" a usage; exit 2
    | a :: rest -> items := (if Filename.check_suffix a ".a" then Occ.Link.Archive a else Occ.Link.Object a) :: !items; go rest in
  go (List.tl (Array.to_list Sys.argv));
  if !items = [] then begin prerr_string usage; exit 2 end;
  try
    if !relocatable then Occ.Partial.link ~output:!output ~search:!search (List.rev !items)
    else Occ.Link.link ~output:!output ~entry:!entry ~search:!search (List.rev !items)
  with Failure msg -> prerr_endline ("occld: " ^ msg); exit 1
