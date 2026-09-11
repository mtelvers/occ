(* occld: a static linker with the parts of ld's command line that gcc
   and the OCaml build use. *)

let usage = "usage: occld [-r|-shared] [-o output] [-L dir] [-l lib] [-e entry] [-soname name] files...\n"

let () =
  let output = ref "a.out" and entry = ref "_start" and search = ref [] and items = ref [] in
  (* -r asks for another relocatable object rather than an executable,
     which is a different job: see src/linker/partial.ml.  -shared asks
     for a shared object, which is the same job with the loader left
     something to do: see src/linker/dynamic.ml *)
  let relocatable = ref false and shared = ref false and soname = ref "" in
  let export_all = ref false and rpath = ref "" and static = ref false in
  let rec go = function
    | [] -> ()
    | "-o" :: f :: rest -> output := f; go rest
    | ("-r" | "-i" | "--relocatable") :: rest -> relocatable := true; go rest
    | "-e" :: e :: rest -> entry := e; go rest
    | ("-shared" | "--shared" | "-Bshareable") :: rest -> shared := true; go rest
    | ("-soname" | "--soname" | "-h") :: n :: rest -> soname := n; go rest
    | a :: rest when String.length a > 8 && String.sub a 0 8 = "-soname=" ->
        soname := String.sub a 8 (String.length a - 8); go rest
    | "-L" :: d :: rest -> search := !search @ [ d ]; go rest
    | "-l" :: l :: rest -> items := Occ.Link.Library l :: !items; go rest
    | ("-E" | "--export-dynamic") :: rest -> export_all := true; go rest
    | ("-rpath" | "--rpath" | "-rpath-link") :: d :: rest -> rpath := d; go rest
    | "-static" :: rest -> static := true; go rest
    | ("--start-group" | "--end-group" | "--build-id" | "-z" | "--hash-style" | "--as-needed" | "-m") :: rest ->
        (match rest with
         | arg :: rest' when (List.mem (List.hd rest) [ "-z"; "--hash-style"; "-m" ]) -> ignore arg; go rest'
         | _ -> go rest)
    | a :: rest when String.length a > 2 && String.sub a 0 2 = "-L" -> search := !search @ [ String.sub a 2 (String.length a - 2) ]; go rest
    | a :: rest when String.length a > 2 && String.sub a 0 2 = "-l" -> items := Occ.Link.Library (String.sub a 2 (String.length a - 2)) :: !items; go rest
    | "--version" :: _ -> print_string "occld (occ) 0.1\n"; exit 0
    | a :: _ when String.length a > 0 && a.[0] = '-' -> Printf.eprintf "occld: unknown option %s\n%s" a usage; exit 2
    | a :: rest ->
        items :=
          (if Filename.check_suffix a ".a" then Occ.Link.Archive a
           else if Filename.check_suffix a ".so" then Occ.Link.Shared a
           else Occ.Link.Object a) :: !items;
        go rest in
  go (List.tl (Array.to_list Sys.argv));
  if !items = [] then begin prerr_string usage; exit 2 end;
  try
    if !relocatable then Occ.Partial.link ~output:!output ~search:!search (List.rev !items)
    else if !shared then
      Occ.Link.link ~shared:true ~soname:!soname ~export_all:true ~rpath:!rpath
        ~output:!output ~entry:None ~search:!search (List.rev !items)
    else
      Occ.Link.link ~export_all:!export_all ~prefer_shared:(not !static) ~rpath:!rpath
        ~output:!output ~entry:(Some !entry) ~search:!search (List.rev !items)
  with Failure msg -> prerr_endline ("occld: " ^ msg); exit 1
