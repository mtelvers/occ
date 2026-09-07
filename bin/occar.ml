(* occar: the ar command, enough of it for OCaml's build (ar rc, ar rcs
   from ocamlmklib, and the usual t, x, d, q for looking at archives). *)

let usage = "usage: occar {rqtxd}[csvD] archive [files...]\n"

let read_file f = In_channel.with_open_bin f In_channel.input_all
let write_file f s = Out_channel.with_open_bin f (fun oc -> output_string oc s)

let () =
  match Array.to_list Sys.argv with
  | _ :: "--version" :: _ -> print_string "occar (occ) 0.1\n"
  | _ :: ops :: archive :: files ->
      let ops = if String.length ops > 0 && ops.[0] = '-' then String.sub ops 1 (String.length ops - 1) else ops in
      let has c = String.contains ops c in
      let verbose = has 'v' in
      let existing = if Sys.file_exists archive then Occ.Ar.read (read_file archive) else [] in
      let basename f = Filename.basename f in
      let result =
        if has 'r' then
          (* replace members of the same name in place, append the rest *)
          let added = List.map (fun f -> { Occ.Ar.name = basename f; body = read_file f }) files in
          let replaced = List.map (fun (m : Occ.Ar.member) ->
              match List.find_opt (fun (a : Occ.Ar.member) -> a.name = m.name) added with Some a -> a | None -> m) existing in
          let fresh = List.filter (fun (a : Occ.Ar.member) -> not (List.exists (fun (m : Occ.Ar.member) -> m.name = a.name) existing)) added in
          Some (replaced @ fresh)
        else if has 'q' then
          Some (existing @ List.map (fun f -> { Occ.Ar.name = basename f; body = read_file f }) files)
        else if has 'd' then
          Some (List.filter (fun (m : Occ.Ar.member) -> not (List.mem m.name (List.map basename files))) existing)
        else if has 't' then begin
          List.iter (fun (m : Occ.Ar.member) ->
              if files = [] || List.mem m.name (List.map basename files) then print_endline m.name) existing;
          None
        end else if has 'x' then begin
          List.iter (fun (m : Occ.Ar.member) ->
              if files = [] || List.mem m.name (List.map basename files) then begin
                if verbose then Printf.printf "x - %s\n" m.name;
                write_file m.name m.body
              end) existing;
          None
        end else if has 's' then Some existing   (* ranlib: rewrite with a fresh index *)
        else begin prerr_string usage; exit 2 end in
      (match result with
       | Some members -> write_file archive (Occ.Ar.write members)
       | None -> ())
  | _ -> prerr_string usage; exit 2
