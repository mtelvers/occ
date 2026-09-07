(* The ar archive format, GNU variant (ar(5); binutils' "archive
   format").  Static libraries are archives of object files with an index
   of the symbols each member defines, so a linker can pull in only the
   members it needs.

   An archive is the magic string "!<arch>\n" followed by members, each a
   60-byte header of space-padded decimal fields and then the body,
   padded to an even offset with "\n".  Two special members come first:
   "/" holds the symbol index (a big-endian count, one big-endian offset
   per symbol pointing at its member's header, then the NUL-terminated
   names) and "//" holds the names too long for the 16-byte field, which
   members refer to as "/offset".

   Output is deterministic (timestamps and owners are zero, as with GNU
   ar's D modifier, the default on this platform), so an archive built
   here is byte for byte what GNU ar produces. *)

type member = {
  name : string;     (* the file name without directories *)
  body : string;
}

let magic = "!<arch>\n"

(* ---- Reading ------------------------------------------------------------- *)

let error fmt = Printf.ksprintf failwith fmt

let field s off len = String.trim (String.sub s off len)

let read_int s off len =
  match int_of_string_opt (field s off len) with Some n -> n | None -> error "malformed archive header"

(* the members of an archive, with the two index members resolved away *)
let read (s : string) : member list =
  if String.length s < 8 || String.sub s 0 8 <> magic then error "not an archive";
  let long_names = ref "" in
  let rec go off acc =
    if off + 60 > String.length s then List.rev acc
    else begin
      if String.sub s (off + 58) 2 <> "`\n" then error "malformed archive header at %d" off;
      let size = read_int s (off + 48) 10 in
      let body = String.sub s (off + 60) size in
      let next = off + 60 + size + (size land 1) in
      let raw = field s off 16 in
      if raw = "/" then go next acc                         (* symbol index: rebuilt on writing *)
      else if raw = "//" then begin long_names := body; go next acc end
      else begin
        let name =
          if String.length raw > 1 && raw.[0] = '/' then begin
            let start = int_of_string (String.sub raw 1 (String.length raw - 1)) in
            let stop = String.index_from !long_names start '/' in
            String.sub !long_names start (stop - start)
          end else if raw <> "" && raw.[String.length raw - 1] = '/' then String.sub raw 0 (String.length raw - 1)
          else raw in
        go next ({ name; body } :: acc)
      end
    end in
  go 8 []

(* ---- Writing ------------------------------------------------------------- *)

let pad_field b s len =
  Buffer.add_string b s;
  for _ = String.length s + 1 to len do Buffer.add_char b ' ' done

(* a member header; the special members leave owner and mode fields blank or zero *)
let header b ~name ~mtime ~uid ~gid ~mode ~size =
  pad_field b name 16; pad_field b mtime 12; pad_field b uid 6; pad_field b gid 6; pad_field b mode 8;
  pad_field b (string_of_int size) 10;
  Buffer.add_string b "`\n"

let be32 b v = for i = 3 downto 0 do Buffer.add_char b (Char.chr ((v lsr (8 * i)) land 0xff)) done

let write (members : member list) : string =
  (* the long-name table: "name/\n" for every name over 15 characters *)
  let long = Buffer.create 256 in
  let name_field = List.map (fun m ->
      if String.length m.name > 15 then begin
        let off = Buffer.length long in
        Buffer.add_string long m.name; Buffer.add_string long "/\n";
        "/" ^ string_of_int off
      end else m.name ^ "/") members in
  if Buffer.length long land 1 = 1 then Buffer.add_char long '\n';
  (* the symbol index: names per member, then offsets once the layout is known *)
  let symbols = List.map (fun m -> Elf_read.exported_symbols m.body) members in
  let names_size = List.fold_left (fun n names -> List.fold_left (fun n s -> n + String.length s + 1) n names) 0 symbols in
  let count = List.fold_left (fun n names -> n + List.length names) 0 symbols in
  let index_size = 4 + 4 * count + names_size in
  let index_size = index_size + (index_size land 1) in    (* padded with NUL, inside the size *)
  let first_member = 8 + 60 + index_size + (if Buffer.length long > 0 then 60 + Buffer.length long else 0) in
  let offsets = ref [] in
  let pos = ref first_member in
  List.iter2 (fun m names ->
      List.iter (fun _ -> offsets := !pos :: !offsets) names;
      pos := !pos + 60 + String.length m.body + (String.length m.body land 1)) members symbols;
  let offsets = List.rev !offsets in
  (* the file *)
  let b = Buffer.create (!pos) in
  Buffer.add_string b magic;
  header b ~name:"/" ~mtime:"0" ~uid:"0" ~gid:"0" ~mode:"0" ~size:index_size;
  be32 b count;
  List.iter (be32 b) offsets;
  List.iter (fun names -> List.iter (fun s -> Buffer.add_string b s; Buffer.add_char b '\000') names) symbols;
  if (4 + 4 * count + names_size) land 1 = 1 then Buffer.add_char b '\000';
  if Buffer.length long > 0 then begin
    header b ~name:"//" ~mtime:"" ~uid:"" ~gid:"" ~mode:"" ~size:(Buffer.length long);
    Buffer.add_buffer b long
  end;
  List.iter2 (fun m field ->
      header b ~name:field ~mtime:"0" ~uid:"0" ~gid:"0" ~mode:"644" ~size:(String.length m.body);
      Buffer.add_string b m.body;
      if String.length m.body land 1 = 1 then Buffer.add_char b '\n') members name_field;
  Buffer.contents b
