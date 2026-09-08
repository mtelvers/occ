(* Partial linking: `ld -r' (ELF specification 1.2, "Linking view";
   System V x86-64 ABI supplement chapter 4).

   The output is not an executable but another relocatable object, so
   nothing is given an address and no relocation is computed.  What has
   to be done instead is bookkeeping: the input sections are
   concatenated into output sections of the same name, the symbol tables
   are merged into one, and every relocation is rewritten to say where
   its target now sits and which entry of the new symbol table it refers
   to.

   Two points are easy to get wrong and both silently corrupt the
   result.  A relocation against a section symbol carries the offset
   within that section in its addend, and the section symbol of the
   merged output section points at the start of the whole thing, so the
   addend must gain the offset at which this input's piece was placed.
   And a local symbol keeps its section, so its value moves by the same
   amount.

   OCaml needs this for two things: `ocamlopt -pack', which packs the
   modules of a library into one object, and `-output-complete-obj',
   which puts a whole program including the runtime into one. *)

open Elf_in

let error fmt = Printf.ksprintf failwith fmt

let et_rel = 1
let sht_null = 0 and sht_symtab = 2 and sht_strtab = 3
let sht_rela = 4 and sht_nobits = 8 and sht_rel = 9 and sht_group = 17
let shf_info_link = 0x40
let stb_local = 0 and stb_global = 1 and stb_weak = 2
let stt_section = 3 and stt_file = 4
let shn_undef = 0 and shn_abs = 0xfff1 and shn_common = 0xfff2

(* ---- The output ---------------------------------------------------------- *)

type piece = { pobj : Elf_in.t; pindex : int; poffset : int }

(* A symbol of the output.  Locals keep no object of origin: two objects
   may each have a local of the same name and both are kept. *)
type osym = {
  syname : string;
  mutable sybind : int;
  mutable sytype : int;
  mutable syother : int;
  mutable syshndx : int;                     (* an output section index, or SHN_* *)
  mutable syvalue : int;
  mutable sysize : int;
  mutable syindex : int;                     (* in the output symbol table *)
}

type osec = {
  oname : string;
  mutable otype : int;
  mutable oflags : int;
  mutable oalign : int;
  mutable oentsize : int;
  mutable osize : int;
  mutable opieces : piece list;              (* reversed while collecting *)
  mutable oshndx : int;                      (* in the output's section headers *)
  mutable osym : osym option;                (* its section symbol *)
}

type t = {
  mutable sections : osec list;              (* reversed *)
  by_name : (string, osec) Hashtbl.t;
  globals : (string, osym) Hashtbl.t;
  mutable locals : osym list;                (* reversed *)
  mutable global_order : osym list;          (* reversed *)
  comdat : (string, unit) Hashtbl.t;
  (* per input object, the map from its symbol indices to output symbols *)
  mutable maps : (Elf_in.t * osym option array) list;
  mutable objects : Elf_in.t list;           (* reversed *)
  places : (string * int, int) Hashtbl.t;    (* (object file, section) -> offset *)
  kept : (string * int, osec) Hashtbl.t;     (* (object file, section) -> where it went *)
}

let create () = {
  sections = []; by_name = Hashtbl.create 32; globals = Hashtbl.create 4096;
  locals = []; global_order = []; comdat = Hashtbl.create 64; maps = [];
  objects = []; places = Hashtbl.create 1024; kept = Hashtbl.create 1024;
}

let osec st name typ flags align entsize =
  match Hashtbl.find_opt st.by_name name with
  | Some o ->
      o.oalign <- max o.oalign align;
      if o.otype = sht_nobits && typ <> sht_nobits then o.otype <- typ;
      o.oflags <- o.oflags lor flags;
      if o.oentsize = 0 then o.oentsize <- entsize;
      o
  | None ->
      let o = { oname = name; otype = typ; oflags = flags; oalign = align;
                oentsize = entsize; osize = 0; opieces = []; oshndx = 0; osym = None } in
      Hashtbl.replace st.by_name name o;
      st.sections <- o :: st.sections;
      o

(* Which input sections are carried over.  The tables that describe the
   object itself are rebuilt, so they are dropped; everything else,
   including .eh_frame, the note sections and the constructor arrays, is
   kept under its own name -- `ld -r' merges sections that have the same
   name and no others, so .text.foo stays .text.foo. *)
let carried (s : section) =
  s.typ <> sht_null && s.typ <> sht_symtab && s.typ <> sht_strtab
  && s.typ <> sht_rela && s.typ <> sht_rel && s.typ <> sht_group

let align_up n a = if a <= 1 then n else (n + a - 1) / a * a

(* ---- Loading ------------------------------------------------------------- *)

let add_object st (obj : Elf_in.t) =
  st.objects <- obj :: st.objects;
  (* A COMDAT group is kept once: the members of a repeated signature are
     dropped, as they would be in a final link. *)
  let dropped = Hashtbl.create 8 in
  List.iter (fun (signature, members) ->
      if Hashtbl.mem st.comdat signature then
        List.iter (fun i -> Hashtbl.replace dropped i ()) members
      else Hashtbl.replace st.comdat signature ()) obj.groups;
  Array.iteri (fun i (s : section) ->
      if carried s && not (Hashtbl.mem dropped i) then begin
        let o = osec st s.name s.typ s.flags s.addralign s.entsize in
        let at = align_up o.osize (max 1 s.addralign) in
        o.osize <- at + s.size;
        o.opieces <- { pobj = obj; pindex = i; poffset = at } :: o.opieces;
        Hashtbl.replace st.places (obj.file, i) at;
        Hashtbl.replace st.kept (obj.file, i) o
      end) obj.sections

let load st ~search items =
  let archives = ref [] in
  let rec add item =
    match item with
    | Link.Object f -> add_object st (Elf_in.read f (Link.read_file f))
    | Link.Archive f | Link.Library f ->
        let f = match item with Link.Library name -> Link.find_library search name | _ -> f in
        let text = Link.read_file f in
        if String.length text >= 8 && String.sub text 0 8 = "!<arch>\n" then
          archives := (f, Ar.read text) :: !archives
        else if Elf_in.is_object text then add_object st (Elf_in.read f text)
        else List.iter add (Link.script_items text) in
  List.iter add items;
  (* An archive still gives up only the members that are wanted, and
     wanting one may want another, so this runs to a fixed point. *)
  let undefined = Hashtbl.create 256 in
  let defined = Hashtbl.create 4096 in
  let note_object (obj : Elf_in.t) =
    Array.iter (fun (s : symbol) ->
        if s.bind <> stb_local && s.sname <> "" then begin
          if s.shndx = shn_undef then
            (if not (Hashtbl.mem defined s.sname) then Hashtbl.replace undefined s.sname ())
          else (Hashtbl.replace defined s.sname (); Hashtbl.remove undefined s.sname)
        end) obj.symbols in
  List.iter note_object st.objects;
  let pending = List.rev_map (fun (name, members) ->
      (name, Array.of_list (List.map (fun (m : Ar.member) ->
           (m, Elf_read.exported_symbols m.body, ref false)) members))) !archives in
  let changed = ref true in
  while !changed do
    changed := false;
    List.iter (fun (aname, members) ->
        Array.iter (fun ((m : Ar.member), syms, loaded) ->
            if not !loaded && List.exists (fun s -> Hashtbl.mem undefined s) syms then begin
              loaded := true;
              changed := true;
              let obj = Elf_in.read (Printf.sprintf "%s(%s)" aname m.Ar.name) m.Ar.body in
              add_object st obj;
              note_object obj
            end) members) pending
  done

(* ---- Merging the symbol tables -------------------------------------------- *)

(* One section symbol per output section, which every relocation against
   a section refers to.  They are made before the merge so that the map
   from an input's symbols can name them. *)
let section_symbols sections =
  List.iter (fun o ->
      o.osym <- Some { syname = ""; sybind = stb_local; sytype = stt_section;
                       syother = 0; syshndx = o.oshndx; syvalue = 0; sysize = 0;
                       syindex = -1 }) sections

let merge_symbols st =
  List.iter (fun (obj : Elf_in.t) ->
      let map = Array.make (Array.length obj.symbols) None in
      (* the output section an input section became, if it was kept *)
      let where (shndx : int) =
        if shndx = shn_undef || shndx = shn_abs || shndx = shn_common then None
        else Hashtbl.find_opt st.kept (obj.file, shndx) in
      let placed shndx = match Hashtbl.find_opt st.places (obj.file, shndx) with
        | Some at -> at
        | None -> 0 in
      Array.iteri (fun i (s : symbol) ->
          if s.stype = stt_section then
            (match where s.shndx with
             | Some o -> map.(i) <- o.osym
             | None -> ())
          else if s.bind = stb_local then begin
            if s.sname <> "" then begin
              let shndx, value =
                if s.stype = stt_file then (shn_abs, 0)
                else match where s.shndx with
                  | Some o -> (o.oshndx, s.value + placed s.shndx)
                  | None ->
                      if s.shndx = shn_abs then (shn_abs, s.value)
                      else (shn_undef, s.value) in
              let sym = { syname = s.sname; sybind = stb_local; sytype = s.stype;
                          syother = s.other; syshndx = shndx; syvalue = value;
                          sysize = s.ssize; syindex = -1 } in
              st.locals <- sym :: st.locals;
              map.(i) <- Some sym
            end
          end
          else if s.sname <> "" then begin
            let sym =
              match Hashtbl.find_opt st.globals s.sname with
              | Some e -> e
              | None ->
                  let sym = { syname = s.sname; sybind = s.bind; sytype = s.stype;
                              syother = s.other; syshndx = shn_undef; syvalue = 0;
                              sysize = 0; syindex = -1 } in
                  Hashtbl.replace st.globals s.sname sym;
                  st.global_order <- sym :: st.global_order;
                  sym in
            (* Take this definition when it is the first, when it replaces
               a reference or a common symbol, or when it is strong and
               what is there is weak; two strong definitions of one name
               are an error. *)
            let take () =
              sym.sybind <- s.bind;
              sym.sytype <- s.stype;
              sym.syother <- s.other;
              sym.sysize <- s.ssize;
              if s.shndx = shn_common then (sym.syshndx <- shn_common; sym.syvalue <- s.value)
              else if s.shndx = shn_abs then (sym.syshndx <- shn_abs; sym.syvalue <- s.value)
              else match where s.shndx with
                | Some o -> sym.syshndx <- o.oshndx; sym.syvalue <- s.value + placed s.shndx
                | None -> sym.syshndx <- shn_undef in
            (if s.shndx = shn_undef then ()
             else if sym.syshndx = shn_undef then take ()
             else if sym.syshndx = shn_common then begin
               if s.shndx = shn_common then begin
                 (* the larger common wins, with the stricter alignment *)
                 if s.ssize > sym.sysize then sym.sysize <- s.ssize;
                 sym.syvalue <- max sym.syvalue s.value
               end else take ()
             end
             else if sym.sybind = stb_weak && s.bind = stb_global then take ()
             else if s.bind = stb_global && sym.sybind = stb_global then
               error "multiple definition of `%s' (%s)" s.sname obj.file);
            map.(i) <- Some sym
          end) obj.symbols;
      st.maps <- (obj, map) :: st.maps) st.objects

(* ---- Writing ------------------------------------------------------------- *)

let u16 b v = Buffer.add_char b (Char.chr (v land 255)); Buffer.add_char b (Char.chr ((v lsr 8) land 255))
let u32 b v = u16 b (v land 0xffff); u16 b ((v lsr 16) land 0xffff)
let u64 b v = u32 b (v land 0xffffffff); u32 b ((v asr 32) land 0xffffffff)

type strtab = { sbuf : Buffer.t; sof : (string, int) Hashtbl.t }

let strtab () =
  let s = { sbuf = Buffer.create 4096; sof = Hashtbl.create 256 } in
  Buffer.add_char s.sbuf '\000';
  s

let intern s name =
  if name = "" then 0
  else match Hashtbl.find_opt s.sof name with
    | Some off -> off
    | None ->
        let off = Buffer.length s.sbuf in
        Buffer.add_string s.sbuf name;
        Buffer.add_char s.sbuf '\000';
        Hashtbl.replace s.sof name off;
        off

let link ~output ~search items =
  let st = create () in
  load st ~search items;
  st.objects <- List.rev st.objects;
  let sections = List.rev st.sections in
  (* the output's section numbering: null, then the carried sections,
     then a .rela for each that needs one, then symtab, strtab, shstrtab *)
  List.iteri (fun i o -> o.oshndx <- i + 1) sections;
  section_symbols sections;
  merge_symbols st;
  st.maps <- List.rev st.maps;
  let locals = List.rev st.locals and globals = List.rev st.global_order in
  (* the section symbols come before the other locals, as the convention
     is; relocations against a section refer to these *)
  let all_locals =
    List.filter_map (fun o -> o.osym) sections @ locals in
  let table = all_locals @ globals in
  List.iteri (fun i s -> s.syindex <- i + 1) table;    (* entry 0 is the null one *)
  (* the relocations of each output section, in the order the pieces were
     placed *)
  let relocs = List.map (fun o ->
      let out = ref [] in
      List.iter (fun p ->
          match List.assq_opt p.pobj st.maps with
          | None -> ()
          | Some map ->
              (match List.assoc_opt p.pindex p.pobj.relocs with
               | None -> ()
               | Some entries ->
                   Array.iter (fun (r : reloc) ->
                       let target = map.(r.sym) in
                       match target with
                       | None -> error "%s: relocation against a dropped symbol" p.pobj.file
                       | Some sym ->
                           (* A relocation against a section symbol counts
                              from the start of that section, which is now
                              the start of the merged one, so the addend
                              gains this piece's offset. *)
                           let extra =
                             if sym.sytype = stt_section then
                               (match Hashtbl.find_opt st.places
                                        (p.pobj.file, p.pobj.symbols.(r.sym).shndx) with
                                | Some at -> at
                                | None -> 0)
                             else 0 in
                           out := (p.poffset + r.offset, sym, r.rtype, r.addend + extra) :: !out)
                     entries)) (List.rev o.opieces);
      (o, List.rev !out)) sections in
  let relocs = List.filter (fun (_, l) -> l <> []) relocs in
  (* section numbering for the .rela sections and the tables *)
  let n_carried = List.length sections in
  let rela_index = ref (n_carried + 1) in
  let rela_shndx = List.map (fun (o, l) -> let i = !rela_index in incr rela_index; (o, l, i)) relocs in
  let symtab_shndx = !rela_index in
  let strtab_shndx = symtab_shndx + 1 in
  let shstrtab_shndx = strtab_shndx + 1 in
  let shnum = shstrtab_shndx + 1 in
  (* section bodies *)
  let body_of o =
    if o.otype = sht_nobits then ""
    else begin
      let b = Bytes.make o.osize '\000' in
      List.iter (fun p ->
          let s = p.pobj.sections.(p.pindex).body in
          if s <> "" then Bytes.blit_string s 0 b p.poffset (String.length s)) o.opieces;
      Bytes.to_string b
    end in
  let strings = strtab () in
  let shstrings = strtab () in
  (* the symbol table, ELF64: 24 bytes an entry *)
  let symtab = Buffer.create ((List.length table + 1) * 24) in
  let put_sym name info other shndx value size =
    u32 symtab (intern strings name);
    Buffer.add_char symtab (Char.chr info);
    Buffer.add_char symtab (Char.chr other);
    u16 symtab shndx;
    u64 symtab value;
    u64 symtab size in
  put_sym "" 0 0 0 0 0;
  List.iter (fun s ->
      put_sym s.syname ((s.sybind lsl 4) lor (s.sytype land 15)) s.syother
        s.syshndx s.syvalue s.sysize) table;
  let n_local = List.length all_locals + 1 in
  (* lay the file out: header, bodies, tables, section headers *)
  let out = Buffer.create (1 lsl 20) in
  let ehsize = 64 and shentsize = 64 in
  let offsets = Hashtbl.create 32 in
  Buffer.add_string out (String.make ehsize '\000');
  let place name data align =
    let pad = align_up (Buffer.length out) (max 1 align) - Buffer.length out in
    Buffer.add_string out (String.make pad '\000');
    Hashtbl.replace offsets name (Buffer.length out);
    Buffer.add_string out data in
  List.iter (fun o ->
      if o.otype = sht_nobits then Hashtbl.replace offsets o.oname (Buffer.length out)
      else place o.oname (body_of o) o.oalign) sections;
  List.iter (fun (o, l, _) ->
      let b = Buffer.create (List.length l * 24) in
      List.iter (fun (offset, sym, rtype, addend) ->
          u64 b offset;
          u32 b rtype;
          u32 b sym.syindex;
          u64 b addend) l;
      place (".rela" ^ o.oname) (Buffer.contents b) 8) rela_shndx;
  place ".symtab" (Buffer.contents symtab) 8;
  place ".strtab" (Buffer.contents strings.sbuf) 1;
  (* the section header names go in last, once every name is known *)
  ignore (intern shstrings "");
  List.iter (fun o -> ignore (intern shstrings o.oname)) sections;
  List.iter (fun (o, _, _) -> ignore (intern shstrings (".rela" ^ o.oname))) rela_shndx;
  List.iter (fun n -> ignore (intern shstrings n)) [ ".symtab"; ".strtab"; ".shstrtab" ];
  place ".shstrtab" (Buffer.contents shstrings.sbuf) 1;
  let shoff = align_up (Buffer.length out) 8 in
  Buffer.add_string out (String.make (shoff - Buffer.length out) '\000');
  let sh = Buffer.create (shnum * shentsize) in
  let put_sh name typ flags addr off size link info align entsize =
    u32 sh (intern shstrings name);
    u32 sh typ;
    u64 sh flags;
    u64 sh addr;
    u64 sh off;
    u64 sh size;
    u32 sh link;
    u32 sh info;
    u64 sh align;
    u64 sh entsize in
  put_sh "" sht_null 0 0 0 0 0 0 0 0;
  List.iter (fun o ->
      put_sh o.oname o.otype o.oflags 0 (Hashtbl.find offsets o.oname)
        o.osize 0 0 (max 1 o.oalign) o.oentsize) sections;
  List.iter (fun (o, l, _) ->
      put_sh (".rela" ^ o.oname) sht_rela shf_info_link 0
        (Hashtbl.find offsets (".rela" ^ o.oname)) (List.length l * 24)
        symtab_shndx o.oshndx 8 24) rela_shndx;
  put_sh ".symtab" sht_symtab 0 0 (Hashtbl.find offsets ".symtab")
    (Buffer.length symtab) strtab_shndx n_local 8 24;
  put_sh ".strtab" sht_strtab 0 0 (Hashtbl.find offsets ".strtab")
    (Buffer.length strings.sbuf) 0 0 1 0;
  put_sh ".shstrtab" sht_strtab 0 0 (Hashtbl.find offsets ".shstrtab")
    (Buffer.length shstrings.sbuf) 0 0 1 0;
  Buffer.add_string out (Buffer.contents sh);
  (* the ELF header, now that the section headers are placed *)
  let h = Buffer.create ehsize in
  Buffer.add_string h "\x7fELF\002\001\001\000";
  Buffer.add_string h (String.make 8 '\000');
  u16 h et_rel;
  u16 h 62;                       (* EM_X86_64 *)
  u32 h 1;                        (* EV_CURRENT *)
  u64 h 0;                        (* no entry point *)
  u64 h 0;                        (* no program headers *)
  u64 h shoff;
  u32 h 0;                        (* flags *)
  u16 h ehsize;
  u16 h 0; u16 h 0;               (* no program header table *)
  u16 h shentsize;
  u16 h shnum;
  u16 h shstrtab_shndx;
  let text = Buffer.contents out in
  let text = Buffer.contents h ^ String.sub text ehsize (String.length text - ehsize) in
  let oc = open_out_bin output in
  output_string oc text;
  close_out oc
