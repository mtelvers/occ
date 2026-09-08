(* A static linker for x86-64 ELF (ELF specification 1.2, "Linking view"
   and "Execution view"; System V x86-64 ABI supplement chapter 4,
   "Relocation" and chapter 5, "Program loading").

   The linker takes relocatable objects and archives and produces a
   statically linked executable, in these steps:

   1. Loading.  Objects are read; archive members are pulled in while they
      define symbols that are still undefined, until nothing changes.
      COMDAT groups keep their first copy.
   2. Symbol resolution.  One global table: a strong definition beats a
      weak one, a definition beats a common symbol, the larger common
      symbol wins.  Unresolved weak references become zero.
   3. Section placement.  Input sections are grouped into output sections
      by name (.text.* into .text and so on), output sections into three
      loadable segments by permission, and addresses are assigned.  GOT,
      PLT and TLS bookkeeping is sized here from a scan of the relocations.
   4. Relocation.  Every relocation is computed from the ABI's formulas and
      patched into the output image; TLS general-dynamic sequences are
      rewritten to their local-exec form, and IFUNC symbols get PLT
      entries with IRELATIVE relocations for the C library to resolve.
   5. Output.  ELF header, program headers, sections, and a symbol table
      for debuggers. *)

open Elf_in

let error fmt = Printf.ksprintf failwith fmt

(* ELF constants used here *)
let sht_progbits = 1 and sht_nobits = 8 and sht_rela = 4 and sht_note = 7
let shf_write = 1 and shf_alloc = 2 and shf_execinstr = 4 and shf_tls = 0x400
let stb_local = 0 and stb_global = 1 and stb_weak = 2
let stt_notype = 0 and stt_object = 1 and stt_func = 2 and stt_section = 3 and stt_file = 4 and stt_tls = 6 and stt_gnu_ifunc = 10
let shn_undef = 0 and shn_abs = 0xfff1 and shn_common = 0xfff2

let base_address = 0x400000
let page = 0x1000

(* ---- Inputs and symbols ------------------------------------------------------ *)

type input = {
  obj : Elf_in.t;
  place : (osec * int) option array;   (* per section: output section and offset, None if dropped *)
}

and osec = {
  oname : string;
  mutable oflags : int;
  mutable otype : int;
  mutable oalign : int;
  mutable pieces : (input * int) list;   (* input and section index, reversed while collecting *)
  mutable osize : int;
  mutable addr : int;
  mutable fileoff : int;
  mutable body : Bytes.t;
  mutable shndx : int;                   (* in the output section header table *)
}

type defn =
  | Undefined
  | Defined of input * int * int          (* object, section index, value within it *)
  | Absolute of int
  | Common of int * int                   (* size, alignment *)
  | Synthetic of (unit -> int)            (* linker-defined, known after layout *)

type gsym = {
  name : string;
  mutable defn : defn;
  mutable weak : bool;                    (* the definition is weak, or every reference so far was *)
  mutable ifunc : bool;
  mutable referenced : bool;
  mutable plt : int option;               (* index of its .iplt entry, for IFUNC symbols *)
  mutable common_at : int;                (* offset in .bss once allocated *)
}

(* what a GOT slot holds: the address of a global, or of a local symbol
   of one object (section index and value), or the thread-pointer offset
   of either *)
type got_key = Global of string * bool | Local of string * int * int * bool

type state = {
  symbols : (string, gsym) Hashtbl.t;
  mutable order : gsym list;              (* reversed creation order *)
  mutable inputs : input list;            (* reversed *)
  comdat : (string, unit) Hashtbl.t;      (* signatures already kept *)
  sections : (string, osec) Hashtbl.t;
  mutable section_order : osec list;      (* reversed *)
  got : (got_key, int) Hashtbl.t;         (* slot index per key; each slot is 8 bytes *)
  mutable got_slots : (got_key * bool) list;   (* reversed; true for a thread-pointer offset slot *)
  mutable iplt : gsym list;               (* reversed *)
  mutable n_got : int;
  mutable n_iplt : int;
  mutable tls_end : int;                  (* the thread pointer's position relative to the TLS block *)
}

let gsym st name =
  match Hashtbl.find_opt st.symbols name with
  | Some s -> s
  | None ->
      let s = { name; defn = Undefined; weak = true; ifunc = false; referenced = false; plt = None; common_at = 0 } in
      Hashtbl.replace st.symbols name s;
      st.order <- s :: st.order;
      s

let is_defined s = match s.defn with Undefined -> false | _ -> true

(* ---- Output section naming ------------------------------------------------ *)

let starts_with p s = String.length s >= String.length p && String.sub s 0 (String.length p) = p

(* Where an input section goes: Some name, or None to drop it. *)
let output_name (sec : section) =
  let n = sec.name in
  let alloc = sec.flags land shf_alloc <> 0 in
  if sec.typ = sht_rela || sec.typ = 2 || sec.typ = 3 || sec.typ = 17 || sec.typ = sht_note then None
  else if starts_with ".text" n then Some ".text"
  else if starts_with ".rodata" n || starts_with "rodata" n then Some ".rodata"
  else if starts_with ".data.rel.ro" n then Some ".data.rel.ro"
  else if starts_with ".data" n then Some ".data"
  else if starts_with ".bss" n then Some ".bss"
  else if starts_with ".tdata" n then Some ".tdata"
  else if starts_with ".tbss" n then Some ".tbss"
  else if starts_with ".init_array" n then Some ".init_array"
  else if starts_with ".fini_array" n then Some ".fini_array"
  else if starts_with ".preinit_array" n then Some ".preinit_array"
  else if starts_with ".gcc_except_table" n then Some ".gcc_except_table"
  else if starts_with ".debug_" n then Some n
  else if n = ".comment" || starts_with ".gnu.warning" n || starts_with ".gnu.lto" n || n = ".gnu_debuglink" then None
  else if alloc then Some n
  else None

(* the priority of a .init_array.N section: lower runs first; plain sections last *)
let init_priority (name : string) =
  match String.rindex_opt name '.' with
  | Some i when i > 0 && String.length name > i + 1 ->
      (match int_of_string_opt (String.sub name (i + 1) (String.length name - i - 1)) with Some n -> n | None -> 65536)
  | _ -> 65536

let osec st name =
  match Hashtbl.find_opt st.sections name with
  | Some o -> o
  | None ->
      let o = { oname = name; oflags = 0; otype = sht_progbits; oalign = 1; pieces = []; osize = 0; addr = 0;
                fileoff = 0; body = Bytes.empty; shndx = 0 } in
      Hashtbl.replace st.sections name o;
      st.section_order <- o :: st.section_order;
      o

(* ---- Step 1: loading ----------------------------------------------------------- *)

let add_object st (obj : Elf_in.t) =
  let inp = { obj; place = Array.make (Array.length obj.sections) None } in
  (* COMDAT: sections of a group whose signature was already seen are dropped *)
  let dropped = Hashtbl.create 8 in
  List.iter (fun (sig_, members) ->
      if Hashtbl.mem st.comdat sig_ then List.iter (fun m -> Hashtbl.replace dropped m ()) members
      else Hashtbl.replace st.comdat sig_ ()) obj.groups;
  Array.iter (fun (sec : section) ->
      if not (Hashtbl.mem dropped sec.index) then
        match output_name sec with
        | None -> ()
        | Some name ->
            let o = osec st name in
            o.oflags <- o.oflags lor (sec.flags land (shf_alloc lor shf_write lor shf_execinstr lor shf_tls));
            if sec.typ = sht_nobits && (o.pieces = [] || o.otype = sht_nobits) then o.otype <- sht_nobits
            else o.otype <- sht_progbits;
            o.oalign <- max o.oalign sec.addralign;
            o.pieces <- (inp, sec.index) :: o.pieces;
            inp.place.(sec.index) <- Some (o, 0)) obj.sections;
  (* symbols *)
  Array.iter (fun (sy : symbol) ->
      if sy.bind <> stb_local && sy.stype <> stt_section && sy.stype <> stt_file && sy.sname <> "" then begin
        let g = gsym st sy.sname in
        let weak = sy.bind = stb_weak in
        if sy.shndx = shn_undef then begin
          g.referenced <- true;
          if not weak && not (is_defined g) then g.weak <- false
        end else if sy.shndx = shn_common then begin
          (match g.defn with
           | Undefined -> g.defn <- Common (sy.ssize, max 1 sy.value); g.weak <- weak
           | Common (size, align) -> g.defn <- Common (max size sy.ssize, max align (max 1 sy.value))
           | _ -> ())
        end else if sy.shndx <> shn_abs && Hashtbl.mem dropped sy.shndx then ()   (* a discarded COMDAT copy *)
        else begin
          let d = if sy.shndx = shn_abs then Absolute sy.value else Defined (inp, sy.shndx, sy.value) in
          match g.defn with
          | Undefined | Common _ -> g.defn <- d; g.weak <- weak; g.ifunc <- (sy.stype = stt_gnu_ifunc)
          | Synthetic _ -> ()
          | _ when g.weak && not weak -> g.defn <- d; g.weak <- false; g.ifunc <- (sy.stype = stt_gnu_ifunc)
          | _ when weak -> ()
          | _ -> error "%s: duplicate definition of %s" obj.file sy.sname
        end
      end) obj.symbols;
  st.inputs <- inp :: st.inputs

type item = Object of string | Archive of string | Library of string

(* remove /* ... */ comments from a linker script *)
let strip_comments text =
  let b = Buffer.create (String.length text) in
  let n = String.length text in
  let rec go i =
    if i >= n then ()
    else if i + 1 < n && text.[i] = '/' && text.[i + 1] = '*' then begin
      let rec close j = if j + 1 >= n then n else if text.[j] = '*' && text.[j + 1] = '/' then j + 2 else close (j + 1) in
      go (close (i + 2))
    end else begin Buffer.add_char b text.[i]; go (i + 1) end in
  go 0;
  Buffer.contents b

let read_file f = In_channel.with_open_bin f In_channel.input_all

let find_library search name =
  let candidates = List.map (fun d -> Filename.concat d ("lib" ^ name ^ ".a")) search in
  match List.find_opt Sys.file_exists candidates with
  | Some f -> f
  | None -> error "cannot find -l%s (searched %s)" name (String.concat ", " search)

(* an archive's members with the symbols each defines, read once *)
type archive = { aname : string; members : (Ar.member * string list) array; loaded : bool array }

(* Some "libraries" are tiny GNU ld scripts naming the real files, such as
   Ubuntu's libm.a: GROUP ( libm-2.39.a libmvec.a ).  Only GROUP and
   INPUT lists of file names and -l options are understood. *)
let script_items text =
  let text = strip_comments text in
  let words = String.split_on_char ' ' (String.map (fun c -> if c = '\n' || c = '\t' || c = '(' || c = ')' then ' ' else c) text) in
  let rec go acc = function
    | [] -> List.rev acc
    | ("GROUP" | "INPUT" | "AS_NEEDED" | "") :: rest -> go acc rest
    | ("OUTPUT_FORMAT" | "TARGET") :: _ :: rest -> go acc rest
    | w :: rest when String.length w > 2 && String.sub w 0 2 = "-l" -> go (Library (String.sub w 2 (String.length w - 2)) :: acc) rest
    | w :: rest -> go (Archive w :: acc) rest in
  go [] words

let load st ~search items =
  let archives = ref [] in
  let rec add item =
    match item with
    | Object f -> add_object st (Elf_in.read f (read_file f))
    | Archive f | Library f ->
        let f = match item with Library name -> find_library search name | _ -> f in
        let text = read_file f in
        if starts_with "!<arch>\n" text then begin
          let members = Array.of_list (List.map (fun (m : Ar.member) -> m, Elf_read.exported_symbols m.body) (Ar.read text)) in
          archives := { aname = f; members; loaded = Array.make (Array.length members) false } :: !archives
        end else if Elf_in.is_object text then add_object st (Elf_in.read f text)
        else List.iter add (script_items text) in
  List.iter add items;
  let archives = List.rev !archives in
  (* pull in members while some undefined, non-weak symbol is defined by one *)
  let needed name = match Hashtbl.find_opt st.symbols name with
    | Some g -> g.referenced && not (is_defined g) && not g.weak
    | None -> false in
  let changed = ref true in
  while !changed do
    changed := false;
    List.iter (fun a ->
        Array.iteri (fun i (m, syms) ->
            if not a.loaded.(i) && List.exists needed syms then begin
              a.loaded.(i) <- true;
              changed := true;
              add_object st (Elf_in.read (Printf.sprintf "%s(%s)" a.aname m.Ar.name) m.Ar.body)
            end) a.members) archives
  done

(* ---- Step 3: placement ---------------------------------------------------------- *)

let align_up n a = if a <= 1 then n else (n + a - 1) / a * a

(* the order of output sections; orphans go after the listed ones of the same segment *)
let r_order = [ ".rela.iplt"; ".rodata"; ".eh_frame"; ".gcc_except_table" ]
let x_order = [ ".init"; ".iplt"; ".text"; ".fini" ]
let w_order = [ ".tdata"; ".tbss"; ".preinit_array"; ".init_array"; ".fini_array"; ".data.rel.ro"; ".got"; ".got.plt"; ".data" ]

let segment_of (o : osec) =
  if o.oflags land shf_alloc = 0 then `Other
  else if o.oflags land shf_execinstr <> 0 then `X
  else if o.oflags land shf_write <> 0 then `W
  else `R

let ordered st =
  let all = List.rev st.section_order in
  let named names seg =
    let listed = List.filter_map (fun n -> Hashtbl.find_opt st.sections n) names in
    let orphans = List.filter (fun o -> segment_of o = seg && not (List.mem o.oname names) && o.oname <> ".bss") all in
    listed @ orphans in
  let bss = Option.to_list (Hashtbl.find_opt st.sections ".bss") in
  named r_order `R, named x_order `X, named w_order `W @ bss, List.filter (fun o -> segment_of o = `Other) all

(* offsets of the pieces within their output section, and the section's size *)
let layout_section (o : osec) =
  let pieces = List.rev o.pieces in
  let pieces =
    if o.oname = ".init_array" || o.oname = ".fini_array" || o.oname = ".preinit_array" then
      List.stable_sort (fun (a, i) (b, j) -> compare (init_priority a.obj.sections.(i).name) (init_priority b.obj.sections.(j).name)) pieces
    else pieces in
  let off = ref 0 in
  List.iter (fun (inp, i) ->
      let sec = inp.obj.sections.(i) in
      off := align_up !off sec.addralign;
      inp.place.(i) <- Some (o, !off);
      off := !off + (if sec.typ = sht_nobits then sec.size else String.length sec.body)) pieces;
  o.osize <- !off

(* ---- Symbol values ---------------------------------------------------------------- *)

let section_address (o : osec) = if o.oflags land shf_alloc <> 0 then o.addr else 0

let rec value_of st (g : gsym) =
  match g.defn with
  | Undefined -> if g.weak then 0 else error "undefined symbol %s" g.name
  | Absolute v -> v
  | Common _ -> section_address (osec st ".bss") + g.common_at
  | Synthetic f -> f ()
  | Defined (inp, sec, v) ->
      (match inp.place.(sec) with
       | Some (o, off) -> section_address o + off + v
       | None -> error "%s: symbol %s in a discarded section" inp.obj.file g.name)

(* the address a reference to [g] denotes: for an IFUNC, its PLT entry *)
and address_of st (g : gsym) =
  match g.plt with
  | Some k when g.ifunc -> (osec st ".iplt").addr + 16 * k
  | _ -> value_of st g

(* a symbol of an input object, local or global *)
let symbol_value st (inp : input) (sy : symbol) =
  if sy.bind = stb_local || sy.stype = stt_section then begin
    if sy.shndx = shn_abs then sy.value
    else if sy.shndx = shn_undef then 0
    else match inp.place.(sy.shndx) with
      | Some (o, off) -> section_address o + off + sy.value
      | None -> error "%s: reference to a discarded section" inp.obj.file
  end else address_of st (gsym st sy.sname)

let tp_offset st v = v - st.tls_end

(* ---- Step 3 continued: GOT, PLT and addresses --------------------------------------- *)

let r_x86_64_64 = 1 and r_x86_64_pc32 = 2 and r_x86_64_plt32 = 4 and r_x86_64_gotpcrel = 9 and r_x86_64_32 = 10
and r_x86_64_32s = 11 and r_x86_64_16 = 12 and r_x86_64_pc16 = 13 and r_x86_64_8 = 14 and r_x86_64_pc8 = 15
and r_x86_64_tlsgd = 19 and r_x86_64_tlsld = 20 and r_x86_64_dtpoff32 = 21 and r_x86_64_gottpoff = 22
and r_x86_64_tpoff32 = 23 and r_x86_64_pc64 = 24 and r_x86_64_size32 = 32 and r_x86_64_size64 = 33
and r_x86_64_irelative = 37 and r_x86_64_gotpcrelx = 41 and r_x86_64_rex_gotpcrelx = 42

(* the GOT slot for a relocation's symbol: [tls] for a thread-pointer offset slot *)
let got_key (inp : input) (sy : symbol) tls =
  if sy.bind <> stb_local && sy.stype <> stt_section && sy.sname <> "" then Global (sy.sname, tls)
  else Local (inp.obj.file, sy.shndx, sy.value, tls)

let got_slot st key =
  match Hashtbl.find_opt st.got key with
  | Some k -> k
  | None ->
      let k = st.n_got in
      Hashtbl.replace st.got key k; st.n_got <- k + 1;
      st.got_slots <- (key, (match key with Global (_, t) | Local (_, _, _, t) -> t)) :: st.got_slots;
      k

let plt_entry st g =
  match g.plt with
  | Some k -> k
  | None -> let k = st.n_iplt in g.plt <- Some k; st.n_iplt <- k + 1; st.iplt <- g :: st.iplt; k

(* Scan relocations for the GOT and PLT entries they need, so those
   sections can be sized before addresses are assigned. *)
let scan_relocs st =
  List.iter (fun inp ->
      List.iter (fun (target, rels) ->
          if inp.place.(target) <> None then
            Array.iter (fun (r : reloc) ->
                let sy = inp.obj.symbols.(r.sym) in
                let global = sy.bind <> stb_local && sy.stype <> stt_section && sy.sname <> "" in
                let ifunc = global && (gsym st sy.sname).ifunc in
                if ifunc then ignore (plt_entry st (gsym st sy.sname));
                if r.rtype = r_x86_64_gotpcrel || r.rtype = r_x86_64_gotpcrelx || r.rtype = r_x86_64_rex_gotpcrelx then
                  (if not ifunc then ignore (got_slot st (got_key inp sy false)))
                else if r.rtype = r_x86_64_gottpoff then ignore (got_slot st (got_key inp sy true))) rels) inp.obj.relocs) (List.rev st.inputs)

(* Synthetic sections: .got, .got.plt, .iplt and .rela.iplt, sized from the scan. *)
let synthesize st =
  let make name flags typ align size =
    if size > 0 then begin
      let o = osec st name in
      o.oflags <- flags; o.otype <- typ; o.oalign <- align; o.osize <- size
    end in
  make ".got" (shf_alloc lor shf_write) sht_progbits 8 (8 * st.n_got);
  make ".got.plt" (shf_alloc lor shf_write) sht_progbits 8 (8 * st.n_iplt);
  make ".iplt" (shf_alloc lor shf_execinstr) sht_progbits 16 (16 * st.n_iplt);
  make ".rela.iplt" shf_alloc sht_rela 8 (24 * st.n_iplt)

(* Common symbols get space at the end of .bss. *)
let allocate_commons st =
  let commons = List.filter (fun g -> match g.defn with Common _ -> true | _ -> false) (List.rev st.order) in
  if commons <> [] then begin
    let bss = osec st ".bss" in
    bss.oflags <- shf_alloc lor shf_write; bss.otype <- sht_nobits;
    let off = ref bss.osize in
    List.iter (fun g ->
        match g.defn with
        | Common (size, align) ->
            off := align_up !off align; g.common_at <- !off; off := !off + size;
            bss.oalign <- max bss.oalign align
        | _ -> ()) commons;
    bss.osize <- !off
  end

(* Addresses and file offsets: three page-aligned segments (read-only,
   executable, writable), each section aligned within its segment; the
   file offset equals the address minus the base, so segments are simple
   to map.  Non-allocated sections follow in the file only. *)
type segment = { flags : int; vaddr : int; off : int; filesz : int; memsz : int }

let assign_addresses st (r, x, w, other) =
  let cursor = ref (64 + 56 * 6) in   (* the ELF header and room for program headers *)
  let segments = ref [] in
  let place flags secs first =
    let secs = List.filter (fun o -> o.osize > 0 || o.oname = ".bss") secs in
    if secs <> [] then begin
      (* the first segment starts at the file's beginning so the ELF and
         program headers are mapped too, as the C library expects *)
      let start = if first then 0 else (cursor := align_up !cursor page; !cursor) in
      let file_end = ref start in
      List.iter (fun o ->
          let a = align_up !cursor o.oalign in
          o.addr <- base_address + a; o.fileoff <- a;
          if o.oname = ".tbss" then ()                         (* occupies TLS space only *)
          else begin
            cursor := a + o.osize;
            if o.otype <> sht_nobits then file_end := !cursor
          end) secs;
      let seg = { flags; vaddr = base_address + start; off = start; filesz = !file_end - start; memsz = !cursor - start } in
      segments := seg :: !segments
    end in
  place 4 r true;                (* PF_R, includes the ELF and program headers *)
  place 5 x false;               (* PF_R | PF_X *)
  place 6 w false;               (* PF_R | PF_W *)
  (* non-allocated sections: file space only *)
  List.iter (fun o ->
      if o.osize > 0 then begin
        let a = align_up !cursor o.oalign in
        o.fileoff <- a; o.addr <- 0;
        cursor := a + o.osize
      end) other;
  (* the thread pointer sits just past the TLS block, rounded to its alignment *)
  (match Hashtbl.find_opt st.sections ".tdata", Hashtbl.find_opt st.sections ".tbss" with
   | None, None -> ()
   | td, tb ->
       let first = match td with Some o when o.osize > 0 -> o | _ -> Option.get tb in
       let align = max (match td with Some o -> o.oalign | None -> 1) (match tb with Some o -> o.oalign | None -> 1) in
       let end_ = match tb with Some o when o.osize > 0 -> o.addr + o.osize | _ -> first.addr + first.osize in
       st.tls_end <- first.addr + align_up (end_ - first.addr) align);
  List.rev !segments, !cursor

(* ---- Step 4: relocation ------------------------------------------------------------ *)

let patch (o : osec) off size v =
  for i = 0 to size - 1 do Bytes.set o.body (off + i) (Char.chr ((v asr (8 * i)) land 0xff)) done

let check_signed32 what v = if v < -0x80000000 || v > 0x7fffffff then error "relocation %s overflows 32 bits (%d)" what v
let check_unsigned32 what v = if v < 0 || v > 0xffffffff then error "relocation %s overflows 32 bits (%d)" what v

let relocate st =
  let got = Hashtbl.find_opt st.sections ".got" in
  let got_addr k = match got with Some o -> o.addr + 8 * k | None -> assert false in
  List.iter (fun inp ->
      List.iter (fun (target, rels) ->
          match inp.place.(target) with
          | None -> ()
          | Some (o, base) when o.otype = sht_nobits -> ignore base
          | Some (o, base) ->
              let skip_next = ref (-1) in
              Array.iter (fun (r : reloc) ->
                  if r.offset = !skip_next then ()
                  else begin
                    let sy = inp.obj.symbols.(r.sym) in
                    let global = sy.bind <> stb_local && sy.stype <> stt_section && sy.sname <> "" in
                    let g = if global then Some (gsym st sy.sname) else None in
                    let s () = symbol_value st inp sy in
                    let p = section_address o + base + r.offset in
                    let where = base + r.offset in
                    let a = r.addend in
                    let name = if global then sy.sname else inp.obj.sections.(sy.shndx).name in
                    let t = r.rtype in
                    if t = r_x86_64_64 then patch o where 8 (s () + a)
                    else if t = r_x86_64_32 then (let v = s () + a in check_unsigned32 name v; patch o where 4 v)
                    else if t = r_x86_64_32s then (let v = s () + a in check_signed32 name v; patch o where 4 v)
                    else if t = r_x86_64_pc32 || t = r_x86_64_plt32 then (let v = s () + a - p in check_signed32 name v; patch o where 4 v)
                    else if t = r_x86_64_pc64 then patch o where 8 (s () + a - p)
                    else if t = r_x86_64_16 then patch o where 2 (s () + a)
                    else if t = r_x86_64_pc16 then patch o where 2 (s () + a - p)
                    else if t = r_x86_64_8 then patch o where 1 (s () + a)
                    else if t = r_x86_64_pc8 then patch o where 1 (s () + a - p)
                    else if t = r_x86_64_gotpcrel || t = r_x86_64_gotpcrelx || t = r_x86_64_rex_gotpcrelx then begin
                      let slot = match g with
                        | Some g when g.ifunc -> (osec st ".got.plt").addr + 8 * Option.get g.plt
                        | _ -> got_addr (Hashtbl.find st.got (got_key inp sy false)) in
                      let v = slot + a - p in check_signed32 name v; patch o where 4 v
                    end
                    else if t = r_x86_64_tpoff32 then (let v = tp_offset st (s ()) + a in check_signed32 name v; patch o where 4 v)
                    else if t = r_x86_64_gottpoff then begin
                      let v = got_addr (Hashtbl.find st.got (got_key inp sy true)) + a - p in check_signed32 name v; patch o where 4 v
                    end
                    else if t = r_x86_64_tlsgd then begin
                      (* general dynamic to local exec (ABI table 4.11):
                           .byte 0x66; leaq x@tlsgd(%rip),%rdi; .word 0x6666; rex64; call __tls_get_addr@PLT
                         becomes
                           movq %fs:0,%rax; leaq x@tpoff(%rax),%rax
                         both 16 bytes starting 4 before the relocated field *)
                      let start = where - 4 in
                      Bytes.blit_string "\x64\x48\x8b\x04\x25\x00\x00\x00\x00\x48\x8d\x80" 0 o.body start 12;
                      let v = tp_offset st (s ()) in check_signed32 name v; patch o (start + 12) 4 v;
                      skip_next := r.offset + 8          (* the call's own relocation *)
                    end
                    else if t = r_x86_64_tlsld then begin
                      (* local dynamic to local exec: leaq x@tlsld(%rip),%rdi; call __tls_get_addr@PLT
                         becomes .word 0x6666; .byte 0x66; movq %fs:0,%rax *)
                      let start = where - 3 in
                      Bytes.blit_string "\x66\x66\x66\x64\x48\x8b\x04\x25\x00\x00\x00\x00" 0 o.body start 12;
                      skip_next := r.offset + 5
                    end
                    else if t = r_x86_64_dtpoff32 then (let v = tp_offset st (s ()) + a in check_signed32 name v; patch o where 4 v)
                    else if t = r_x86_64_size32 || t = r_x86_64_size64 then patch o where (if t = r_x86_64_size32 then 4 else 8) (sy.ssize + a)
                    else error "%s: unsupported relocation type %d against %s" inp.obj.file t name
                  end) rels) inp.obj.relocs) (List.rev st.inputs)

(* GOT contents, PLT stubs and IRELATIVE entries *)
let fill_tables st =
  (match Hashtbl.find_opt st.sections ".got" with
   | Some o ->
       List.iter (fun (key, tls) ->
           let k = Hashtbl.find st.got key in
           let v = match key with
             | Global (name, _) -> let g = gsym st name in if tls then tp_offset st (value_of st g) else address_of st g
             | Local (file, shndx, value, _) ->
                 let inp = List.find (fun (i : input) -> i.obj.file = file) st.inputs in
                 let v = symbol_value st inp { sname = ""; bind = stb_local; stype = stt_notype; other = 0; shndx; value; ssize = 0 } in
                 if tls then tp_offset st v else v in
           patch o (8 * k) 8 v) st.got_slots
   | None -> ());
  match Hashtbl.find_opt st.sections ".iplt", Hashtbl.find_opt st.sections ".got.plt", Hashtbl.find_opt st.sections ".rela.iplt" with
  | Some plt, Some gotplt, Some rela ->
      List.iter (fun g ->
          let k = Option.get g.plt in
          let slot = gotplt.addr + 8 * k in
          (* jmp *slot(%rip), padded to 16 bytes *)
          let entry = 16 * k in
          Bytes.blit_string "\xff\x25" 0 plt.body entry 2;
          patch plt (entry + 2) 4 (slot - (plt.addr + entry + 6));
          Bytes.blit_string "\x0f\x1f\x84\x00\x00\x00\x00\x00\x66\x90" 0 plt.body (entry + 6) 10;
          (* the resolver runs at startup and its result goes in the slot *)
          patch rela (24 * k) 8 slot;
          patch rela (24 * k + 8) 8 r_x86_64_irelative;
          patch rela (24 * k + 16) 8 (value_of st g)) st.iplt
  | _ -> ()

(* ---- Linker-defined symbols ------------------------------------------------------ *)

let define_synthetics st (segments : segment list) =
  let define name f =
    let g = gsym st name in
    match g.defn with
    | Undefined -> g.defn <- Synthetic f; g.weak <- false
    | _ -> () in
  let sec name = Hashtbl.find_opt st.sections name in
  let start name = fun () -> match sec name with Some o -> o.addr | None -> 0 in
  let bounds name prefix =
    (* an absent array section gets an empty range at the start of .data *)
    match sec name with
    | Some o when o.osize > 0 -> define (prefix ^ "_start") (fun () -> o.addr); define (prefix ^ "_end") (fun () -> o.addr + o.osize)
    | _ -> define (prefix ^ "_start") (start ".data"); define (prefix ^ "_end") (start ".data") in
  bounds ".init_array" "__init_array"; bounds ".fini_array" "__fini_array"; bounds ".preinit_array" "__preinit_array";
  bounds ".rela.iplt" "__rela_iplt";
  define "__ehdr_start" (fun () -> base_address);
  define "_GLOBAL_OFFSET_TABLE_" (fun () -> match sec ".got.plt" with Some o -> o.addr | None -> start ".got" ());
  let seg_end flags = fun () -> match List.find_opt (fun s -> s.flags = flags) segments with Some s -> s.vaddr + s.memsz | None -> 0 in
  let data_end = fun () -> match List.find_opt (fun s -> s.flags = 6) segments with Some s -> s.vaddr + s.filesz | None -> 0 in
  List.iter (fun n -> define n (seg_end 5)) [ "etext"; "_etext"; "__etext" ];
  List.iter (fun n -> define n data_end) [ "edata"; "_edata" ];
  List.iter (fun n -> define n (seg_end 6)) [ "end"; "_end" ];
  define "__bss_start" (start ".bss");
  define "__executable_start" (fun () -> base_address);
  (* __start_X and __stop_X for every output section X named like a C identifier *)
  Hashtbl.iter (fun name (o : osec) ->
      if o.oflags land shf_alloc <> 0 && name <> "" && name.[0] <> '.' then begin
        define ("__start_" ^ name) (fun () -> o.addr);
        define ("__stop_" ^ name) (fun () -> o.addr + o.osize)
      end) st.sections

(* ---- Step 5: output -------------------------------------------------------------- *)

let u16 b v = Buffer.add_char b (Char.chr (v land 0xff)); Buffer.add_char b (Char.chr ((v lsr 8) land 0xff))
let u32 b v = u16 b (v land 0xffff); u16 b ((v lsr 16) land 0xffff)
let u64 b v = u32 b (v land 0xffffffff); u32 b ((v lsr 32) land 0xffffffff)

let string_table names =
  let b = Buffer.create 256 in
  Buffer.add_char b '\000';
  let offs = List.map (fun n -> if n = "" then 0 else begin let o = Buffer.length b in Buffer.add_string b n; Buffer.add_char b '\000'; o end) names in
  Buffer.contents b, offs

let link ~output ~entry ~search items =
  let st = { symbols = Hashtbl.create 4096; order = []; inputs = []; comdat = Hashtbl.create 64; sections = Hashtbl.create 32;
             section_order = []; got = Hashtbl.create 256; got_slots = []; iplt = []; n_got = 0; n_iplt = 0; tls_end = 0 } in
  (* 1, 2 *)
  load st ~search items;
  (gsym st entry).referenced <- true;
  (* 3 *)
  List.iter layout_section (List.rev st.section_order);
  allocate_commons st;
  scan_relocs st;
  synthesize st;
  let r, x, w, other = ordered st in
  let segments, file_size = assign_addresses st (r, x, w, other) in
  define_synthetics st segments;
  let undefined = List.filter (fun g -> g.referenced && not (is_defined g) && not g.weak) (List.rev st.order) in
  if undefined <> [] then error "undefined symbols: %s" (String.concat ", " (List.map (fun g -> g.name) undefined));
  (* section bodies from their pieces *)
  let all = r @ x @ w @ other in
  List.iter (fun (o : osec) ->
      if o.otype <> sht_nobits then begin
        o.body <- Bytes.make o.osize '\000';
        List.iter (fun (inp, i) ->
            match inp.place.(i) with
            | Some (_, off) -> let s = inp.obj.sections.(i).body in Bytes.blit_string s 0 o.body off (String.length s)
            | None -> ()) o.pieces
      end) all;
  (* 4 *)
  fill_tables st;
  relocate st;
  (* symbol table for debuggers: globals, and the locals worth naming *)
  let syms = ref [] in
  let add name info shndx value size = syms := (name, info, shndx, value, size) :: !syms in
  let placed = List.filter (fun (o : osec) -> o.osize > 0 || o.oname = ".bss") all in
  List.iteri (fun i (o : osec) -> o.shndx <- i + 1) placed;
  let shndx_of_value (g : gsym) = match g.defn with
    | Defined (inp, sec, _) -> (match inp.place.(sec) with Some (o, _) -> o.shndx | None -> 0)
    | Common _ -> (osec st ".bss").shndx
    | Absolute _ | Synthetic _ -> shn_abs
    | Undefined -> shn_undef in
  List.iter (fun inp ->
      Array.iter (fun (sy : symbol) ->
          if sy.bind = stb_local && (sy.stype = stt_func || sy.stype = stt_object) && sy.shndx <> shn_undef && sy.shndx < 0xff00
             && not (starts_with ".L" sy.sname) then
            match inp.place.(sy.shndx) with
            | Some (o, off) -> add sy.sname ((stb_local lsl 4) lor sy.stype) o.shndx (section_address o + off + sy.value) sy.ssize
            | None -> ()) inp.obj.symbols) (List.rev st.inputs);
  let n_locals = 1 + List.length !syms in
  List.iter (fun (g : gsym) ->
      if is_defined g || g.referenced then begin
        (* type and size come from the defining symbol, which debuggers use
           to choose among aliases of one address *)
        let defining = match g.defn with
          | Defined (inp, _, _) -> Array.find_opt (fun (sy : symbol) -> sy.sname = g.name && sy.shndx <> shn_undef) inp.obj.symbols
          | _ -> None in
        let typ = if g.ifunc then stt_func else match g.defn, defining with
          | Defined _, Some sy -> (if sy.stype = stt_tls then stt_tls else if sy.stype = stt_gnu_ifunc then stt_func else sy.stype)
          | Common (_, _), _ -> stt_object
          | _ -> stt_notype in
        let size = match g.defn, defining with Defined _, Some sy -> sy.ssize | Common (n, _), _ -> n | _ -> 0 in
        let value = if is_defined g then (if typ = stt_tls then value_of st g - (match Hashtbl.find_opt st.sections ".tdata" with Some o when o.osize > 0 -> o.addr | _ -> (osec st ".tbss").addr) else address_of st g) else 0 in
        add g.name (((if g.weak && is_defined g then stb_weak else stb_global) lsl 4) lor typ) (shndx_of_value g) value size
      end) (List.rev st.order);
  let syms = List.rev !syms in
  let strtab, offs = string_table (List.map (fun (n, _, _, _, _) -> n) syms) in
  let symtab = Buffer.create (24 * (List.length syms + 1)) in
  Buffer.add_string symtab (String.make 24 '\000');
  List.iter2 (fun (_, info, shndx, value, size) off -> u32 symtab off; Buffer.add_char symtab (Char.chr info); Buffer.add_char symtab '\000'; u16 symtab shndx; u64 symtab value; u64 symtab size) syms offs;
  (* the file: headers, segments, non-allocated sections, then the section table *)
  let out = Buffer.create (file_size + 65536) in
  let phnum = List.length segments + (if st.tls_end <> 0 then 1 else 0) + 1 in
  let shstr_names = List.map (fun o -> o.oname) placed @ [ ".symtab"; ".strtab"; ".shstrtab" ] in
  let shstrtab, shstr_offs = string_table shstr_names in
  Buffer.add_string out "\x7fELF\x02\x01\x01\x00"; Buffer.add_string out (String.make 8 '\000');
  u16 out 2; u16 out 62; u32 out 1;                          (* ET_EXEC, x86-64 *)
  u64 out (address_of st (gsym st entry));
  u64 out 64;                                                (* e_phoff *)
  let shoff_pos = Buffer.length out in u64 out 0;
  u32 out 0; u16 out 64; u16 out 56; u16 out phnum; u16 out 64;
  u16 out (List.length placed + 4); u16 out (List.length placed + 3);
  (* program headers *)
  let phdr typ flags off vaddr filesz memsz align =
    u32 out typ; u32 out flags; u64 out off; u64 out vaddr; u64 out vaddr; u64 out filesz; u64 out memsz; u64 out align in
  List.iter (fun s -> phdr 1 s.flags s.off s.vaddr s.filesz s.memsz page) segments;
  if st.tls_end <> 0 then begin
    let td = Hashtbl.find_opt st.sections ".tdata" and tb = Hashtbl.find_opt st.sections ".tbss" in
    let first = match td with Some o when o.osize > 0 -> o | _ -> Option.get tb in
    let filesz = match td with Some o -> o.osize | None -> 0 in
    let align = max (match td with Some o -> o.oalign | None -> 1) (match tb with Some o -> o.oalign | None -> 1) in
    let end_ = match tb with Some o when o.osize > 0 -> o.addr + o.osize | _ -> first.addr + first.osize in
    phdr 7 4 first.fileoff first.addr filesz (end_ - first.addr) align
  end;
  phdr 0x6474e551 6 0 0 0 0 16;                              (* PT_GNU_STACK: no executable stack *)
  (* section bodies at their offsets *)
  List.iter (fun (o : osec) ->
      if o.otype <> sht_nobits && o.osize > 0 then begin
        while Buffer.length out < o.fileoff do Buffer.add_char out '\000' done;
        Buffer.add_bytes out o.body
      end) all;
  let symtab_off = Buffer.length out in
  Buffer.add_buffer out symtab;
  let strtab_off = Buffer.length out in Buffer.add_string out strtab;
  let shstrtab_off = Buffer.length out in Buffer.add_string out shstrtab;
  while Buffer.length out mod 8 <> 0 do Buffer.add_char out '\000' done;
  let shoff = Buffer.length out in
  let shdr name typ flags addr off size link info align entsize =
    u32 out name; u32 out typ; u64 out flags; u64 out addr; u64 out off; u64 out size; u32 out link; u32 out info; u64 out align; u64 out entsize in
  Buffer.add_string out (String.make 64 '\000');
  List.iter2 (fun (o : osec) name ->
      shdr name o.otype o.oflags o.addr o.fileoff o.osize 0 0 o.oalign (if o.otype = sht_rela then 24 else 0)) placed (List.filteri (fun i _ -> i < List.length placed) shstr_offs);
  let tail = List.filteri (fun i _ -> i >= List.length placed) shstr_offs in
  (match tail with
   | [ n_symtab; n_strtab; n_shstrtab ] ->
       shdr n_symtab 2 0 0 symtab_off (Buffer.length symtab) (List.length placed + 2) n_locals 8 24;
       shdr n_strtab 3 0 0 strtab_off (String.length strtab) 0 0 1 0;
       shdr n_shstrtab 3 0 0 shstrtab_off (String.length shstrtab) 0 0 1 0
   | _ -> assert false);
  let bytes = Buffer.to_bytes out in
  Bytes.set_int64_le bytes shoff_pos (Int64.of_int shoff);
  (* recreate the file so it gets execute permission even if it existed *)
  if Sys.file_exists output then Sys.remove output;
  Out_channel.with_open_gen [ Open_wronly; Open_creat; Open_trunc; Open_binary ] 0o755 output (fun oc -> output_bytes oc bytes)
