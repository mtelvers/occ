(* A static linker for x86-64 ELF (ELF specification 1.2, "Linking view"
   and "Execution view"; System V x86-64 ABI supplement chapter 4,
   "Relocation" and chapter 5, "Program loading").

   The linker takes relocatable objects and archives and produces a
   statically linked executable, or -- with [~shared] -- a shared object
   for a dynamic loader to finish (see dynamic.ml), in these steps:

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
      for debuggers.

   A shared object differs in four ways, and each is marked [shared]
   below: it is ET_DYN and starts at address zero, so the loader may
   put it anywhere; the places holding an address it therefore cannot
   know are listed in .rela.dyn for the loader to fix; the symbols it
   offers are listed again in .dynsym, where the loader looks; and
   .dynamic says where all of that is. *)

open Elf_in

let error fmt = Printf.ksprintf failwith fmt

(* ELF constants used here *)
let sht_progbits = 1 and sht_nobits = 8 and sht_rela = 4 and sht_note = 7
let shf_write = 1 and shf_alloc = 2 and shf_execinstr = 4 and shf_tls = 0x400
let stb_local = 0 and stb_global = 1 and stb_weak = 2
let stt_notype = 0 and stt_object = 1 and stt_func = 2 and stt_section = 3 and stt_file = 4 and stt_tls = 6 and stt_gnu_ifunc = 10
let shn_undef = 0 and shn_abs = 0xfff1 and shn_common = 0xfff2

(* Where the image starts.  An executable is linked at a fixed address;
   a shared object starts at zero, since the loader chooses where to put
   it and adds that to every address in the file. *)
let exec_base = 0x400000

(* the loader an executable that needs one names (x86-64 ABI) *)
let interpreter = "/lib64/ld-linux-x86-64.so.2"
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
  (* [shared] a shared object offers it; this link gives it no address,
     and every reference to it has to go through the GOT or the PLT so
     that the loader can put the address in one place *)
  | Imported of string                    (* the name the loader will be told to load *)
  (* Code that was not compiled to be position-independent refers to a
     variable by its address, and the address of a variable in a shared
     object is not known when this links.  So space is set aside here,
     the loader is asked to copy the variable's value into it, and every
     reference is to this copy: a copy relocation. *)
  | Copy of int * int                     (* offset in .bss, and the size copied *)

type gsym = {
  name : string;
  mutable defn : defn;
  mutable weak : bool;                    (* the definition is weak, or every reference so far was *)
  mutable ifunc : bool;
  mutable referenced : bool;
  mutable plt : int option;               (* index of its .iplt entry, for IFUNC symbols *)
  mutable common_at : int;                (* offset in .bss once allocated *)
  mutable dplt : int option;              (* [shared] index of its .plt entry, for an imported function *)
  mutable dynidx : int;                   (* [shared] its place in .dynsym, 0 if it is not there *)
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
  shared : bool;                          (* producing a shared object *)
  mutable dyn : bool;                     (* the output carries the loader's tables *)
  export_all : bool;                      (* -E: every global goes into .dynsym *)
  prefer_shared : bool;                   (* -lname looks for libname.so first *)
  rpath : string;                         (* -rpath: where to look for the objects needed *)
  base : int;                             (* the address the image starts at: 0 when shared *)
  mutable n_dynrel : int;                 (* relocations the loader will be given *)
  mutable n_relative : int;               (* of those, the ones naming no symbol *)
  mutable dynrels : Dynamic.rel list;     (* reversed; built once addresses are known *)
  mutable exports : gsym list;            (* the symbols .dynsym offers, in order *)
  mutable imports : gsym list;            (* the symbols a shared object will provide *)
  mutable needed : string list;           (* the shared objects to record, in order *)
  mutable dplts : gsym list;              (* reversed: one .plt entry each *)
  mutable n_dplt : int;
  provided : (string, symbol) Hashtbl.t;  (* what the shared objects offer, by name *)
  mutable copies : (gsym * int) list;     (* a variable to copy here, and its size *)
  soname : string;                        (* -soname: the name the loader records *)
}

let gsym st name =
  match Hashtbl.find_opt st.symbols name with
  | Some s -> s
  | None ->
      let s = { name; defn = Undefined; weak = true; ifunc = false; referenced = false; plt = None;
                common_at = 0; dplt = None; dynidx = 0 } in
      Hashtbl.replace st.symbols name s;
      st.order <- s :: st.order;
      s

let is_defined s = match s.defn with Undefined -> false | _ -> true
let is_imported s = match s.defn with Imported _ -> true | _ -> false

(* Whether the loader is the one that will say where this symbol is: a
   shared object provides it, or -- in a shared object, where a name
   nothing here defines is not an error -- nothing does and whatever
   loads it may.  Either way this link cannot have its address, so every
   reference goes through the table or a stub. *)
let from_loader ~shared s = is_imported s || (shared && not (is_defined s))

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

type item = Object of string | Archive of string | Library of string | Shared of string

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

(* -lname.  A link that may use shared objects prefers libname.so, as
   ld does; a static link takes only libname.a. *)
let find_library ?(shared = false) search name =
  let names = if shared then [ "lib" ^ name ^ ".so"; "lib" ^ name ^ ".a" ] else [ "lib" ^ name ^ ".a" ] in
  let candidates = List.concat_map (fun d -> List.map (Filename.concat d) names) search in
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
  let archives = ref [] and shareds = ref [] in
  let rec add item =
    match item with
    | Object f -> add_object st (Elf_in.read f (read_file f))
    | Shared f -> shareds := Elf_in.read_shared f (read_file f) :: !shareds
    | Archive f | Library f ->
        let f = match item with Library name -> find_library ~shared:st.prefer_shared search name | _ -> f in
        let text = read_file f in
        if starts_with "!<arch>\n" text then begin
          let members = Array.of_list (List.map (fun (m : Ar.member) -> m, Elf_read.exported_symbols m.body) (Ar.read text)) in
          archives := { aname = f; members; loaded = Array.make (Array.length members) false } :: !archives
        end else if Elf_in.is_object text then add_object st (Elf_in.read f text)
        else if Elf_in.is_shared text then shareds := Elf_in.read_shared f text :: !shareds
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
  done;
  (* [shared] What the shared objects offer settles the rest.  Order on
     the command line does not matter here as it does for an archive: a
     shared object is not taken apart, so it can answer for a name
     referred to before it was named. *)
  let shareds = List.rev !shareds in
  (* An executable that was linked against a shared object needs the
     loader's tables as much as a shared object does, and a loader to
     read them. *)
  if shareds <> [] then st.dyn <- true;
  st.needed <- List.map (fun (sh : Elf_in.shared) -> sh.soname) shareds;
  List.iter (fun (sh : Elf_in.shared) ->
      Array.iter (fun (sy : Elf_in.symbol) ->
          if sy.sname <> "" && sy.shndx <> shn_undef && sy.bind <> stb_local then
            match Hashtbl.find_opt st.symbols sy.sname with
            | Some g when not (is_defined g) ->
                g.defn <- Imported sh.soname;
                Hashtbl.replace st.provided sy.sname sy
            | _ -> ())
        sh.provides)
    shareds

(* ---- Step 3: placement ---------------------------------------------------------- *)

let align_up n a = if a <= 1 then n else (n + a - 1) / a * a

(* the order of output sections; orphans go after the listed ones of the same segment *)
let r_order = [ ".interp"; ".hash"; ".dynsym"; ".dynstr"; ".rela.dyn"; ".rela.plt"; ".rela.iplt";
                ".rodata"; ".eh_frame"; ".gcc_except_table" ]
let x_order = [ ".init"; ".iplt"; ".text"; ".fini" ]
let w_order = [ ".tdata"; ".tbss"; ".preinit_array"; ".init_array"; ".fini_array";
                ".data.rel.ro"; ".dynamic"; ".got"; ".got.plt"; ".data" ]

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
  (* An imported symbol has no address here by definition: a reference
     to it goes through the GOT or the PLT, and anything else is a
     reference this kind of output cannot make. *)
  | Imported _ ->
      error "%s is defined in a shared object, so it cannot be referred to directly" g.name
  | Absolute v -> v
  | Common _ -> section_address (osec st ".bss") + g.common_at
  | Copy (off, _) -> section_address (osec st ".bss") + off
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

(* where the thread-local block begins, which is what the value of a
   thread-local symbol is measured from in a symbol table *)
let tls_block_start st =
  match Hashtbl.find_opt st.sections ".tdata" with
  | Some o when o.osize > 0 -> o.addr
  | _ -> (osec st ".tbss").addr

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

(* [shared] A call to a function a shared object provides goes through
   the procedure linkage table: the call reaches a stub that jumps to
   whatever the loader put in the stub's slot.  The slots are resolved
   before the object runs (DF_BIND_NOW below), so the stub is one
   instruction and there is no resolver to arrange. *)
let dplt_entry st g =
  match g.dplt with
  | Some k -> k
  | None -> let k = st.n_dplt in g.dplt <- Some k; st.n_dplt <- k + 1; st.dplts <- g :: st.dplts; k

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
                (* [shared] a call to a function from elsewhere needs a
                   stub to call instead *)
                if st.dyn && global
                   && (r.rtype = r_x86_64_plt32 || r.rtype = r_x86_64_pc32)
                   && from_loader ~shared:st.shared (gsym st sy.sname)
                then ignore (dplt_entry st (gsym st sy.sname));
                (* and a variable in another object, referred to by its
                   address rather than through the table, needs a copy
                   of its own here for the loader to fill *)
                if st.dyn && not st.shared && global
                   && (r.rtype = r_x86_64_64 || r.rtype = r_x86_64_32 || r.rtype = r_x86_64_32s
                       || r.rtype = r_x86_64_pc32)
                then begin
                  let g = gsym st sy.sname in
                  if is_imported g then
                    match Hashtbl.find_opt st.provided g.name with
                    | Some (sy' : symbol) when sy'.stype = stt_object && sy'.ssize > 0 ->
                        st.copies <- (g, sy'.ssize) :: List.filter (fun (h, _) -> h != g) st.copies
                    | _ -> ()
                end;
                if r.rtype = r_x86_64_gotpcrel || r.rtype = r_x86_64_gotpcrelx || r.rtype = r_x86_64_rex_gotpcrelx then
                  (if not ifunc then ignore (got_slot st (got_key inp sy false)))
                else if r.rtype = r_x86_64_gottpoff then ignore (got_slot st (got_key inp sy true))
                else if st.shared && r.rtype = r_x86_64_64
                        && inp.obj.sections.(target).flags land shf_alloc <> 0 then
                  (* [shared] an address the loader will have to put in,
                     since this link does not know where the object will
                     be *)
                  st.n_dynrel <- st.n_dynrel + 1) rels) inp.obj.relocs) (List.rev st.inputs);
  (* [shared] and one for each entry of the global offset table that
     the loader has to fill, and one for each variable copied here *)
  if st.shared then st.n_dynrel <- st.n_dynrel + st.n_got
  else if st.dyn then begin
    let slots =
      List.length (List.filter (fun (key, _) ->
          match key with Global (name, false) -> is_imported (gsym st name) | _ -> false) st.got_slots) in
    st.n_dynrel <- st.n_dynrel + slots + List.length st.copies
  end

(* [shared] Which symbols .dynsym offers: every global this object
   defines that is not hidden.  A hidden symbol is one the compiler was
   told no other object would look for, so offering it would be wrong as
   well as wasteful. *)
let stv_hidden = 2 and stv_internal = 1

let visibility (inp : input) name =
  match Array.find_opt (fun (sy : symbol) -> sy.sname = name && sy.shndx <> shn_undef) inp.obj.symbols with
  | Some sy -> sy.other land 3
  | None -> 0

let choose_exports st =
  (* A shared object offers what it defines; an executable offers
     nothing unless it is asked to (-E), which it is when it means to
     load objects that will bind back to it. *)
  st.exports <-
    (if not (st.shared || st.export_all) then []
     else
       List.filter (fun (g : gsym) ->
           match g.defn with
           | Defined (inp, _, _) ->
               let v = visibility inp g.name in
               v <> stv_hidden && v <> stv_internal
           | Copy _ -> true
           | Common _ | Absolute _ -> true
           | Imported _ | Synthetic _ | Undefined -> false)
         (List.rev st.order));
  (* a name it has taken a copy of is offered whether or not the rest
     are, since that copy is the one definition the loader must use *)
  let copies = List.filter (fun (g : gsym) -> match g.defn with Copy _ -> true | _ -> false) (List.rev st.order) in
  List.iter (fun g -> if not (List.memq g st.exports) then st.exports <- st.exports @ [ g ]) copies;
  (* and what it wants from elsewhere: a name a shared object offers, or
     one nothing defines, which in a shared object is not an error --
     whatever loads it may have it *)
  st.imports <-
    List.filter (fun (g : gsym) ->
        g.referenced && (is_imported g || not (is_defined g)))
      (List.rev st.order);
  (* .dynsym is numbered now, because the relocations the loader is
     given name symbols by their place in it *)
  let k = ref 1 in
  List.iter (fun (g : gsym) -> g.dynidx <- !k; incr k) st.exports;
  List.iter (fun (g : gsym) -> g.dynidx <- !k; incr k) st.imports

(* [shared] .dynstr holds every name the loader will look at: the
   symbols, the objects to load, and this object's own name.  The order
   is fixed so that the table built to size it and the table built to
   write it come out the same. *)
let build_strtab st =
  let strings = Dynamic.strtab () in
  List.iter (fun (g : gsym) -> ignore (Dynamic.intern strings g.name)) st.exports;
  List.iter (fun (g : gsym) -> ignore (Dynamic.intern strings g.name)) st.imports;
  List.iter (fun n -> ignore (Dynamic.intern strings n)) st.needed;
  if st.soname <> "" then ignore (Dynamic.intern strings st.soname);
  if st.rpath <> "" then ignore (Dynamic.intern strings st.rpath);
  strings

(* [shared] What .dynamic says.  Called twice: once before addresses are
   known, for the length alone, and once after, for the file.  The
   addresses are wrong the first time and the length is the same both
   times, which is what has to be true. *)
let dynamic_entries st strings =
  let sec name = Hashtbl.find_opt st.sections name in
  let addr name = match sec name with Some o -> o.addr | None -> 0 in
  let size name = match sec name with Some o -> o.osize | None -> 0 in
  let df_bind_now = 8 in
  List.map (fun n -> Dynamic.dt_needed, Dynamic.intern strings n) st.needed
  @ (if st.soname <> "" then [ Dynamic.dt_soname, Dynamic.intern strings st.soname ] else [])
  @ [ Dynamic.dt_hash, addr ".hash";
      Dynamic.dt_strtab, addr ".dynstr";
      Dynamic.dt_symtab, addr ".dynsym";
      Dynamic.dt_strsz, size ".dynstr";
      Dynamic.dt_syment, 24;
      Dynamic.dt_rela, addr ".rela.dyn";
      Dynamic.dt_relasz, size ".rela.dyn";
      Dynamic.dt_relaent, 24;
      Dynamic.dt_relacount, st.n_relative ]
  @ (if size ".plt" > 0
     then [ Dynamic.dt_pltgot, addr ".got.plt";
            Dynamic.dt_pltrelsz, size ".rela.plt";
            Dynamic.dt_pltrel, 7;                 (* DT_RELA *)
            Dynamic.dt_jmprel, addr ".rela.plt";
            (* the stubs are one instruction with no resolver behind
               them, so every slot has to be filled before the object
               runs rather than on first use *)
            Dynamic.dt_flags, df_bind_now;
            24 (* DT_BIND_NOW, for a loader that reads only the old tag *), 0 ]
     else [])
  @ (if size ".init_array" > 0
     then [ Dynamic.dt_init_array, addr ".init_array"; Dynamic.dt_init_arraysz, size ".init_array" ] else [])
  @ (if size ".fini_array" > 0
     then [ Dynamic.dt_fini_array, addr ".fini_array"; Dynamic.dt_fini_arraysz, size ".fini_array" ] else [])
  @ (if st.rpath <> "" then [ Dynamic.dt_runpath, Dynamic.intern strings st.rpath ] else [])

(* Synthetic sections: .got, .got.plt, .iplt and .rela.iplt, sized from the scan. *)
let synthesize st =
  let make name flags typ align size =
    if size > 0 then begin
      let o = osec st name in
      o.oflags <- flags; o.otype <- typ; o.oalign <- align; o.osize <- size
    end in
  make ".got" (shf_alloc lor shf_write) sht_progbits 8 (8 * st.n_got);
  (* [shared] the first three entries of .got.plt are the ones a lazy
     loader would use; nothing here binds lazily, but every linker
     leaves them and DT_PLTGOT points at them *)
  if st.dyn && st.n_iplt > 0 then
    error "an indirect function in an object that a loader finishes is not supported yet";
  make ".got.plt" (shf_alloc lor shf_write) sht_progbits 8
    (if st.dyn then (if st.n_dplt > 0 then 24 + 8 * st.n_dplt else 0) else 8 * st.n_iplt);
  make ".iplt" (shf_alloc lor shf_execinstr) sht_progbits 16 (16 * st.n_iplt);
  make ".rela.iplt" shf_alloc sht_rela 8 (24 * st.n_iplt);
  (* [shared] the stubs for calls out, the slots they jump through, and
     the relocations that tell the loader what to put in those slots *)
  make ".plt" (shf_alloc lor shf_execinstr) sht_progbits 16 (16 * st.n_dplt);
  make ".rela.plt" shf_alloc sht_rela 8 (24 * st.n_dplt);
  (* [shared] the tables a loader reads.  Their sizes are known here --
     how many symbols, how many relocations -- although what goes in
     them is not known until addresses are assigned. *)
  (* [shared] An executable the loader has to finish says which loader:
     this is the one every program on the platform names, and the path
     is part of the ABI rather than a choice. *)
  if st.dyn && not st.shared then begin
    let o = osec st ".interp" in
    o.oflags <- shf_alloc; o.otype <- sht_progbits; o.oalign <- 1;
    o.osize <- String.length interpreter + 1
  end;
  if st.dyn then begin
    let strings = build_strtab st in
    let n = 1 + List.length st.exports + List.length st.imports in
    make ".hash" shf_alloc 5 8 (Dynamic.hash_size n);
    make ".dynsym" shf_alloc 11 8 (24 * n);
    make ".dynstr" shf_alloc 3 1 (String.length (Dynamic.strtab_contents strings));
    make ".rela.dyn" shf_alloc sht_rela 8 (24 * st.n_dynrel);
    make ".dynamic" (shf_alloc lor shf_write) 6 8
      (Dynamic.dynamic_size (List.length (dynamic_entries st strings)))
  end

(* [shared] The variables copied from a shared object get space at the
   end of .bss too, and become definitions here: every reference then
   resolves to this copy, and the loader is asked to put the value in
   it (R_X86_64_COPY below). *)
let allocate_copies st =
  if st.copies <> [] then begin
    let bss = osec st ".bss" in
    bss.oflags <- shf_alloc lor shf_write; bss.otype <- sht_nobits;
    List.iter (fun ((g : gsym), size) ->
        let align = if size >= 16 then 16 else 8 in
        let off = align_up bss.osize align in
        g.defn <- Copy (off, size);
        bss.osize <- off + size;
        bss.oalign <- max bss.oalign align)
      (List.rev st.copies)
  end

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
          o.addr <- st.base + a; o.fileoff <- a;
          if o.oname = ".tbss" then ()                         (* occupies TLS space only *)
          else begin
            cursor := a + o.osize;
            if o.otype <> sht_nobits then file_end := !cursor
          end) secs;
      let seg = { flags; vaddr = st.base + start; off = start; filesz = !file_end - start; memsz = !cursor - start } in
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
                    (* [shared] a reference to something a shared
                       object provides: the call goes to its stub, and
                       an address is left for the loader to put in *)
                    if st.dyn && (match g with Some g -> from_loader ~shared:st.shared g | None -> false) then begin
                      let g = Option.get g in
                      if t = r_x86_64_plt32 || t = r_x86_64_pc32 then begin
                        let stub = (osec st ".plt").addr + 16 * dplt_entry st g in
                        let v = stub + a - p in check_signed32 name v; patch o where 4 v
                      end
                      else if t = r_x86_64_64 then begin
                        patch o where 8 a;
                        st.dynrels <- { Dynamic.where = p; rtype = Dynamic.r_x86_64_64;
                                        rsym = g.dynidx; addend = a } :: st.dynrels
                      end
                      else if t = r_x86_64_gotpcrel || t = r_x86_64_gotpcrelx || t = r_x86_64_rex_gotpcrelx then begin
                        let slot = got_addr (Hashtbl.find st.got (got_key inp sy false)) in
                        let v = slot + a - p in check_signed32 name v; patch o where 4 v
                      end
                      else
                        error "%s: %s is for the loader to find and relocation type %d cannot reach it"
                          inp.obj.file name t
                    end
                    else if t = r_x86_64_64 then begin
                      let v = s () + a in
                      patch o where 8 v;
                      (* [shared] the loader adds where the object went
                         to this address, so the value left here is the
                         addend it works from *)
                      if st.shared && o.oflags land shf_alloc <> 0 then
                        st.dynrels <- { Dynamic.where = p; rtype = Dynamic.r_x86_64_relative;
                                        rsym = 0; addend = v } :: st.dynrels
                    end
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
                    else if (t = r_x86_64_tlsgd || t = r_x86_64_tlsld) && st.shared then
                      error "%s: %s is reached by the general dynamic thread-local sequence, \
                             which a shared object cannot have rewritten; compile it with \
                             -ftls-model=initial-exec" inp.obj.file name
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

(* [shared] the slot the stub for entry [k] jumps through *)
let dplt_slot st k = (osec st ".got.plt").addr + 24 + 8 * k

(* GOT contents, PLT stubs and IRELATIVE entries *)
let fill_tables st =
  (match Hashtbl.find_opt st.sections ".got" with
   | Some o ->
       List.iter (fun (key, tls) ->
           let k = Hashtbl.find st.got key in
           let v = match key with
             (* [shared] a slot for a symbol from elsewhere holds nothing
                until the loader fills it, and neither does one holding
                the position of a thread-local, which depends on where
                the object's block of them ends up *)
             | _ when st.shared && tls -> 0
             | Global (name, _) when from_loader ~shared:st.shared (gsym st name) -> 0
             | Global (name, _) -> let g = gsym st name in if tls then tp_offset st (value_of st g) else address_of st g
             | Local (file, shndx, value, _) ->
                 let inp = List.find (fun (i : input) -> i.obj.file = file) st.inputs in
                 let v = symbol_value st inp { sname = ""; bind = stb_local; stype = stt_notype; other = 0; shndx; value; ssize = 0 } in
                 if tls then tp_offset st v else v in
           patch o (8 * k) 8 v;
           if st.dyn then begin
             (* A slot for a symbol the loader knows by name is left to
                it, so that a definition elsewhere -- another object, or
                one preloaded ahead of this -- is the one used.  In a
                shared object a slot holding an address of its own is
                still the loader's to rebase; in an executable at a
                fixed address it is already right. *)
             let named = match key with
               | Global (name, _) -> let g = gsym st name in if g.dynidx > 0 then Some g else None
               | _ -> None in
             let at = o.addr + 8 * k in
             if tls then begin
               (* Where a thread-local sits is measured from the thread
                  pointer, and only the loader knows that: it is told
                  the symbol if it can be named, and otherwise the
                  position within this object's own block, which is
                  what a thread-local nothing else can see needs. *)
               if st.shared then
                 match named with
                 | Some g ->
                     st.dynrels <- { Dynamic.where = at; rtype = Dynamic.r_x86_64_tpoff64;
                                     rsym = g.dynidx; addend = 0 } :: st.dynrels
                 | None ->
                     let within = match key with
                       | Local (file, shndx, value, _) ->
                           let inp = List.find (fun (i : input) -> i.obj.file = file) st.inputs in
                           symbol_value st inp { sname = ""; bind = stb_local; stype = stt_notype;
                                                 other = 0; shndx; value; ssize = 0 }
                           - tls_block_start st
                       | Global (name, _) -> value_of st (gsym st name) - tls_block_start st in
                     st.dynrels <- { Dynamic.where = at; rtype = Dynamic.r_x86_64_tpoff64;
                                     rsym = 0; addend = within } :: st.dynrels
             end
             else
               match named with
               | Some g when st.shared || is_imported g ->
                   st.dynrels <- { Dynamic.where = at; rtype = Dynamic.r_x86_64_glob_dat;
                                   rsym = g.dynidx; addend = 0 } :: st.dynrels
               | _ ->
                   if st.shared then
                     st.dynrels <- { Dynamic.where = at; rtype = Dynamic.r_x86_64_relative;
                                     rsym = 0; addend = v } :: st.dynrels
           end) st.got_slots
   | None -> ());
  (* [shared] the stubs for calls out: jmp *slot(%rip), and a JUMP_SLOT
     relocation asking the loader to put the function's address there *)
  (match Hashtbl.find_opt st.sections ".plt", Hashtbl.find_opt st.sections ".rela.plt" with
   | Some plt, Some rela when st.n_dplt > 0 ->
       (* the first .got.plt entry holds where .dynamic is, which is
          where a loader looks when it has to find its way back *)
       (match Hashtbl.find_opt st.sections ".got.plt", Hashtbl.find_opt st.sections ".dynamic" with
        | Some gotplt, Some dyn -> patch gotplt 0 8 dyn.addr
        | _ -> ());
       List.iter (fun (g : gsym) ->
           let k = Option.get g.dplt in
           let entry = 16 * k in
           let slot = dplt_slot st k in
           Bytes.blit_string "\xff\x25" 0 plt.body entry 2;
           patch plt (entry + 2) 4 (slot - (plt.addr + entry + 6));
           Bytes.blit_string "\x0f\x1f\x84\x00\x00\x00\x00\x00\x66\x90" 0 plt.body (entry + 6) 10;
           patch rela (24 * k) 8 slot;
           patch rela (24 * k + 8) 8 (Dynamic.r_x86_64_jump_slot lor (g.dynidx lsl 32));
           patch rela (24 * k + 16) 8 0)
         st.dplts
   | _ -> ());
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

(* ---- [shared] the loader's tables ------------------------------------------------ *)

(* Written once every address is known.  The sizes were settled in
   [synthesize], and each of these has to come out exactly that long,
   which is why the counts there and the contents here are the only two
   places that decide what goes in. *)
let fill_dynamic st =
  let sec name = Hashtbl.find_opt st.sections name in
  let put name text =
    match sec name with
    | Some o when o.osize > 0 ->
        if String.length text <> o.osize then
          error "%s: the loader's table came out %d bytes, not the %d reserved"
            name (String.length text) o.osize;
        Bytes.blit_string text 0 o.body 0 (String.length text)
    | _ -> () in
  let strings = build_strtab st in
  (* one .dynsym entry per symbol this object offers, then one per
     symbol it wants, which is what the relocations name *)
  let defining (g : gsym) =
    match g.defn with
    | Defined (inp, _, _) ->
        Array.find_opt (fun (sy : symbol) -> sy.sname = g.name && sy.shndx <> shn_undef) inp.obj.symbols
    | _ -> None in
  let export (g : gsym) =
    let typ = match defining g, g.defn with
      | _, Copy _ -> stt_object          (* a variable, wherever it came from *)
      | Some sy, _ -> if sy.stype = stt_gnu_ifunc then stt_func else sy.stype
      | None, Common _ -> stt_object
      | None, _ -> stt_notype in
    let shndx = match g.defn with
      | Defined (inp, sec, _) -> (match inp.place.(sec) with Some (o, _) -> o.shndx | None -> shn_undef)
      | Common _ | Copy _ -> (osec st ".bss").shndx
      | Absolute _ -> shn_abs
      | _ -> shn_undef in
    let size = match defining g, g.defn with
      | _, Copy (_, n) -> n              (* what the loader is to copy *)
      | Some sy, _ -> sy.ssize
      | None, Common (n, _) -> n
      | None, _ -> 0 in
    let value = if typ = stt_tls then value_of st g - tls_block_start st else address_of st g in
    { Dynamic.nameoff = Dynamic.intern strings g.name;
      info = (((if g.weak then stb_weak else stb_global) lsl 4) lor typ);
      other = 0; shndx; value; size } in
  let import (g : gsym) =
    { Dynamic.nameoff = Dynamic.intern strings g.name;
      info = ((if g.weak then stb_weak else stb_global) lsl 4) lor stt_notype;
      other = 0; shndx = shn_undef; value = 0; size = 0 } in
  let syms = List.map export st.exports @ List.map import st.imports in
  put ".dynsym" (Dynamic.dynsym (Dynamic.null_sym :: syms));
  put ".dynstr" (Dynamic.strtab_contents strings);
  put ".hash" (Dynamic.hash_table
                 ("" :: List.map (fun (g : gsym) -> g.name) st.exports
                  @ List.map (fun (g : gsym) -> g.name) st.imports));
  (* the relocations the loader applies, the ones naming no symbol first *)
  let copies =
    List.map (fun ((g : gsym), _) ->
        { Dynamic.where = value_of st g; rtype = 5 (* R_X86_64_COPY *);
          rsym = g.dynidx; addend = 0 })
      (List.rev st.copies) in
  let rels = Dynamic.sort_rels (List.rev st.dynrels @ copies) in
  st.n_relative <- List.length (List.filter (fun (r : Dynamic.rel) -> r.rtype = Dynamic.r_x86_64_relative) rels);
  put ".rela.dyn" (Dynamic.rela rels);
  put ".interp" (interpreter ^ "\000");
  put ".dynamic" (Dynamic.dynamic (dynamic_entries st strings))

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
  define "__ehdr_start" (fun () -> st.base);
  define "_GLOBAL_OFFSET_TABLE_" (fun () -> match sec ".got.plt" with Some o -> o.addr | None -> start ".got" ());
  let seg_end flags = fun () -> match List.find_opt (fun s -> s.flags = flags) segments with Some s -> s.vaddr + s.memsz | None -> 0 in
  let data_end = fun () -> match List.find_opt (fun s -> s.flags = 6) segments with Some s -> s.vaddr + s.filesz | None -> 0 in
  List.iter (fun n -> define n (seg_end 5)) [ "etext"; "_etext"; "__etext" ];
  List.iter (fun n -> define n data_end) [ "edata"; "_edata" ];
  List.iter (fun n -> define n (seg_end 6)) [ "end"; "_end" ];
  define "__bss_start" (start ".bss");
  define "__executable_start" (fun () -> st.base);
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

let link ?(shared = false) ?(soname = "") ?(export_all = false) ?(prefer_shared = false)
    ?(rpath = "") ~output ~entry ~search items =
  let st = { symbols = Hashtbl.create 4096; order = []; inputs = []; comdat = Hashtbl.create 64; sections = Hashtbl.create 32;
             section_order = []; got = Hashtbl.create 256; got_slots = []; iplt = []; n_got = 0; n_iplt = 0; tls_end = 0;
             shared; dyn = shared; export_all; prefer_shared = prefer_shared || shared; rpath;
             base = (if shared then 0 else exec_base); n_dynrel = 0; n_relative = 0; dynrels = []; exports = [];
             imports = []; needed = []; dplts = []; n_dplt = 0;
             provided = Hashtbl.create 256; copies = []; soname } in
  (* 1, 2 *)
  load st ~search items;
  (match entry with Some e -> (gsym st e).referenced <- true | None -> ());
  (* 3 *)
  List.iter layout_section (List.rev st.section_order);
  allocate_commons st;
  scan_relocs st;
  allocate_copies st;
  if st.dyn then choose_exports st;
  synthesize st;
  let r, x, w, other = ordered st in
  let segments, file_size = assign_addresses st (r, x, w, other) in
  define_synthetics st segments;
  (* A shared object may be left with names nothing here defines: what
     loads it may have them, and the loader will say so if not.  An
     executable may not. *)
  if not shared then begin
    let undefined = List.filter (fun g -> g.referenced && not (is_defined g) && not g.weak) (List.rev st.order) in
    if undefined <> [] then error "undefined symbols: %s" (String.concat ", " (List.map (fun g -> g.name) undefined))
  end;
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
  (* [shared] the loader's tables, which name sections by number and so
     wait for that numbering *)
  if st.dyn then fill_dynamic st;
  let shndx_of_value (g : gsym) = match g.defn with
    | Defined (inp, sec, _) -> (match inp.place.(sec) with Some (o, _) -> o.shndx | None -> 0)
    | Common _ | Copy _ -> (osec st ".bss").shndx
    | Absolute _ | Synthetic _ -> shn_abs
    | Imported _ | Undefined -> shn_undef in
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
        (* an imported symbol has no address in this file, and says so
           here the way an undefined one does *)
        let value =
          if is_defined g && not (is_imported g)
          then (if typ = stt_tls then value_of st g - tls_block_start st else address_of st g)
          else 0 in
        add g.name (((if g.weak && is_defined g then stb_weak else stb_global) lsl 4) lor typ) (shndx_of_value g) value size
      end) (List.rev st.order);
  let syms = List.rev !syms in
  let strtab, offs = string_table (List.map (fun (n, _, _, _, _) -> n) syms) in
  let symtab = Buffer.create (24 * (List.length syms + 1)) in
  Buffer.add_string symtab (String.make 24 '\000');
  List.iter2 (fun (_, info, shndx, value, size) off -> u32 symtab off; Buffer.add_char symtab (Char.chr info); Buffer.add_char symtab '\000'; u16 symtab shndx; u64 symtab value; u64 symtab size) syms offs;
  (* the file: headers, segments, non-allocated sections, then the section table *)
  let out = Buffer.create (file_size + 65536) in
  let dynamic_present =
    match Hashtbl.find_opt st.sections ".dynamic" with Some o -> o.osize > 0 | None -> false in
  let interp_present =
    match Hashtbl.find_opt st.sections ".interp" with Some o -> o.osize > 0 | None -> false in
  let phnum = List.length segments + (if st.tls_end <> 0 then 1 else 0)
              + (if dynamic_present then 1 else 0) + (if interp_present then 1 else 0) + 1 in
  let shstr_names = List.map (fun o -> o.oname) placed @ [ ".symtab"; ".strtab"; ".shstrtab" ] in
  let shstrtab, shstr_offs = string_table shstr_names in
  Buffer.add_string out "\x7fELF\x02\x01\x01\x00"; Buffer.add_string out (String.make 8 '\000');
  u16 out (if st.shared then 3 else 2); u16 out 62; u32 out 1;   (* ET_DYN or ET_EXEC, x86-64 *)
  u64 out (match entry with Some e -> address_of st (gsym st e) | None -> 0);
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
  (match Hashtbl.find_opt st.sections ".interp" with
   | Some o when o.osize > 0 -> phdr 3 4 o.fileoff o.addr o.osize o.osize 1   (* PT_INTERP *)
   | _ -> ());
  (match Hashtbl.find_opt st.sections ".dynamic" with
   | Some o when o.osize > 0 -> phdr 2 6 o.fileoff o.addr o.osize o.osize 8   (* PT_DYNAMIC *)
   | _ -> ());
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
  (* [shared] the loader reads only the program headers and .dynamic,
     but the section headers still have to say which table each of these
     refers to, since that is what every tool reads *)
  let index name = match Hashtbl.find_opt st.sections name with Some o -> o.shndx | None -> 0 in
  let describe (o : osec) =
    match o.oname with
    | ".dynsym" -> index ".dynstr", 1, 24            (* one local entry: the null one *)
    | ".hash" -> index ".dynsym", 0, 4
    | ".dynamic" -> index ".dynstr", 0, 16
    | ".rela.dyn" | ".rela.plt" -> index ".dynsym", 0, 24
    | _ -> 0, 0, (if o.otype = sht_rela then 24 else 0) in
  List.iter2 (fun (o : osec) name ->
      let link, info, entsize = describe o in
      shdr name o.otype o.oflags o.addr o.fileoff o.osize link info o.oalign entsize)
    placed (List.filteri (fun i _ -> i < List.length placed) shstr_offs);
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
