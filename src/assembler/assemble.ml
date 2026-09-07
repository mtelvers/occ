(* The assembler proper: from parsed statements to an ELF object.

   The work happens in four passes, each a section below:

   1. Collection.  Statements are read in order.  Instructions are encoded
      into byte strings with fixups for anything symbolic; directives
      define symbols, switch sections, or append data.  Every section
      becomes a list of chunks: bytes, relaxable branches, alignment
      padding and labels.

   2. Layout.  Offsets are assigned to chunks.  Branches start in their
      short form and grow to the long form when their target turns out to
      be too far; the loop repeats until nothing changes (the classic
      fixed-point algorithm, since growing one branch can push another
      out of range).

   3. Generation.  With offsets known, .cfi and .loc records become the
      .eh_frame and .debug_line sections, which are laid out too.

   4. Resolution and output.  Each fixup is evaluated: a value the
      assembler knows is patched in; anything else becomes a relocation
      for the linker.  The symbol table is built and the ELF file written. *)

open Gas

type fixup = Encode.fixup

type chunk =
  | Bytes of string * fixup list * int                (* content, fixups, source line *)
  | Branch of { short : string; long : string; target : expr; mutable is_long : bool; bline : int }
  | Align of int * int option                        (* alignment in bytes, fill byte *)
  | Label of string

type section = {
  sname : string;
  mutable flags : int;
  mutable typ : int;
  mutable entsize : int;
  mutable chunks : chunk list;          (* reversed while collecting *)
  mutable count : int;                  (* number of chunks, so a label knows its index *)
  mutable arr : chunk array;            (* after collection *)
  mutable offsets : int array;          (* after layout *)
  mutable size : int;
  mutable align : int;
  mutable index : int;                  (* ELF section index, after output ordering *)
  mutable symidx : int;                 (* the section symbol's index in .symtab *)
  mutable locs : Debug_line.entry list; (* reversed *)
  mutable frames : Eh_frame.frame list; (* reversed *)
}

type def =
  | Undefined
  | At of section * int                 (* index of its Label chunk *)
  | Absolute of int64                   (* .set to a constant; may be redefined *)
  | Alias of expr                       (* .set to a symbolic expression *)
  | Common of int64 * int               (* size and alignment *)

type symbol = {
  name : string;
  mutable def : def;
  mutable binding : int option;         (* Elf.stb_*, None until .globl/.local/.weak *)
  mutable typ : int;                    (* Elf.stt_* *)
  mutable size : expr option;
  mutable visibility : int;
  mutable referenced : bool;
  mutable needed : bool;                (* named in a relocation, so it must be in the symbol table *)
  mutable symidx : int;
}

type state = {
  file : string;
  sections : (string, section) Hashtbl.t;
  mutable section_order : section list; (* reversed *)
  symbols : (string, symbol) Hashtbl.t;
  mutable symbol_order : symbol list;   (* reversed *)
  mutable current : section;
  mutable previous : section option;
  mutable files : (int * string) list;  (* .file n "name" *)
  mutable file_symbol : string option;  (* .file "name" *)
  mutable frame : (string * bool * (string * cfi) list) option;   (* open .cfi_startproc: label, signal, ops reversed *)
  mutable counter : int;
  mutable line : int;
}

let error st fmt = Diag.error { Loc.file = st.file; line = st.line; col = 0 } fmt

(* ---- Sections -------------------------------------------------------------- *)

let starts_with p s = String.length s >= String.length p && String.sub s 0 (String.length p) = p

(* flags and type for a section named without an explicit specification *)
let default_section_attrs name =
  let open Elf in
  if name = ".text" || starts_with ".text." name then shf_alloc lor shf_execinstr, sht_progbits
  else if name = ".data" || starts_with ".data." name then shf_alloc lor shf_write, sht_progbits
  else if name = ".bss" || starts_with ".bss." name then shf_alloc lor shf_write, sht_nobits
  else if name = ".rodata" || starts_with ".rodata." name then shf_alloc, sht_progbits
  else if name = ".tdata" then shf_alloc lor shf_write lor shf_tls, sht_progbits
  else if name = ".tbss" then shf_alloc lor shf_write lor shf_tls, sht_nobits
  else if name = ".eh_frame" then shf_alloc, sht_progbits
  else if name = ".init_array" || name = ".fini_array" then shf_alloc lor shf_write, sht_progbits
  else if name = ".note.GNU-stack" then 0, sht_progbits
  else 0, sht_progbits

let flags_of_string s =
  let open Elf in
  String.fold_left (fun acc c ->
      acc lor (match c with
          | 'a' -> shf_alloc | 'w' -> shf_write | 'x' -> shf_execinstr | 'M' -> shf_merge
          | 'S' -> shf_strings | 'T' -> shf_tls | 'G' -> shf_group | '\"' -> 0
          | _ -> 0)) 0 s

let type_of_string = function
  | "progbits" -> Elf.sht_progbits | "nobits" -> Elf.sht_nobits | "note" -> Elf.sht_note
  | "init_array" -> 14 | "fini_array" -> 15
  | t -> failwith ("unknown section type " ^ t)

let new_section name flags typ =
  { sname = name; flags; typ; entsize = 0; chunks = []; count = 0; arr = [||]; offsets = [||]; size = 0;
    align = 1; index = 0; symidx = 0; locs = []; frames = [] }

let find_section st name =
  match Hashtbl.find_opt st.sections name with
  | Some s -> s
  | None ->
      let flags, typ = default_section_attrs name in
      let s = new_section name flags typ in
      if name = ".eh_frame" then s.align <- 8;
      Hashtbl.replace st.sections name s;
      st.section_order <- s :: st.section_order;
      s

let switch_section st (spec : Gas.section_spec) =
  let s = find_section st spec.sname in
  (match spec.sflags with Some f -> s.flags <- flags_of_string f | None -> ());
  (match spec.stype with Some t -> s.typ <- type_of_string t | None -> ());
  (match spec.sextra with
   | [ e ] -> (match Encode.const e with Some v -> s.entsize <- Int64.to_int v | None -> ())
   | _ -> ());
  if s != st.current then st.previous <- Some st.current;
  st.current <- s

let add_chunk st c =
  let s = st.current in
  s.chunks <- c :: s.chunks;
  s.count <- s.count + 1

(* ---- Symbols --------------------------------------------------------------- *)

let symbol st name =
  match Hashtbl.find_opt st.symbols name with
  | Some s -> s
  | None ->
      let s = { name; def = Undefined; binding = None; typ = Elf.stt_notype; size = None; visibility = Elf.stv_default;
                referenced = false; needed = false; symidx = 0 } in
      Hashtbl.replace st.symbols name s;
      st.symbol_order <- s :: st.symbol_order;
      s

let define_label st name =
  let s = symbol st name in
  (match s.def with
   | Undefined -> ()
   | _ -> error st "symbol %s is already defined" name);
  s.def <- At (st.current, st.current.count);
  add_chunk st (Label name)

let fresh_label st prefix =
  st.counter <- st.counter + 1;
  let name = Printf.sprintf ".L%s%d" prefix st.counter in
  define_label st name;
  name

(* Replace symbols with a current constant value by that value, and "." by
   a label at this point.  Constant symbols are substituted at once
   because .set may give them a new value later. *)
let rec substitute st e =
  match e with
  | Num _ -> e
  | Sym (name, _) ->
      (* symbols are created at first mention, so the symbol table keeps source order *)
      (match symbol st name with
       | { def = Absolute v; _ } when (match e with Sym (_, None) -> true | _ -> false) -> Num v
       | _ -> e)
  | Dot -> Sym (fresh_label st "dot", None)
  | Neg x -> Neg (substitute st x)
  | Not x -> Not (substitute st x)
  | Bin (op, x, y) -> let x = substitute st x in let y = substitute st y in Bin (op, x, y)

let substitute_operand st = function
  | Imm e -> Imm (substitute st e)
  | Mem m -> Mem { m with disp = Option.map (substitute st) m.disp }
  | (Reg _ | Indirect _) as o ->
      (match o with
       | Indirect (Mem m) -> Indirect (Mem { m with disp = Option.map (substitute st) m.disp })
       | o -> o)

(* ---- Pass 1: collection ---------------------------------------------------- *)

let data_item width e =
  match Encode.const e with
  | Some v ->
      let b = Buffer.create 8 in
      for i = 0 to width - 1 do Buffer.add_char b (Char.chr (Int64.to_int (Int64.shift_right_logical v (8 * i)) land 0xff)) done;
      Buffer.contents b, []
  | None ->
      String.make width '\000',
      [ { Encode.at = 0; size = width; target = e; pcrel = false; pcbase = 0; signed = false; relaxable = false; branch = false } ]

let constant st e =
  match Encode.const (substitute st e) with
  | Some v -> Int64.to_int v
  | None -> error st "expected a constant expression"

let record_cfi st c =
  let here = fresh_label st "cfi" in
  match c, st.frame with
  | Cfi_startproc _, Some _ -> error st ".cfi_startproc inside an open frame"
  | Cfi_startproc _, None -> st.frame <- Some (here, false, [])
  | _, None -> error st "%s outside .cfi_startproc" "cfi directive"
  | Cfi_endproc, Some (start, signal, ops) ->
      st.current.frames <- { Eh_frame.start; finish = here; signal; ops = List.rev ops } :: st.current.frames;
      st.frame <- None
  | Cfi_signal_frame, Some (start, _, ops) -> st.frame <- Some (start, true, ops)
  | c, Some (start, signal, ops) -> st.frame <- Some (start, signal, (here, c) :: ops)

let directive st = function
  | Section spec -> switch_section st spec
  | Previous ->
      (match st.previous with
       | Some p -> let c = st.current in st.current <- p; st.previous <- Some c
       | None -> error st ".previous without a previous section")
  | Global s -> (symbol st s).binding <- Some Elf.stb_global
  | Local s -> (symbol st s).binding <- Some Elf.stb_local
  | Weak s -> (symbol st s).binding <- Some Elf.stb_weak
  | Visibility (s, v) ->
      (symbol st s).visibility <- (match v with "hidden" -> Elf.stv_hidden | "protected" -> Elf.stv_protected | _ -> Elf.stv_internal)
  | Type (s, t) ->
      (symbol st s).typ <- (match t with
          | "function" | "gnu_indirect_function" -> Elf.stt_func
          | "object" -> Elf.stt_object | "tls_object" -> Elf.stt_tls
          | "notype" -> Elf.stt_notype
          | t -> error st "unknown symbol type %s" t)
  | Size (s, e) -> (symbol st s).size <- Some (substitute st e)
  | Set (s, e) ->
      let e = substitute st e in
      let sy = symbol st s in
      (match sy.def, Encode.const e with
       | (Undefined | Absolute _), Some v -> sy.def <- Absolute v
       | Undefined, None -> sy.def <- Alias e
       | _ -> error st "symbol %s is already defined" s)
  | Comm (s, size, align) ->
      let size = Int64.of_int (constant st size) in
      let align = match align with Some a -> constant st a | None -> 1 in
      let sy = symbol st s in
      if sy.binding = Some Elf.stb_local then begin
        (* a local common symbol is just space in .bss *)
        let saved = st.current in
        st.current <- find_section st ".bss";
        add_chunk st (Align (align, None)); st.current.align <- max st.current.align align;
        define_label st s;
        add_chunk st (Bytes (String.make (Int64.to_int size) '\000', [], st.line));
        sy.size <- Some (Num size); sy.typ <- Elf.stt_object;
        st.current <- saved
      end else begin
        sy.def <- Common (size, align);
        if sy.binding = None then sy.binding <- Some Elf.stb_global
      end
  | Data (width, es) ->
      List.iter (fun e ->
          let bytes, fixups = data_item width (substitute st e) in
          add_chunk st (Bytes (bytes, fixups, st.line))) es
  | Ascii ss -> add_chunk st (Bytes (String.concat "" ss, [], st.line))
  | Asciz ss -> add_chunk st (Bytes (String.concat "" (List.map (fun s -> s ^ "\000") ss), [], st.line))
  | Zero (n, fill) -> add_chunk st (Bytes (String.make (constant st n) (Char.chr (fill land 0xff)), [], st.line))
  | Uleb128 es | Sleb128 es as d ->
      let b = Buffer.create 8 in
      List.iter (fun e ->
          let v = constant st e in
          match d with Uleb128 _ -> Leb.uleb b v | _ -> Leb.sleb b v) es;
      add_chunk st (Bytes (Buffer.contents b, [], st.line))
  | Align (n, fill) ->
      if n <= 0 || n land (n - 1) <> 0 then error st "alignment %d is not a power of two" n;
      add_chunk st (Align (n, fill));
      st.current.align <- max st.current.align n
  | File (None, name) -> st.file_symbol <- Some name
  | File (Some n, name) -> st.files <- (n, name) :: List.remove_assoc n st.files
  | Loc (file, line, col) ->
      if not (List.mem_assoc file st.files) then error st ".loc refers to undeclared file %d" file;
      let at = fresh_label st "loc" in
      st.current.locs <- { Debug_line.at; file; line; col } :: st.current.locs
  | Cfi c -> record_cfi st c
  | Ident _ | Ignored _ -> ()

let statement st (l : line) =
  st.line <- l.lineno;
  match l.stmt with
  | Label name -> define_label st name
  | Directive d -> directive st d
  | Instruction i ->
      let i = { i with operands = List.map (substitute_operand st) i.operands } in
      (match Encode.instruction i with
       | Encode.Fixed (bytes, fixups) -> add_chunk st (Bytes (bytes, fixups, st.line))
       | Encode.Branch { short; long; target } -> add_chunk st (Branch { short; long; target; is_long = false; bline = st.line })
       | exception Encode.Bad msg -> error st "%s" msg)

(* ---- Expression evaluation after layout ------------------------------------ *)

(* A value is  symbol + addend - (a point in a section).  Differences of
   two labels in one section fold to a constant; a symbol minus a point
   in the fixup's own section becomes a pc-relative relocation. *)
type value = { sym : symbol option; addend : int64; minus : (section * int) option }

let const v = { sym = None; addend = v; minus = None }

let offset_of (sy : symbol) =
  match sy.def with
  | At (sec, i) -> Some (sec, sec.offsets.(i))
  | _ -> None

let rec eval st e =
  match e with
  | Num v -> const v
  | Dot -> error st "unexpected \".\""
  | Sym (name, _) ->
      let sy = symbol st name in
      sy.referenced <- true;
      (match sy.def with
       | Absolute v -> const v
       | Alias e -> eval st e
       | _ -> { sym = Some sy; addend = 0L; minus = None })
  | Neg x ->
      let v = eval st x in
      if v.sym <> None || v.minus <> None then error st "cannot negate an address";
      const (Int64.neg v.addend)
  | Not x ->
      let v = eval st x in
      if v.sym <> None || v.minus <> None then error st "cannot complement an address";
      const (Int64.lognot v.addend)
  | Bin (Add, x, y) ->
      let a = eval st x and b = eval st y in
      if (a.sym <> None && b.sym <> None) || (a.minus <> None && b.minus <> None) then error st "expression is not an address plus a constant";
      simplify { sym = (if a.sym <> None then a.sym else b.sym); addend = Int64.add a.addend b.addend;
                    minus = (if a.minus <> None then a.minus else b.minus) }
  | Bin (Sub, x, y) ->
      let a = eval st x and b = eval st y in
      (match b.sym, b.minus with
       | None, None -> { a with addend = Int64.sub a.addend b.addend }
       | Some sb, None ->
           if a.minus <> None then error st "expression subtracts two addresses";
           (match offset_of sb with
            | Some (sec, off) -> simplify { a with addend = Int64.sub a.addend b.addend; minus = Some (sec, off) }
            | None -> error st "cannot subtract undefined symbol %s" sb.name)
       | _ -> error st "expression is too complex")
  | Bin (op, x, y) ->
      let a = eval st x and b = eval st y in
      if a.sym <> None || b.sym <> None || a.minus <> None || b.minus <> None then error st "arithmetic on addresses";
      let a = a.addend and b = b.addend in
      const (match op with
          | Mul -> Int64.mul a b | Div -> Int64.div a b | Mod -> Int64.rem a b
          | And -> Int64.logand a b | Or -> Int64.logor a b | Xor -> Int64.logxor a b
          | Shl -> Int64.shift_left a (Int64.to_int b) | Shr -> Int64.shift_right_logical a (Int64.to_int b)
          | Add | Sub -> assert false)

(* fold  label - point  when both are in one section *)
and simplify v =
  match v.sym, v.minus with
  | Some sy, Some (sec, off) ->
      (match offset_of sy with
       | Some (sec', off') when sec' == sec -> const (Int64.add v.addend (Int64.of_int (off' - off)))
       | _ -> v)
  | _ -> v

(* ---- Pass 2: layout -------------------------------------------------------- *)

let modifier_of e =
  let rec go = function
    | Sym (_, m) -> m
    | Bin (_, x, y) -> (match go x with Some m -> Some m | None -> go y)
    | Neg x | Not x -> go x
    | Num _ | Dot -> None in
  go e


let align_up n a = (n + a - 1) / a * a

let chunk_size off = function
  | Bytes (s, _, _) -> String.length s
  | Branch b -> if b.is_long then String.length b.long + 4 else String.length b.short + 1
  | Align (n, _) -> align_up off n - off
  | Label _ -> 0

let layout st (sec : section) =
  if Array.length sec.arr <> sec.count then sec.arr <- Array.of_list (List.rev sec.chunks);
  sec.offsets <- Array.make sec.count 0;
  let changed = ref true in
  while !changed do
    changed := false;
    let off = ref 0 in
    Array.iteri (fun i c -> sec.offsets.(i) <- !off; off := !off + chunk_size !off c) sec.arr;
    sec.size <- !off;
    Array.iteri (fun i c ->
        match c with
        | Branch b when not b.is_long ->
            st.line <- b.bline;
            let v = eval st b.target in
            (* short form only for a target in this section that may be
               resolved here: gas never relaxes an @PLT branch to a global *)
            let local =
              match v.sym, v.minus with
              | Some sy, None ->
                  let global = sy.binding = Some Elf.stb_global || sy.binding = Some Elf.stb_weak in
                  let plt = modifier_of b.target <> None in
                  (match offset_of sy with
                   | Some (s, off) when s == sec && not (plt && global) -> Some off
                   | _ -> None)
              | _ -> None in
            (match local with
             | Some target ->
                 let disp = Int64.add (Int64.of_int (target - (sec.offsets.(i) + String.length b.short + 1))) v.addend in
                 if not (Encode.fits_int8 disp) then begin b.is_long <- true; changed := true end
             | None -> b.is_long <- true; changed := true)
        | _ -> ()) sec.arr
  done

(* ---- Pass 3: .eh_frame and .debug_line -------------------------------------- *)

let generate_debug st =
  let label_offset name =
    match offset_of (symbol st name) with Some (_, off) -> off | None -> error st "undefined label %s" name in
  let with_frames = List.filter (fun (s : section) -> s.frames <> []) (List.rev st.section_order) in
  if with_frames <> [] then begin
    let frames = List.concat_map (fun s -> List.rev s.frames) with_frames in
    (try
       let bytes, fixups = Eh_frame.generate ~offset:label_offset frames in
       let eh = find_section st ".eh_frame" in
       eh.chunks <- Bytes (bytes, fixups, 0) :: eh.chunks; eh.count <- eh.count + 1;
       layout st eh
     with Failure msg -> error st "%s" msg)
  end;
  let with_locs = List.filter (fun (s : section) -> s.locs <> []) (List.rev st.section_order) in
  (* a .file directive, or an empty .debug_line section left by the compiler,
     still produces a line table header, as in gas *)
  if with_locs <> [] || st.files <> [] || Hashtbl.mem st.sections ".debug_line" then begin
    let in_section (s : section) f =
      let saved = st.current in
      st.current <- s;
      let r = f () in
      st.current <- saved; r in
    let sequences = List.map (fun (s : section) ->
        let finish = in_section s (fun () -> fresh_label st "end") in
        layout st s;
        { Debug_line.entries = List.rev s.locs; finish }) with_locs in
    let bytes, fixups = Debug_line.generate ~files:st.files ~offset:label_offset sequences in
    let synthesize = not (Hashtbl.mem st.sections ".debug_info") in
    let dl = find_section st ".debug_line" in
    let line_label = in_section dl (fun () -> fresh_label st "line") in
    in_section dl (fun () -> add_chunk st (Bytes (bytes, fixups, 0)));
    layout st dl;
    (* a file with line information but no debugging information entries
       gets a compile unit, so debuggers can find the line table *)
    if synthesize && with_locs <> [] then begin
      let text = List.hd with_locs in
      let text_end = in_section text (fun () -> fresh_label st "end") in
      layout st text;
      let funcs = List.filter_map (fun (sy : symbol) ->
          match sy.def, sy.size with
          | At (sec, _), Some size when sec == text && sy.typ = Elf.stt_func ->
              (match eval st size with
               | { sym = None; minus = None; addend } ->
                   Some { Debug_info.fname = sy.name; global = (sy.binding = Some Elf.stb_global); fsize = Int64.to_int addend }
               | _ -> None)
          | _ -> None) (List.rev st.symbol_order) in
      let label_for name = in_section (find_section st name) (fun () -> fresh_label st "dbg") in
      let abbrev_label = label_for ".debug_abbrev" and info_label = label_for ".debug_info" and str_label = label_for ".debug_str" in
      let name = match st.file_symbol, List.assoc_opt 1 st.files with
        | Some f, _ | None, Some f -> f
        | None, None -> st.file in
      let sections = Debug_info.generate
          { Debug_info.name; comp_dir = Sys.getcwd (); producer = "occas 0.1"; text_end; text_size = text.size; funcs;
            line_label; abbrev_label; info_label; str_label } in
      List.iter (fun (sname, bytes, fixups) ->
          let sec = find_section st sname in
          in_section sec (fun () -> add_chunk st (Bytes (bytes, fixups, 0)));
          layout st sec) sections
    end
  end

(* ---- Pass 4: resolution ---------------------------------------------------- *)

type resolved =
  | Value of int64
  | Reloc of int * [ `Sym of symbol | `Section of section ] * int64   (* type, against, addend *)

let reloc_type st ~modifier ~pcrel ~size ~signed ~branch ~relaxable ~rex =
  let open Elf in
  match modifier with
  | Some ("PLT" | "plt") -> r_x86_64_plt32
  | Some ("GOTPCREL" | "gotpcrel") -> if relaxable then (if rex then r_x86_64_rex_gotpcrelx else r_x86_64_gotpcrelx) else r_x86_64_gotpcrel
  | Some ("gottpoff" | "GOTTPOFF") -> r_x86_64_gottpoff
  | Some ("tpoff" | "TPOFF") -> r_x86_64_tpoff32
  | Some m -> error st "unsupported relocation modifier @%s" m
  | None ->
      if pcrel then (if branch then r_x86_64_plt32 else match size with 8 -> r_x86_64_pc64 | 4 -> r_x86_64_pc32 | 2 -> r_x86_64_pc16 | _ -> r_x86_64_pc8)
      else match size with 8 -> r_x86_64_64 | 4 -> if signed then r_x86_64_32s else r_x86_64_32 | 2 -> r_x86_64_16 | _ -> r_x86_64_8

(* A relocation against a local symbol is expressed against its section,
   except for the GOT, PLT and TLS kinds, where the linker needs the
   symbol itself (it creates one GOT entry per symbol), and for symbols in
   mergeable sections, whose contents the linker may rearrange. *)
let against st rtype (sy : symbol) =
  let open Elf in
  let per_symbol = List.mem rtype [ r_x86_64_plt32; r_x86_64_gotpcrel; r_x86_64_gotpcrelx; r_x86_64_rex_gotpcrelx;
                                    r_x86_64_gottpoff; r_x86_64_tpoff32 ] in
  (* a symbol referenced through a TLS relocation is a TLS symbol, and the
     linker checks that its definition agrees *)
  if rtype = r_x86_64_gottpoff || rtype = r_x86_64_tpoff32 then sy.typ <- stt_tls;
  match sy.def with
  | At (sec, i) when not per_symbol && sec.flags land shf_merge = 0
                     && sy.binding <> Some Elf.stb_global && sy.binding <> Some Elf.stb_weak ->
      `Section sec, Int64.of_int sec.offsets.(i)
  | At _ | Undefined | Common _ -> sy.needed <- true; `Sym sy, 0L
  | Absolute _ | Alias _ -> error st "cannot relocate against %s" sy.name

(* [pos] is the fixup's offset in the section; [base] the position
   pc-relative values are relative to.  A pc-relative reference to a
   symbol in the same section is resolved here unless the symbol is global
   (the linker may still redirect it to another definition), except for a
   relaxable jmp or jcc, which was already measured against it. *)
let resolve st (sec : section) ~pos ~base ~rex ?(relaxed = false) (f : fixup) =
  let v = eval st f.target in
  let modifier = modifier_of f.target in
  let rtype () = reloc_type st ~modifier ~pcrel:f.pcrel ~size:f.size ~signed:f.signed ~branch:f.branch ~relaxable:f.relaxable ~rex in
  let is_global (sy : symbol) = sy.binding = Some Elf.stb_global || sy.binding = Some Elf.stb_weak in
  (* may a same-section pc-relative reference be resolved here? *)
  let resolvable sy = match modifier with
    | None -> relaxed || not (is_global sy)
    | Some ("PLT" | "plt") -> not (is_global sy)
    | Some _ -> false in
  match v.sym, v.minus with
  | None, None -> if f.pcrel then error st "pc-relative reference to a constant" else Value v.addend
  | Some sy, None ->
      (match offset_of sy with
       | Some (sec', off) when f.pcrel && sec' == sec && resolvable sy ->
           Value (Int64.add v.addend (Int64.of_int (off - base)))
       | _ ->
           let rtype = rtype () in
           let target, extra = against st rtype sy in
           let addend = if f.pcrel then Int64.sub v.addend (Int64.of_int (base - pos)) else v.addend in
           Reloc (rtype, target, Int64.add addend extra))
  | Some sy, Some (msec, moff) ->
      if msec != sec then error st "difference of addresses in different sections";
      let target, extra = against st Elf.r_x86_64_pc32 sy in
      Reloc (Elf.r_x86_64_pc32, target, Int64.add (Int64.add v.addend (Int64.of_int (pos - moff))) extra)
  | None, Some _ -> error st "negative address in expression"

(* gas's multi-byte nops for padding in code sections *)
let nops = [|
  "\x90"; "\x66\x90"; "\x0f\x1f\x00"; "\x0f\x1f\x40\x00"; "\x0f\x1f\x44\x00\x00"; "\x66\x0f\x1f\x44\x00\x00";
  "\x0f\x1f\x80\x00\x00\x00\x00"; "\x0f\x1f\x84\x00\x00\x00\x00\x00"; "\x66\x0f\x1f\x84\x00\x00\x00\x00\x00";
  "\x66\x2e\x0f\x1f\x84\x00\x00\x00\x00\x00"; "\x66\x66\x2e\x0f\x1f\x84\x00\x00\x00\x00\x00" |]

let padding (sec : section) n fill =
  match fill with
  | Some f -> String.make n (Char.chr (f land 0xff))
  | None when sec.flags land Elf.shf_execinstr <> 0 ->
      let b = Buffer.create n in
      let rest = ref n in
      while !rest > 11 do Buffer.add_string b nops.(10); rest := !rest - 11 done;
      if !rest > 0 then Buffer.add_string b nops.(!rest - 1);
      Buffer.contents b
  | None -> String.make n '\000'

let patch bytes at size v =
  for i = 0 to size - 1 do
    Bytes.set bytes (at + i) (Char.chr (Int64.to_int (Int64.shift_right_logical v (8 * i)) land 0xff))
  done

let check_fits st size signed v =
  let ok = match size, signed with
    | 8, _ -> true
    | 4, true -> Encode.fits_int32 v
    | 4, false -> v >= -2147483648L && v <= 4294967295L
    | 2, _ -> v >= -32768L && v <= 65535L
    | _ -> v >= -128L && v <= 255L in
  if not ok then error st "value %Ld does not fit in %d bytes" v size

(* the bytes of a section and its relocations *)
let section_body st (sec : section) =
  let body = Buffer.create (max 16 sec.size) in
  let relocs = ref [] in
  let has_rex bytes at = at >= 3 && (let c = Char.code (Bytes.get bytes (at - 3)) in c >= 0x40 && c <= 0x4f) in
  Array.iteri (fun i c ->
      let off = sec.offsets.(i) in
      match c with
      | Label _ -> ()
      | Align (n, fill) -> Buffer.add_string body (padding sec (align_up off n - off) fill)
      | Bytes (s, fixups, line) ->
          st.line <- line;
          let bytes = Bytes.of_string s in
          List.iter (fun (f : fixup) ->
              match resolve st sec ~pos:(off + f.at) ~base:(off + f.pcbase) ~rex:(has_rex bytes f.at) f with
              | Value v -> check_fits st f.size f.signed v; patch bytes f.at f.size v
              | Reloc (t, target, addend) -> relocs := (off + f.at, t, target, addend) :: !relocs) fixups;
          Buffer.add_bytes body bytes
      | Branch b ->
          st.line <- b.bline;
          let opcode = if b.is_long then b.long else b.short in
          let size = if b.is_long then 4 else 1 in
          let at = String.length opcode in
          let bytes = Bytes.make (at + size) '\000' in
          Bytes.blit_string opcode 0 bytes 0 at;
          let f = { Encode.at; size; target = b.target; pcrel = true; pcbase = at + size; signed = true; relaxable = false; branch = true } in
          (match resolve st sec ~pos:(off + at) ~base:(off + at + size) ~rex:false ~relaxed:true f with
           | Value v -> check_fits st size true v; patch bytes at size v
           | Reloc (t, target, addend) -> relocs := (off + at, t, target, addend) :: !relocs);
          Buffer.add_bytes body bytes) sec.arr;
  Buffer.contents body, List.rev !relocs

(* ---- Output ---------------------------------------------------------------- *)

let is_local_label name = starts_with ".L" name

let run file text =
  let lines = Gas_parse.parse file text in
  let text_section = new_section ".text" (Elf.shf_alloc lor Elf.shf_execinstr) Elf.sht_progbits in
  let st = { file; sections = Hashtbl.create 8; section_order = [ text_section ]; symbols = Hashtbl.create 64;
             symbol_order = []; current = text_section; previous = None; files = []; file_symbol = None;
             frame = None; counter = 0; line = 0 } in
  Hashtbl.replace st.sections ".text" text_section;
  (* pass 1 *)
  List.iter (statement st) lines;
  if st.frame <> None then error st "missing .cfi_endproc";
  (* pass 2 *)
  let sections = List.rev st.section_order in
  List.iter (layout st) sections;
  (* pass 3 *)
  generate_debug st;
  let sections = List.rev st.section_order in
  (* pass 4: bodies and relocations first, since they mark referenced symbols *)
  let bodies = List.map (fun (sec : section) -> sec, section_body st sec) sections in
  (* like gas, a file using the GOT declares _GLOBAL_OFFSET_TABLE_, which the linker defines *)
  let uses_got = List.exists (fun (_, (_, relocs)) ->
      List.exists (fun (_, t, _, _) ->
          List.mem t Elf.[ r_x86_64_gotpcrel; r_x86_64_gotpcrelx; r_x86_64_rex_gotpcrelx; r_x86_64_gottpoff; r_x86_64_tpoff32 ]) relocs) bodies in
  if uses_got then begin
    let sy = symbol st "_GLOBAL_OFFSET_TABLE_" in
    if sy.def = Undefined then sy.referenced <- true
  end;
  (* the symbol table: file, sections, local symbols, then globals *)
  let defined_symbols = List.rev st.symbol_order in
  let emitted = List.filter (fun (sy : symbol) ->
      (sy.needed || not (is_local_label sy.name)) &&
      (match sy.def with
       | Undefined -> sy.referenced || sy.binding <> None
       | _ -> true)) defined_symbols in
  (* section symbols only for sections that relocations refer to *)
  let referenced_sections = List.concat_map (fun (_, (_, relocs)) ->
      List.filter_map (fun (_, _, target, _) -> match target with `Section s -> Some s | `Sym _ -> None) relocs) bodies in
  let is_global (sy : symbol) = match sy.def with
    | Undefined -> true
    | Common _ -> true
    | _ -> sy.binding = Some Elf.stb_global || sy.binding = Some Elf.stb_weak in
  let locals = List.filter (fun sy -> not (is_global sy)) emitted in
  let globals = List.filter is_global emitted in
  (* section indices: each content section, then its .rela if it has one *)
  let next_index = ref 1 in
  List.iter (fun ((sec : section), (_, relocs)) ->
      sec.index <- !next_index;
      incr next_index;
      if relocs <> [] then incr next_index) bodies;
  let symtab_index = !next_index in
  let strtab_index = symtab_index + 1 in
  let syms = ref [] in
  let add s = syms := s :: !syms in
  (match st.file_symbol with
   | Some name -> add { Elf.sname = name; bind = Elf.stb_local; stype = Elf.stt_file; other = 0; shndx = Elf.shn_abs; value = 0L; ssize = 0L }
   | None -> ());
  let n_syms = ref (List.length !syms) in
  List.iter (fun ((sec : section), _) ->
      if List.memq sec referenced_sections then begin
        incr n_syms; sec.symidx <- !n_syms;
        add { Elf.sname = ""; bind = Elf.stb_local; stype = Elf.stt_section; other = 0; shndx = sec.index; value = 0L; ssize = 0L }
      end) bodies;
  let symbol_entry (sy : symbol) bind =
    let size = match sy.size with
      | Some e -> (match eval st e with { sym = None; minus = None; addend } -> addend | _ -> error st "size of %s is not a constant" sy.name)
      | None -> 0L in
    let shndx, value = match sy.def with
      | At (sec, i) -> sec.index, Int64.of_int sec.offsets.(i)
      | Absolute v -> Elf.shn_abs, v
      | Alias e ->
          (match eval st e with
           | { sym = None; minus = None; addend } -> Elf.shn_abs, addend
           | { sym = Some target; minus = None; addend } ->
               (match offset_of target with
                | Some (sec, off) -> sec.index, Int64.add (Int64.of_int off) addend
                | None -> error st "alias %s of an undefined symbol" sy.name)
           | _ -> error st "alias %s is too complex" sy.name)
      | Common (_, align) -> Elf.shn_common, Int64.of_int align
      | Undefined -> Elf.shn_undef, 0L in
    let ssize = match sy.def with Common (s, _) -> s | _ -> size in
    { Elf.sname = sy.name; bind; stype = sy.typ; other = sy.visibility; shndx; value; ssize } in
  List.iter (fun (sy : symbol) -> incr n_syms; sy.symidx <- !n_syms; add (symbol_entry sy Elf.stb_local)) locals;
  let n_locals = !n_syms + 1 in
  List.iter (fun (sy : symbol) ->
      incr n_syms; sy.symidx <- !n_syms;
      add (symbol_entry sy (if sy.binding = Some Elf.stb_weak then Elf.stb_weak else Elf.stb_global))) globals;
  let symtab, strtab = Elf.symtab_body (List.rev !syms) in
  (* the section list *)
  let elf_sections = List.concat_map (fun ((sec : section), (body, relocs)) ->
      let nobits = sec.typ = Elf.sht_nobits in
      let s = Elf.section ~typ:sec.typ ~flags:sec.flags ~align:sec.align ~entsize:sec.entsize
          ~size:sec.size sec.sname (if nobits then "" else body) in
      if relocs = [] then [ s ]
      else
        let rela = Elf.rela_body (List.map (fun (offset, rtype, target, addend) ->
            let sym = match target with `Sym (sy : symbol) -> sy.symidx | `Section (s : section) -> s.symidx in
            { Elf.offset; rtype; sym; addend }) relocs) in
        [ s; Elf.section ~typ:Elf.sht_rela ~flags:Elf.shf_info_link ~align:8 ~entsize:24 ~link:symtab_index ~info:sec.index
            (".rela" ^ sec.sname) rela ]) bodies in
  let elf_sections = elf_sections @ [
      Elf.section ~typ:Elf.sht_symtab ~flags:0 ~align:8 ~entsize:24 ~link:strtab_index ~info:n_locals ".symtab" symtab;
      Elf.section ~typ:Elf.sht_strtab ~flags:0 ~align:1 ".strtab" strtab ] in
  Elf.write elf_sections

(* assemble the given files, concatenated, into one object *)
let files inputs output =
  let text = String.concat "\n" (List.map (fun f -> In_channel.with_open_bin f In_channel.input_all) inputs) in
  let name = match inputs with [ f ] -> f | _ -> String.concat "+" inputs in
  let obj = run name text in
  Out_channel.with_open_bin output (fun oc -> output_string oc obj)
