open Asm

let reg64 = function
  | RAX -> "%rax" | RBX -> "%rbx" | RCX -> "%rcx" | RDX -> "%rdx" | RSI -> "%rsi" | RDI -> "%rdi"
  | RBP -> "%rbp" | RSP -> "%rsp" | R8 -> "%r8" | R9 -> "%r9" | R10 -> "%r10" | R11 -> "%r11"
  | R12 -> "%r12" | R13 -> "%r13" | R14 -> "%r14" | R15 -> "%r15" | XMM n -> Printf.sprintf "%%xmm%d" n

let reg32 = function
  | RAX -> "%eax" | RBX -> "%ebx" | RCX -> "%ecx" | RDX -> "%edx" | RSI -> "%esi" | RDI -> "%edi"
  | RBP -> "%ebp" | RSP -> "%esp" | R8 -> "%r8d" | R9 -> "%r9d" | R10 -> "%r10d" | R11 -> "%r11d"
  | R12 -> "%r12d" | R13 -> "%r13d" | R14 -> "%r14d" | R15 -> "%r15d" | XMM n -> Printf.sprintf "%%xmm%d" n

let reg16 = function
  | RAX -> "%ax" | RBX -> "%bx" | RCX -> "%cx" | RDX -> "%dx" | RSI -> "%si" | RDI -> "%di"
  | RBP -> "%bp" | RSP -> "%sp" | R8 -> "%r8w" | R9 -> "%r9w" | R10 -> "%r10w" | R11 -> "%r11w"
  | R12 -> "%r12w" | R13 -> "%r13w" | R14 -> "%r14w" | R15 -> "%r15w" | XMM n -> Printf.sprintf "%%xmm%d" n

let reg8 = function
  | RAX -> "%al" | RBX -> "%bl" | RCX -> "%cl" | RDX -> "%dl" | RSI -> "%sil" | RDI -> "%dil"
  | RBP -> "%bpl" | RSP -> "%spl" | R8 -> "%r8b" | R9 -> "%r9b" | R10 -> "%r10b" | R11 -> "%r11b"
  | R12 -> "%r12b" | R13 -> "%r13b" | R14 -> "%r14b" | R15 -> "%r15b" | XMM n -> Printf.sprintf "%%xmm%d" n

let reg w r = match w with B -> reg8 r | W -> reg16 r | L -> reg32 r | Q -> reg64 r

let suffix = function B -> "b" | W -> "w" | L -> "l" | Q -> "q"

let operand w = function
  | Imm v -> "$" ^ Int64.to_string v
  | Reg r -> reg w r
  | Mem (r, 0) -> "(" ^ reg64 r ^ ")"
  | Mem (r, d) -> Printf.sprintf "%d(%s)" d (reg64 r)
  | Mem_index (b, i, sc) -> Printf.sprintf "(%s,%s,%d)" (reg64 b) (reg64 i) sc
  | Rip (s, 0) -> s ^ "(%rip)"
  | Rip (s, d) -> Printf.sprintf "%s%+d(%%rip)" s d
  | Got s -> s ^ "@GOTPCREL(%rip)"
  | Plt s -> s ^ "@PLT"
  | Tpoff (r, s) -> Printf.sprintf "%s@tpoff(%s)" s (reg64 r)
  | Gottpoff s -> s ^ "@gottpoff(%rip)"
  | Fs_zero -> "%fs:0"

let cc = function
  | CE -> "e" | CNE -> "ne" | CL -> "l" | CLE -> "le" | CG -> "g" | CGE -> "ge" | CB -> "b" | CBE -> "be"
  | CA -> "a" | CAE -> "ae" | CO -> "o" | CNO -> "no" | CP -> "p" | CNP -> "np" | CS -> "s" | CNS -> "ns"

let escape s =
  let b = Buffer.create (String.length s * 2) in
  String.iter (fun c ->
      match c with
      | '"' -> Buffer.add_string b "\\\""
      | '\\' -> Buffer.add_string b "\\\\"
      | '\n' -> Buffer.add_string b "\\n"
      | c when Char.code c < 32 || Char.code c >= 127 -> Buffer.add_string b (Printf.sprintf "\\%03o" (Char.code c))
      | c -> Buffer.add_char b c) s;
  Buffer.contents b

let rec instr ppf i =
  let p fmt = Format.fprintf ppf fmt in
  match i with
  | Mov (w, a, b) -> p "\tmov%s\t%s, %s" (suffix w) (operand w a) (operand w b)
  | Movabs (v, r) -> p "\tmovabsq\t$%Ld, %s" v (reg64 r)
  | Movsx (f, t, a, b) ->
      (* movslq is spelled specially; the rest are movs<from><to> *)
      p "\tmovs%s%s\t%s, %s" (suffix f) (suffix t) (operand f a) (operand t b)
  | Movzx (f, t, a, b) -> p "\tmovz%s%s\t%s, %s" (suffix f) (suffix t) (operand f a) (operand t b)
  | Lea (a, r) -> p "\tleaq\t%s, %s" (operand Q a) (reg64 r)
  | Alu (m, w, a, b) -> p "\t%s%s\t%s, %s" m (suffix w) (operand w a) (operand w b)
  | Unary (m, w, a) -> p "\t%s%s\t%s" m (suffix w) (operand w a)
  | Shift (m, w, Reg RCX, b) -> p "\t%s%s\t%%cl, %s" m (suffix w) (operand w b)
  | Shift (m, w, a, b) -> p "\t%s%s\t%s, %s" m (suffix w) (operand B a) (operand w b)
  | Cqo -> p "\tcqto"
  | Cdq -> p "\tcltd"
  | Idiv (w, a) -> p "\tidiv%s\t%s" (suffix w) (operand w a)
  | Div (w, a) -> p "\tdiv%s\t%s" (suffix w) (operand w a)
  | Setcc (c, a) -> p "\tset%s\t%s" (cc c) (operand B a)
  | Jmp l -> p "\tjmp\t%s" l
  | Jmp_indirect (Reg r) -> p "\tjmp\t*%s" (reg64 r)
  | Jmp_indirect a -> p "\tjmp\t*%s" (operand Q a)
  | Jcc (c, l) -> p "\tj%s\t%s" (cc c) l
  | Call (Reg r) -> p "\tcall\t*%s" (reg64 r)
  | Call (Rip (s, 0)) -> p "\tcall\t%s" s
  | Call a -> p "\tcall\t%s" (operand Q a)
  | Ret -> p "\tret"
  | Push a -> p "\tpushq\t%s" (operand Q a)
  | Pop a -> p "\tpopq\t%s" (operand Q a)
  | Sse (m, a, b) ->
      (* cvtsi2sdl and cvttsd2sil take a 32-bit general register *)
      let w = if String.length m > 3 && String.sub m 0 3 = "cvt" && m.[String.length m - 1] = 'l' then L else Q in
      p "\t%s\t%s, %s" m (operand w a) (operand w b)
  | Xchg (w, a, b) -> p "\txchg%s\t%s, %s" (suffix w) (operand w a) (operand w b)
  | Lock i -> p "\tlock\n"; instr ppf i
  | Mfence -> p "\tmfence"
  | Rep_movsb -> p "\trep movsb"
  | Rep_stosb -> p "\trep stosb"
  | Ud2 -> p "\tud2"
  | Label l -> p "%s:" l
  | Raw s -> p "%s" s
  | Comment s -> p "\t# %s" s
  | Cfi d -> p "\t.cfi_%s" d
  | File (n, name) -> p "\t.file\t%d \"%s\"" n (escape name)
  | Loc (n, line) -> p "\t.loc\t%d %d" n line

let section_directive = function
  | Data -> "\t.data"
  | Bss -> "\t.bss"
  | Rodata -> "\t.section .rodata"
  | Tdata -> "\t.section .tdata,\"awT\",@progbits"
  | Tbss -> "\t.section .tbss,\"awT\",@nobits"

let data ppf (d : data) =
  let p fmt = Format.fprintf ppf fmt in
  p "%s@." (section_directive d.section);
  if d.dglobal then p "\t.globl\t%s@." d.dname;
  p "\t.align\t%d@." d.dalign;
  p "\t.type\t%s, @%s@." d.dname (match d.section with Tdata | Tbss -> "tls_object" | _ -> "object");
  p "\t.size\t%s, %d@." d.dname d.size;
  p "%s:@." d.dname;
  List.iter (function
      | Bytes s -> p "\t.ascii\t\"%s\"@." (escape s)
      | Zeros n -> p "\t.zero\t%d@." n
      | Quad_sym (s, 0L) -> p "\t.quad\t%s@." s
      | Quad_sym (s, o) -> p "\t.quad\t%s%+Ld@." s o
      | Quad v -> p "\t.quad\t%Ld@." v
      | Long v -> p "\t.long\t%ld@." v
      | Long_diff (a, b) -> p "\t.long\t%s - %s@." a b) d.items

let func ppf (f : func) =
  let p fmt = Format.fprintf ppf fmt in
  p "\t.text@.";
  if f.global then p "\t.globl\t%s@." f.name;
  p "\t.type\t%s, @function@." f.name;
  p "%s:@." f.name;
  if f.debug <> None then p ".LFB.%s:@." f.name;
  List.iter (fun i -> instr ppf i; p "@.") f.body;
  if f.debug <> None then p ".LFE.%s:@." f.name;
  p "\t.size\t%s, .-%s@." f.name f.name

(* ---- DWARF 4 (.debug_info and .debug_abbrev) --------------------------------

   The assembler builds .debug_line from the .file/.loc directives and
   .eh_frame from the .cfi directives; what remains is the tree of
   debugging information entries: one compile unit, a subprogram per
   function with its formal parameters, and the types they mention.
   Abbreviation codes are fixed: 1 compile unit, 2 subprogram with a
   return type, 3 subprogram returning void, 4 formal parameter, 5 base
   type, 6 pointer type, 7 structure declaration, 8 union declaration. *)

let dwarf_abbrevs = [
  (* code, tag, has children, [attribute, form] *)
  1, 0x11, true,  [ 0x25, 0x08; 0x13, 0x0b; 0x03, 0x08; 0x1b, 0x08; 0x11, 0x01; 0x12, 0x07; 0x10, 0x17 ];
  2, 0x2e, true,  [ 0x3f, 0x0c; 0x03, 0x08; 0x3a, 0x0b; 0x3b, 0x05; 0x49, 0x13; 0x11, 0x01; 0x12, 0x07; 0x40, 0x18 ];
  3, 0x2e, true,  [ 0x3f, 0x0c; 0x03, 0x08; 0x3a, 0x0b; 0x3b, 0x05; 0x11, 0x01; 0x12, 0x07; 0x40, 0x18 ];
  4, 0x05, false, [ 0x03, 0x08; 0x49, 0x13; 0x02, 0x18 ];
  5, 0x24, false, [ 0x0b, 0x0b; 0x3e, 0x0b; 0x03, 0x08 ];
  6, 0x0f, false, [ 0x0b, 0x0b ];
  7, 0x13, false, [ 0x03, 0x08; 0x3c, 0x19 ];
  8, 0x17, false, [ 0x03, 0x08; 0x3c, 0x19 ];
]

let sleb128_size v =
  let rec go v n = let v' = Int64.shift_right v 7 in if (v' = 0L && Int64.logand v 0x40L = 0L) || (v' = -1L && Int64.logand v 0x40L <> 0L) then n + 1 else go v' (n + 1) in
  go (Int64.of_int v) 0

let debug_info ppf (prog : program) source =
  let p fmt = Format.fprintf ppf fmt in
  (* one DIE per distinct type, labelled for ref4 references *)
  let types = Hashtbl.create 16 in
  let type_label t =
    match t with
    | Dw_void -> None
    | _ -> (match Hashtbl.find_opt types t with
        | Some l -> Some l
        | None -> let l = Printf.sprintf ".Ltype%d" (Hashtbl.length types) in Hashtbl.replace types t l; Some l) in
  let funcs = List.filter_map (fun f -> Option.map (fun d -> f, d) f.debug) prog.funcs in
  (* collect types first so their labels exist when parameters refer to them *)
  List.iter (fun (_, d) -> ignore (type_label d.dret); List.iter (fun pr -> ignore (type_label pr.ptype)) d.dparams) funcs;
  p "\t.section .debug_info,\"\",@progbits@.";
  p ".Ldebug_info0:@.";
  p "\t.long\t.Ldebug_info_end - .Ldebug_info_start@.";
  p ".Ldebug_info_start:@.";
  p "\t.value\t4@.\t.long\t.Ldebug_abbrev0@.\t.byte\t8@.";
  (* compile unit *)
  p "\t.uleb128 1@.\t.string\t\"occ 0.1\"@.\t.byte\t0x0c@.\t.string\t\"%s\"@.\t.string\t\"%s\"@." (escape source) (escape (Sys.getcwd ()));
  p "\t.quad\t.Ltext0@.\t.quad\t.Letext0-.Ltext0@.\t.long\t.Ldebug_line0@.";
  List.iter (fun (f, d) ->
      let ret = type_label d.dret in
      p "\t.uleb128 %d@." (if ret = None then 3 else 2);
      p "\t.byte\t%d@.\t.string\t\"%s\"@.\t.byte\t%d@.\t.value\t%d@." (if f.global then 1 else 0) (escape f.name) d.dfile d.dline;
      (match ret with Some l -> p "\t.long\t%s - .Ldebug_info0@." l | None -> ());
      p "\t.quad\t.LFB.%s@.\t.quad\t.LFE.%s - .LFB.%s@." f.name f.name f.name;
      p "\t.uleb128 1@.\t.byte\t0x9c@."; (* frame base: DW_OP_call_frame_cfa *)
      List.iter (fun pr ->
          match type_label pr.ptype with
          | None -> ()
          | Some l ->
              p "\t.uleb128 4@.\t.string\t\"%s\"@.\t.long\t%s - .Ldebug_info0@." (escape pr.pname) l;
              (match pr.ploc with
               | At_cfa_offset off -> p "\t.uleb128 %d@.\t.byte\t0x91@.\t.sleb128 %d@." (1 + sleb128_size off) off
               | In_register n -> p "\t.uleb128 1@.\t.byte\t0x%x@." (0x50 + n))) d.dparams;
      p "\t.byte\t0@." (* end of children *)) funcs;
  Hashtbl.iter (fun t l ->
      p "%s:@." l;
      match t with
      | Dw_base (name, enc, size) -> p "\t.uleb128 5@.\t.byte\t%d@.\t.byte\t%d@.\t.string\t\"%s\"@." size enc (escape name)
      | Dw_pointer -> p "\t.uleb128 6@.\t.byte\t8@."
      | Dw_struct name -> p "\t.uleb128 7@.\t.string\t\"%s\"@." (escape name)
      | Dw_union name -> p "\t.uleb128 8@.\t.string\t\"%s\"@." (escape name)
      | Dw_void -> ()) types;
  p "\t.byte\t0@."; (* end of the compile unit's children *)
  p ".Ldebug_info_end:@.";
  p "\t.section .debug_abbrev,\"\",@progbits@.";
  p ".Ldebug_abbrev0:@.";
  List.iter (fun (code, tag, children, attrs) ->
      p "\t.uleb128 %d@.\t.uleb128 0x%x@.\t.byte\t%d@." code tag (if children then 1 else 0);
      List.iter (fun (a, f) -> p "\t.uleb128 0x%x@.\t.uleb128 0x%x@." a f) attrs;
      p "\t.byte\t0@.\t.byte\t0@.") dwarf_abbrevs;
  p "\t.byte\t0@.";
  p "\t.section .debug_line,\"\",@progbits@.";
  p ".Ldebug_line0:@."

let program ppf (prog : program) =
  (* the .file table comes first, before any .loc refers to it *)
  List.iter (fun (n, name) -> Format.fprintf ppf "\t.file\t%d \"%s\"@." n (escape name)) prog.files;
  List.iter (data ppf) prog.data;
  if prog.source <> None then Format.fprintf ppf "\t.text@..Ltext0:@.";
  List.iter (func ppf) prog.funcs;
  (match prog.source with
   | Some source ->
       Format.fprintf ppf "\t.text@..Letext0:@.";
       debug_info ppf prog source
   | None -> ());
  Format.fprintf ppf "\t.section .note.GNU-stack,\"\",@progbits@."
