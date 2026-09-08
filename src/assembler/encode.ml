(* Machine-code encoding of x86-64 instructions (Intel SDM volume 2,
   chapter 2 "Instruction format", and the per-instruction tables).

   An instruction is: legacy prefixes, an optional REX prefix, the opcode
   (one to three bytes), an optional ModRM byte with its SIB byte and
   displacement, and an optional immediate.  Anything that refers to a
   symbol is left as zero bytes plus a fixup, which the assembler
   resolves after layout, either to a value or to a relocation.

   Where the architecture offers several encodings for one instruction,
   the choices below follow GNU as, so the two assemblers can be compared
   byte for byte.  Only the instructions the producers use are here. *)

open Gas

(* ---- Fixups ------------------------------------------------------------- *)

type fixup = {
  at : int;              (* offset of the field within the instruction *)
  size : int;            (* 1, 2, 4 or 8 bytes *)
  target : expr;         (* symbol plus offset; its @modifier picks the relocation *)
  pcrel : bool;          (* relative to [pcbase] (rip-relative, call) *)
  pcbase : int;          (* offset the value is relative to: the end of the instruction *)
  signed : bool;         (* a 32-bit absolute value is sign-extended (R_X86_64_32S) *)
  relaxable : bool;      (* a GOTPCREL load the linker may relax to a direct reference *)
  branch : bool;         (* the target of a call or jump: relocates as PLT32 *)
}

(* jmp and jcc to a label in the same section have a 2-byte form with an
   8-bit displacement and a longer form with a 32-bit one; the assembler
   picks after layout ("branch relaxation"). *)
type encoded =
  | Fixed of string * fixup list
  | Branch of { short : string; long : string; target : expr }

exception Bad of string
let bad fmt = Printf.ksprintf (fun s -> raise (Bad s)) fmt

(* ---- Sizes and condition codes ----------------------------------------- *)

type size = S8 | S16 | S32 | S64

let size_of_suffix = function 'b' -> Some S8 | 'w' -> Some S16 | 'l' -> Some S32 | 'q' -> Some S64 | _ -> None
let size_of_reg r = match r.rwidth with 8 -> S8 | 16 -> S16 | 32 -> S32 | _ -> S64
let bytes_of_size = function S8 -> 1 | S16 -> 2 | S32 -> 4 | S64 -> 8

let condition_codes = [
  "o", 0; "no", 1; "b", 2; "c", 2; "nae", 2; "ae", 3; "nb", 3; "nc", 3; "e", 4; "z", 4;
  "ne", 5; "nz", 5; "be", 6; "na", 6; "a", 7; "nbe", 7; "s", 8; "ns", 9; "p", 10; "pe", 10;
  "np", 11; "po", 11; "l", 12; "nge", 12; "ge", 13; "nl", 13; "le", 14; "ng", 14; "g", 15; "nle", 15 ]

(* ---- The instruction under construction -------------------------------- *)

type builder = {
  buf : Buffer.t;
  mutable fixups : fixup list;
}

let byte b v = Buffer.add_char b.buf (Char.chr (v land 0xff))
let bytes b l = List.iter (byte b) l
let imm_bytes b v n = for i = 0 to n - 1 do byte b (Int64.to_int (Int64.shift_right_logical v (8 * i))) done

let fits_int8 v = v >= -128L && v <= 127L
let fits_int32 v = v >= -2147483648L && v <= 2147483647L

(* Is the expression a plain number?  Symbolic values become fixups. *)
let rec const = function
  | Num v -> Some v
  | Neg e -> Option.map Int64.neg (const e)
  | Not e -> Option.map Int64.lognot (const e)
  | Bin (op, x, y) ->
      (match const x, const y with
       | Some a, Some b ->
           Some (match op with
               | Add -> Int64.add a b | Sub -> Int64.sub a b | Mul -> Int64.mul a b
               | Div -> Int64.div a b | Mod -> Int64.rem a b | And -> Int64.logand a b
               | Or -> Int64.logor a b | Xor -> Int64.logxor a b
               | Shl -> Int64.shift_left a (Int64.to_int b) | Shr -> Int64.shift_right_logical a (Int64.to_int b))
       | _ -> None)
  | Sym _ | Dot -> None

(* ---- ModRM, SIB and displacement (SDM 2.1.3 to 2.1.5) ------------------ *)

(* The r/m operand: a register, or a memory reference.  [reg_field] is the
   3-bit value for the ModRM.reg field (a register number or an opcode
   extension).  Returns the REX.X and REX.B bits the operand needs. *)
type rm = RmReg of int | RmMem of mem

let rex_bits_of_rm = function
  | RmReg n -> (false, n >= 8)
  | RmMem m ->
      let ext = function Some r when r.rclass = Gpr -> r.rnum >= 8 | _ -> false in
      (ext m.index, ext m.base)

let emit_modrm b ~reg_field ~relaxable rm =
  let modrm md reg rmv = byte b ((md lsl 6) lor ((reg land 7) lsl 3) lor (rmv land 7)) in
  match rm with
  | RmReg n -> modrm 3 reg_field n
  | RmMem m ->
      let scale_bits = match m.scale with 1 -> 0 | 2 -> 1 | 4 -> 2 | _ -> 3 in
      let disp_const = match m.disp with None -> Some 0L | Some e -> const e in
      let disp32 ?(pcrel = false) () =
        (match m.disp, disp_const with
         | _, Some v -> imm_bytes b v 4
         | Some e, None ->
             b.fixups <- { at = Buffer.length b.buf; size = 4; target = e; pcrel; pcbase = 0; signed = true; relaxable; branch = false } :: b.fixups;
             imm_bytes b 0L 4
         | None, None -> assert false) in
      (* mod for a base register: 00 if no displacement (not for rbp/r13),
         01 with disp8, 10 with disp32 *)
      let mode base_num =
        match disp_const with
        | Some 0L when base_num land 7 <> 5 -> 0
        | Some v when fits_int8 v -> 1
        | _ -> 2 in
      let disp_for md = match md with 0 -> () | 1 -> imm_bytes b (Option.get disp_const) 1 | _ -> disp32 () in
      (match m.base, m.index with
       | Some { rclass = Rip; _ }, None -> modrm 0 reg_field 5; disp32 ~pcrel:true ()
       | Some { rclass = Rip; _ }, Some _ -> bad "rip-relative addressing takes no index"
       | None, None -> modrm 0 reg_field 4; byte b 0x25; disp32 ()
       | Some base, None when base.rnum land 7 <> 4 ->
           let md = mode base.rnum in
           modrm md reg_field base.rnum; disp_for md
       | Some base, None ->
           (* rsp and r12 as a base need a SIB byte with no index *)
           let md = mode base.rnum in
           modrm md reg_field 4; byte b (0x20 lor (base.rnum land 7)); disp_for md
       | None, Some index ->
           if index.rnum = 4 then bad "rsp cannot be an index register";
           modrm 0 reg_field 4; byte b ((scale_bits lsl 6) lor ((index.rnum land 7) lsl 3) lor 5); disp32 ()
       | Some base, Some index ->
           if index.rnum = 4 then bad "rsp cannot be an index register";
           let md = mode base.rnum in
           modrm md reg_field 4;
           byte b ((scale_bits lsl 6) lor ((index.rnum land 7) lsl 3) lor (base.rnum land 7));
           disp_for md)

(* ---- Assembling the pieces --------------------------------------------- *)

type pieces = {
  legacy : int list;                 (* prefixes before REX, in order *)
  w : bool;                          (* REX.W *)
  reg : int;                         (* the ModRM.reg field, or the register in the opcode *)
  rm : rm option;
  force_rex : bool;                  (* sil/dil/spl/bpl need a REX even with no bits set *)
  opcode : int list;
  imm : (expr * int * bool) option;  (* value, size in bytes, sign-extended *)
  relaxable : bool;
}

let default = { legacy = []; w = false; reg = 0; rm = None; force_rex = false; opcode = []; imm = None; relaxable = false }

let build p =
  let b = { buf = Buffer.create 16; fixups = [] } in
  bytes b p.legacy;
  let x, bb = match p.rm with Some rm -> rex_bits_of_rm rm | None -> (false, false) in
  let rex_r = p.rm <> None && p.reg >= 8 in
  (* a register encoded in the opcode byte itself uses REX.B *)
  let bb = bb || (p.rm = None && p.reg >= 8) in
  let rex = (if p.w then 8 else 0) lor (if rex_r then 4 else 0) lor (if x then 2 else 0) lor (if bb then 1 else 0) in
  if rex <> 0 || p.force_rex then byte b (0x40 lor rex);
  bytes b p.opcode;
  (match p.rm with Some rm -> emit_modrm b ~reg_field:p.reg ~relaxable:p.relaxable rm | None -> ());
  (match p.imm with
   | None -> ()
   | Some (e, n, signed) ->
       (match const e with
        | Some v -> imm_bytes b v n
        | None ->
            b.fixups <- { at = Buffer.length b.buf; size = n; target = e; pcrel = false; pcbase = 0; signed; relaxable = false; branch = false } :: b.fixups;
            imm_bytes b 0L n));
  let len = Buffer.length b.buf in
  Fixed (Buffer.contents b.buf, List.rev_map (fun f -> { f with pcbase = len }) b.fixups)

(* ---- Operand helpers ----------------------------------------------------- *)

let is_gpr = function Reg { rclass = Gpr; _ } -> true | _ -> false
let is_xmm = function Reg { rclass = Xmm; _ } -> true | _ -> false
let is_mem = function Mem _ -> true | _ -> false
let is_imm = function Imm _ -> true | _ -> false

let gpr = function Reg ({ rclass = Gpr; _ } as r) -> r | _ -> bad "expected a general register"
let xmm = function Reg ({ rclass = Xmm; _ } as r) -> r | _ -> bad "expected an xmm register"

(* an r/m operand, plus the segment and force-REX facts it carries *)
(* spl, bpl, sil and dil exist only with a REX prefix; ah, ch, dh and bh
   only without one *)
let low_byte_reg (r : reg) = r.rwidth = 8 && r.rnum >= 4 && r.rnum < 8 && r.rname.[1] <> 'h'

let rm_of size = function
  | Reg ({ rclass = Gpr; _ } as r) ->
      RmReg r.rnum, [], (size = S8 && low_byte_reg r)
  | Reg ({ rclass = Xmm; _ } as r) -> RmReg r.rnum, [], false
  | Mem m ->
      let seg = match m.seg with Some { rnum = 4; _ } -> [ 0x64 ] | Some { rnum = 5; _ } -> [ 0x65 ] | _ -> [] in
      RmMem m, seg, false
  | _ -> bad "expected a register or memory operand"

let byte_reg_needs_rex size = function
  | Reg ({ rclass = Gpr; _ } as r) -> size = S8 && low_byte_reg r
  | _ -> false

(* the operand size of an integer instruction: from the suffix, else from a
   general register operand *)
let operand_size suffix ops =
  match suffix with
  | Some s -> s
  | None ->
      (match List.find_opt is_gpr ops with
       | Some (Reg r) -> size_of_reg r
       | _ -> bad "ambiguous operand size: add a b/w/l/q suffix")

let size_prefix = function S16 -> [ 0x66 ] | _ -> []

(* opcode for an 8-bit or wider form: the SDM lists "op r/m8" and "op r/m16/32/64"
   as adjacent opcodes *)
let wide size op = if size = S8 then op else op + 1

(* Immediate operands are sign-extended to the operand size; gas rejects
   values that do not fit, and so do we. *)
let imm_fits size v =
  match size with
  | S8 -> v >= -128L && v <= 255L
  | S16 -> v >= -32768L && v <= 65535L
  | S32 -> v >= -2147483648L && v <= 4294967295L
  | S64 -> fits_int32 v

let imm_field size e =
  (* immediates are at most 32 bits, sign-extended for 64-bit operands *)
  let n = min 4 (bytes_of_size size) in
  (match const e with
   | Some v when not (imm_fits size v) -> bad "immediate %Ld does not fit" v
   | _ -> ());
  Some (e, n, size = S64)

(* ---- Instruction families ---------------------------------------------- *)

(* the "op r/m, reg" and "op reg, r/m" pair, e.g. mov 88/89/8A/8B *)
let two_operand ~store_op ~load_op size src dst =
  match src, dst with
  | _, Reg r when is_gpr dst && (is_mem src) ->
      let rm, seg, frex = rm_of size src in
      build { default with legacy = seg @ size_prefix size; w = (size = S64); reg = r.rnum; rm = Some rm;
                           force_rex = frex || byte_reg_needs_rex size dst; opcode = [ wide size load_op ]; relaxable = true }
  | Reg r, _ when is_gpr src ->
      let rm, seg, frex = rm_of size dst in
      build { default with legacy = seg @ size_prefix size; w = (size = S64); reg = r.rnum; rm = Some rm;
                           force_rex = frex || byte_reg_needs_rex size src; opcode = [ wide size store_op ] }
  | _ -> bad "bad operand combination"

(* add/or/adc/sbb/and/sub/xor/cmp: opcode group with base 00,08,...,38 and
   the immediate forms 80/81/83 with the group number in ModRM.reg *)
let alu group size src dst =
  let base = group * 8 in
  match src with
  | Imm e ->
      let rm, seg, frex = rm_of size dst in
      let v = const e in
      let short_imm8 = size <> S8 && (match v with Some v -> fits_int8 v | None -> false) in
      (match dst with
       | Reg { rnum = 0; _ } when not short_imm8 ->
           (* the accumulator form: op al/ax/eax/rax, imm *)
           build { default with legacy = size_prefix size; w = (size = S64); opcode = [ wide size (base + 4) ]; imm = imm_field size e }
       | _ ->
           let opcode, imm =
             if size = S8 then 0x80, imm_field S8 e
             else if short_imm8 then 0x83, Some (e, 1, true)
             else 0x81, imm_field size e in
           build { default with legacy = seg @ size_prefix size; w = (size = S64); reg = group; rm = Some rm;
                                force_rex = frex; opcode = [ opcode ]; imm })
  | _ -> two_operand ~store_op:base ~load_op:(base + 2) size src dst

let mov size src dst =
  match src, dst with
  | Imm e, Reg r when is_gpr dst ->
      let v = const e in
      (match size, v with
       | S64, Some v when not (fits_int32 v) ->
           (* movabs: the only 64-bit immediate *)
           build { default with w = true; reg = r.rnum; opcode = [ 0xB8 + (r.rnum land 7) ]; imm = Some (e, 8, false) }
       | S64, _ ->
           build { default with w = true; reg = 0; rm = Some (RmReg r.rnum); opcode = [ 0xC7 ]; imm = imm_field S64 e }
       | _ ->
           build { default with legacy = size_prefix size; reg = r.rnum; force_rex = byte_reg_needs_rex size dst;
                                opcode = [ (if size = S8 then 0xB0 else 0xB8) + (r.rnum land 7) ]; imm = imm_field size e })
  | Imm e, Mem _ ->
      let rm, seg, _ = rm_of size dst in
      build { default with legacy = seg @ size_prefix size; w = (size = S64); reg = 0; rm = Some rm;
                           opcode = [ wide size 0xC6 ]; imm = imm_field size e }
  | _ -> two_operand ~store_op:0x88 ~load_op:0x8A size src dst

let movabs src dst =
  match src, dst with
  | Imm e, Reg r -> build { default with w = true; reg = r.rnum; opcode = [ 0xB8 + (r.rnum land 7) ]; imm = Some (e, 8, false) }
  | _ -> bad "movabs takes an immediate and a register"

(* movzx/movsx: 0F B6/B7 and 0F BE/BF, plus movslq = movsxd 63 *)
let movx ~sign from_size to_size src dst =
  let r = gpr dst in
  let rm, seg, frex = rm_of from_size src in
  let opcode =
    match from_size with
    | S8 -> [ 0x0F; (if sign then 0xBE else 0xB6) ]
    | S16 -> [ 0x0F; (if sign then 0xBF else 0xB7) ]
    | S32 when sign -> [ 0x63 ]
    | _ -> bad "no zero-extension from 32 bits: use movl" in
  build { default with legacy = seg @ size_prefix to_size; w = (to_size = S64); reg = r.rnum; rm = Some rm;
                       force_rex = frex; opcode }

let lea src dst =
  let r = gpr dst in
  let rm, seg, _ = rm_of S64 src in
  if not (is_mem src) then bad "lea needs a memory operand";
  let size = size_of_reg r in
  build { default with legacy = seg @ size_prefix size; w = (size = S64); reg = r.rnum; rm = Some rm; opcode = [ 0x8D ] }

(* F6/F7 group: test /0, not /2, neg /3, mul /4, imul /5, div /6, idiv /7 *)
let group3 ext size op =
  let rm, seg, frex = rm_of size op in
  build { default with legacy = seg @ size_prefix size; w = (size = S64); reg = ext; rm = Some rm; force_rex = frex;
                       opcode = [ wide size 0xF6 ] }

let test size src dst =
  match src, dst with
  | Imm e, Reg { rnum = 0; _ } ->
      build { default with legacy = size_prefix size; w = (size = S64); opcode = [ wide size 0xA8 ]; imm = imm_field size e }
  | Imm e, _ ->
      let rm, seg, frex = rm_of size dst in
      build { default with legacy = seg @ size_prefix size; w = (size = S64); reg = 0; rm = Some rm; force_rex = frex;
                           opcode = [ wide size 0xF6 ]; imm = imm_field size e }
  | Reg r, _ when is_gpr src ->
      let rm, seg, frex = rm_of size dst in
      build { default with legacy = seg @ size_prefix size; w = (size = S64); reg = r.rnum; rm = Some rm;
                           force_rex = frex || byte_reg_needs_rex size src; opcode = [ wide size 0x84 ] }
  | _, Reg r when is_gpr dst -> (* test mem, reg is the same instruction *)
      let rm, seg, frex = rm_of size src in
      build { default with legacy = seg @ size_prefix size; w = (size = S64); reg = r.rnum; rm = Some rm;
                           force_rex = frex; opcode = [ wide size 0x84 ]; relaxable = true }
  | _ -> bad "bad operands for test"

(* FE/FF group: inc /0, dec /1, call /2, jmp /4, push /6 *)
let group5 ext size op =
  let rm, seg, frex = rm_of size op in
  build { default with legacy = seg @ size_prefix size; w = (size = S64); reg = ext; rm = Some rm; force_rex = frex;
                       opcode = [ wide size 0xFE ] }

(* shifts and rotates: rol /0, ror /1, shl /4, shr /5, sar /7 *)
let shift ext size count dst =
  let rm, seg, frex = rm_of size dst in
  let common = { default with legacy = seg @ size_prefix size; w = (size = S64); reg = ext; rm = Some rm; force_rex = frex } in
  match count with
  | None | Some (Imm (Num 1L)) -> build { common with opcode = [ wide size 0xD0 ] }
  | Some (Imm e) -> build { common with opcode = [ wide size 0xC0 ]; imm = Some (e, 1, false) }
  | Some (Reg { rclass = Gpr; rnum = 1; rwidth = 8; _ }) -> build { common with opcode = [ wide size 0xD2 ] }
  | Some _ -> bad "shift count must be an immediate or %%cl"

let imul size ops =
  match ops with
  | [ src ] -> group3 5 size src
  | [ src; dst ] when not (is_imm src) ->
      let r = gpr dst in
      let rm, seg, _ = rm_of size src in
      build { default with legacy = seg @ size_prefix size; w = (size = S64); reg = r.rnum; rm = Some rm; opcode = [ 0x0F; 0xAF ] }
  | [ Imm e; dst ] -> (* imul $imm, %r  is  imul $imm, %r, %r *)
      let r = gpr dst in
      let rm, seg, _ = rm_of size dst in
      let short = match const e with Some v -> fits_int8 v | None -> false in
      build { default with legacy = seg @ size_prefix size; w = (size = S64); reg = r.rnum; rm = Some rm;
                           opcode = [ (if short then 0x6B else 0x69) ]; imm = (if short then Some (e, 1, true) else imm_field size e) }
  | [ Imm e; src; dst ] ->
      let r = gpr dst in
      let rm, seg, _ = rm_of size src in
      let short = match const e with Some v -> fits_int8 v | None -> false in
      build { default with legacy = seg @ size_prefix size; w = (size = S64); reg = r.rnum; rm = Some rm;
                           opcode = [ (if short then 0x6B else 0x69) ]; imm = (if short then Some (e, 1, true) else imm_field size e) }
  | _ -> bad "bad operands for imul"

let setcc cc op =
  let rm, seg, frex = rm_of S8 op in
  build { default with legacy = seg; reg = 0; rm = Some rm; force_rex = frex; opcode = [ 0x0F; 0x90 + cc ] }

let cmovcc cc size src dst =
  let r = gpr dst in
  let rm, seg, _ = rm_of size src in
  build { default with legacy = seg @ size_prefix size; w = (size = S64); reg = r.rnum; rm = Some rm; opcode = [ 0x0F; 0x40 + cc ] }

let push_pop ~reg_op ~mem_op ~mem_ext ~imm_ops op =
  match op with
  | Reg r when is_gpr op -> build { default with reg = r.rnum; opcode = [ reg_op + (r.rnum land 7) ] }
  | Mem _ -> let rm, seg, _ = rm_of S64 op in build { default with legacy = seg; reg = mem_ext; rm = Some rm; opcode = [ mem_op ] }
  | Imm e ->
      (match imm_ops, const e with
       | Some (short, _), Some v when fits_int8 v -> build { default with opcode = [ short ]; imm = Some (e, 1, true) }
       | Some (_, long), _ -> build { default with opcode = [ long ]; imm = Some (e, 4, true) }
       | None, _ -> bad "pop takes no immediate")
  | _ -> bad "bad operand for push/pop"

(* the target of a direct call or jump: a symbol, possibly @PLT *)
let target_of = function
  | Mem { disp = Some e; base = None; index = None; seg = None; _ } | Imm e -> e
  | _ -> bad "bad branch target"

let call op =
  match op with
  | Indirect target ->
      let rm, seg, _ = rm_of S64 target in
      build { default with legacy = seg; reg = 2; rm = Some rm; opcode = [ 0xFF ]; relaxable = true }
  | _ ->
      let e = target_of op in
      Fixed ("\xE8\000\000\000\000", [ { at = 1; size = 4; target = e; pcrel = true; pcbase = 5; signed = true; relaxable = false; branch = true } ])

let jmp op =
  match op with
  | Indirect target ->
      let rm, seg, _ = rm_of S64 target in
      build { default with legacy = seg; reg = 4; rm = Some rm; opcode = [ 0xFF ]; relaxable = true }
  | _ -> Branch { short = "\xEB"; long = "\xE9"; target = target_of op }

let jcc cc op = Branch { short = String.make 1 (Char.chr (0x70 + cc)); long = "\x0F" ^ String.make 1 (Char.chr (0x80 + cc)); target = target_of op }

let xchg size a b =
  match a, b with
  | Reg { rnum = 0; _ }, Reg r when size <> S8 && is_gpr a && is_gpr b && not (size = S32 && r.rnum = 0) ->
      build { default with legacy = size_prefix size; w = (size = S64); reg = r.rnum; opcode = [ 0x90 + (r.rnum land 7) ] }
  | Reg r, Reg { rnum = 0; _ } when size <> S8 && is_gpr a && is_gpr b && not (size = S32 && r.rnum = 0) ->
      build { default with legacy = size_prefix size; w = (size = S64); reg = r.rnum; opcode = [ 0x90 + (r.rnum land 7) ] }
  | Reg r, _ when is_gpr a ->
      let rm, seg, frex = rm_of size b in
      build { default with legacy = seg @ size_prefix size; w = (size = S64); reg = r.rnum; rm = Some rm;
                           force_rex = frex || byte_reg_needs_rex size a; opcode = [ wide size 0x86 ] }
  | _, Reg r when is_gpr b ->
      let rm, seg, frex = rm_of size a in
      build { default with legacy = seg @ size_prefix size; w = (size = S64); reg = r.rnum; rm = Some rm;
                           force_rex = frex || byte_reg_needs_rex size b; opcode = [ wide size 0x86 ] }
  | _ -> bad "bad operands for xchg"

(* cmpxchg 0F B0/B1 and xadd 0F C0/C1: "op reg, r/m" *)
let reg_to_rm opcode size src dst =
  let r = gpr src in
  let rm, seg, frex = rm_of size dst in
  build { default with legacy = seg @ size_prefix size; w = (size = S64); reg = r.rnum; rm = Some rm;
                       force_rex = frex || byte_reg_needs_rex size src; opcode = [ 0x0F; wide size opcode ] }

(* ---- SSE ------------------------------------------------------------------ *)

(* prefix 0F opcode /r with an xmm register in ModRM.reg *)
let sse ?(imm = "") prefix opcode ~w ~reg rm_op =
  let rm, seg, _ = rm_of S64 rm_op in
  match build { default with legacy = seg @ prefix; w; reg; rm = Some rm; opcode = 0x0F :: opcode } with
  | Fixed (s, f) -> Fixed (s ^ imm, f)
  | Branch _ -> assert false

(* xmm <- xmm/mem *)
let sse_load prefix opcode src dst = sse prefix [ opcode ] ~w:false ~reg:(xmm dst).rnum src

(* load/store pairs such as movsd 10/11: the register form uses the load opcode *)
let sse_move prefix load store src dst =
  if is_xmm dst then sse prefix [ load ] ~w:false ~reg:(xmm dst).rnum src
  else sse prefix [ store ] ~w:false ~reg:(xmm src).rnum dst

(* an 8-bit immediate after ModRM, as in roundsd and cmpsd *)
let imm8 = function
  | Imm e -> (match const e with Some v -> String.make 1 (Char.chr (Int64.to_int v land 0xff)) | None -> bad "expected a constant")
  | _ -> bad "expected an immediate"

(* ---- x87 (SDM chapter 8 and the D8-DF opcode maps) ----------------------------

   Memory forms are an opcode byte with the operation in ModRM.reg; register
   forms are two bytes with the stack register added to the second.  The
   AT&T spellings of the subtract and divide pop forms are those of GNU as,
   which are the reverse of Intel's: "fsubrp" here is Intel's FSUBP, that
   is st(1) = st(1) - st(0) then pop. *)

let x87_memory = [
  "flds", (0xD9, 0); "fldl", (0xDD, 0); "fldt", (0xDB, 5);
  "fsts", (0xD9, 2); "fstl", (0xDD, 2); "fstps", (0xD9, 3); "fstpl", (0xDD, 3); "fstpt", (0xDB, 7);
  "filds", (0xDF, 0); "fildl", (0xDB, 0); "fildll", (0xDF, 5); "fildq", (0xDF, 5);
  "fistps", (0xDF, 3); "fistpl", (0xDB, 3); "fistpll", (0xDF, 7); "fistpq", (0xDF, 7);
  "fisttps", (0xDF, 1); "fisttpl", (0xDB, 1); "fisttpll", (0xDD, 1); "fisttpq", (0xDD, 1);
  "fadds", (0xD8, 0); "faddl", (0xDC, 0); "fmuls", (0xD8, 1); "fmull", (0xDC, 1);
  "fsubs", (0xD8, 4); "fsubl", (0xDC, 4); "fsubrs", (0xD8, 5); "fsubrl", (0xDC, 5);
  "fdivs", (0xD8, 6); "fdivl", (0xDC, 6); "fdivrs", (0xD8, 7); "fdivrl", (0xDC, 7);
  "fcoms", (0xD8, 2); "fcoml", (0xDC, 2); "fcomps", (0xD8, 3); "fcompl", (0xDC, 3);
  "fnstcw", (0xD9, 7); "fldcw", (0xD9, 5); "fnstsw", (0xDD, 7); "fnstenv", (0xD9, 6); "fldenv", (0xD9, 4);
  "fistl", (0xDB, 2); "fists", (0xDF, 2); "fisttpl", (0xDB, 1);
  "fnsave", (0xDD, 6); "frstor", (0xDD, 4) ]

(* two-operand register arithmetic: "op %st(i), %st" (result in st(0),
   the D8 row) or "op %st, %st(i)" (result in st(i), the DC row, where
   GNU as swaps the sub and div spellings) *)
let x87_binary = [
  "fadd", (0xC0, 0xC0); "fmul", (0xC8, 0xC8); "fsub", (0xE0, 0xE0); "fsubr", (0xE8, 0xE8); "fdiv", (0xF0, 0xF0); "fdivr", (0xF8, 0xF8) ]

let x87_register = [
  "fld", (0xD9, 0xC0); "fxch", (0xD9, 0xC8); "fst", (0xDD, 0xD0); "fstp", (0xDD, 0xD8); "ffree", (0xDD, 0xC0);
  "fcom", (0xD8, 0xD0); "fcomp", (0xD8, 0xD8);
  "faddp", (0xDE, 0xC0); "fmulp", (0xDE, 0xC8); "fsubp", (0xDE, 0xE0); "fsubrp", (0xDE, 0xE8);
  "fdivp", (0xDE, 0xF0); "fdivrp", (0xDE, 0xF8);
  "fucom", (0xDD, 0xE0); "fucomp", (0xDD, 0xE8); "fucomi", (0xDB, 0xE8); "fucomip", (0xDF, 0xE8);
  "fcomi", (0xDB, 0xF0); "fcomip", (0xDF, 0xF0) ]

let x87_plain = [
  "fchs", "\xD9\xE0"; "fabs", "\xD9\xE1"; "ftst", "\xD9\xE4"; "fxam", "\xD9\xE5"; "fld1", "\xD9\xE8";
  "fldl2t", "\xD9\xE9"; "fldl2e", "\xD9\xEA"; "fldpi", "\xD9\xEB"; "fldlg2", "\xD9\xEC"; "fldln2", "\xD9\xED"; "fldz", "\xD9\xEE";
  "f2xm1", "\xD9\xF0"; "fyl2x", "\xD9\xF1"; "fptan", "\xD9\xF2"; "fpatan", "\xD9\xF3"; "fxtract", "\xD9\xF4"; "fprem1", "\xD9\xF5";
  "fdecstp", "\xD9\xF6"; "fincstp", "\xD9\xF7"; "fprem", "\xD9\xF8"; "fyl2xp1", "\xD9\xF9"; "fsqrt", "\xD9\xFA";
  "fsincos", "\xD9\xFB"; "frndint", "\xD9\xFC"; "fscale", "\xD9\xFD"; "fsin", "\xD9\xFE"; "fcos", "\xD9\xFF";
  "fucompp", "\xDA\xE9"; "fcompp", "\xDE\xD9"; "fninit", "\xDB\xE3"; "fnclex", "\xDB\xE2"; "fwait", "\x9B"; "fnop", "\xD9\xD0" ]

let is_x87 m = List.mem m [ "fstcw"; "fstsw"; "fclex"; "finit"; "stmxcsr"; "ldmxcsr" ] || List.mem_assoc m x87_memory || List.mem_assoc m x87_register || List.mem_assoc m x87_plain || List.mem_assoc m x87_binary || m = "fnstsw"

let rec x87 mnemonic operands =
  let stack_index ops =
    (* the st(i) operand, if any; "faddp" alone means st(1) *)
    match List.filter_map (function Reg { rclass = X87; rnum; _ } -> Some rnum | _ -> None) ops with
    | [] -> 1
    | [ i ] -> i
    | [ a; b ] -> max a b
    | _ -> bad "too many operands for %s" mnemonic in
  match mnemonic, operands with
  | "fnstsw", [ Reg { rclass = Gpr; rnum = 0; _ } ] -> Fixed ("\xDF\xE0", [])
  | "fstsw", [ Reg { rclass = Gpr; rnum = 0; _ } ] -> Fixed ("\x9B\xDF\xE0", [])
  | ("fstcw" | "fstsw" | "fclex" | "finit"), _ ->
      (* the waiting forms: fwait then the no-wait instruction *)
      let m = "fn" ^ String.sub mnemonic 1 (String.length mnemonic - 1) in
      (match x87 m operands with Fixed (b, f) -> Fixed ("\x9B" ^ b, f) | Branch _ -> assert false)
  | "stmxcsr", [ (Mem _ as m) ] -> let rm, seg, _ = rm_of S64 m in build { default with legacy = seg; reg = 3; rm = Some rm; opcode = [ 0x0F; 0xAE ] }
  | "ldmxcsr", [ (Mem _ as m) ] -> let rm, seg, _ = rm_of S64 m in build { default with legacy = seg; reg = 2; rm = Some rm; opcode = [ 0x0F; 0xAE ] }
  | _, ([ Reg { rclass = X87; rnum = i; _ } ] | [ Reg { rclass = X87; rnum = i; _ }; Reg { rclass = X87; rnum = 0; _ } ])
    when List.mem_assoc mnemonic x87_binary ->
      (* result in st(0) *)
      let to_st0, _ = List.assoc mnemonic x87_binary in
      Fixed ("\xD8" ^ String.make 1 (Char.chr (to_st0 + i)), [])
  | _, [ Reg { rclass = X87; rnum = 0; _ }; Reg { rclass = X87; rnum = i; _ } ] when List.mem_assoc mnemonic x87_binary ->
      (* result in st(i) *)
      let _, to_sti = List.assoc mnemonic x87_binary in
      Fixed ("\xDC" ^ String.make 1 (Char.chr (to_sti + i)), [])
  | _, [] when List.mem_assoc mnemonic x87_plain -> Fixed (List.assoc mnemonic x87_plain, [])
  | _, [ (Mem _ as m) ] when List.mem_assoc mnemonic x87_memory ->
      let opcode, ext = List.assoc mnemonic x87_memory in
      let rm, seg, _ = rm_of S64 m in
      build { default with legacy = seg; reg = ext; rm = Some rm; opcode = [ opcode ] }
  | _, _ when List.mem_assoc mnemonic x87_register ->
      let first, base = List.assoc mnemonic x87_register in
      let i = if operands = [] then (if mnemonic = "fstp" || mnemonic = "fst" || mnemonic = "fld" then 0 else 1) else stack_index operands in
      Fixed (String.make 1 (Char.chr first) ^ String.make 1 (Char.chr (base + i)), [])
  | _ -> bad "bad operands for %s" mnemonic

(* ---- Mnemonic dispatch --------------------------------------------------- *)

let strip s n = String.sub s 0 (String.length s - n)
let last s = s.[String.length s - 1]

(* split "addq" into ("add", Some S64) when the base is a sized mnemonic *)
let sized_mnemonics = [
  "mov"; "add"; "or"; "adc"; "sbb"; "and"; "sub"; "xor"; "cmp"; "test"; "lea"; "push"; "pop";
  "inc"; "dec"; "neg"; "not"; "mul"; "imul"; "div"; "idiv"; "shl"; "sal"; "shr"; "sar"; "rol"; "ror";
  "xchg"; "cmpxchg"; "xadd"; "bswap"; "movabs"; "call"; "jmp"; "bsf"; "bsr"; "popcnt"; "lzcnt"; "tzcnt" ]

let rec encode mnemonic operands =
  let cc_of s = List.assoc_opt s condition_codes in
  let n = String.length mnemonic in
  match mnemonic, operands with
  (* --- no operands --- *)
  | "ret", [] | "retq", [] -> Fixed ("\xC3", [])
  | "leave", [] | "leaveq", [] -> Fixed ("\xC9", [])
  | "nop", [] -> Fixed ("\x90", [])
  | "hlt", [] -> Fixed ("\xF4", [])
  | "int3", [] -> Fixed ("\xCC", [])
  | "ud2", [] -> Fixed ("\x0F\x0B", [])
  | "pause", [] -> Fixed ("\xF3\x90", [])
  | "mfence", [] -> Fixed ("\x0F\xAE\xF0", [])
  | "syscall", [] -> Fixed ("\x0F\x05", [])
  | "cpuid", [] -> Fixed ("\x0F\xA2", [])
  | "rdtsc", [] -> Fixed ("\x0F\x31", [])
  | "cld", [] -> Fixed ("\xFC", [])
  | "std", [] -> Fixed ("\xFD", [])
  | "int", [ Imm e ] -> Fixed ("\xCD" ^ imm8 (Imm e), [])
  | "lfence", [] -> Fixed ("\x0F\xAE\xE8", [])
  | "sfence", [] -> Fixed ("\x0F\xAE\xF8", [])
  | "cqto", [] | "cqo", [] -> Fixed ("\x48\x99", [])
  | "cltd", [] | "cdq", [] -> Fixed ("\x99", [])
  | "cwtd", [] | "cwd", [] -> Fixed ("\x66\x99", [])
  | "cltq", [] | "cdqe", [] -> Fixed ("\x48\x98", [])
  | "cwtl", [] | "cwde", [] -> Fixed ("\x98", [])
  | "cbtw", [] | "cbw", [] -> Fixed ("\x66\x98", [])
  | "movsb", [] -> Fixed ("\xA4", []) | "movsw", [] -> Fixed ("\x66\xA5", [])
  | "movsl", [] -> Fixed ("\xA5", []) | "movsq", [] -> Fixed ("\x48\xA5", [])
  | "stosb", [] -> Fixed ("\xAA", []) | "stosw", [] -> Fixed ("\x66\xAB", [])
  | "stosl", [] -> Fixed ("\xAB", []) | "stosq", [] -> Fixed ("\x48\xAB", [])
  (* --- fixed spellings --- *)
  | "movslq", [ src; dst ] -> movx ~sign:true S32 S64 src dst
  | "movsbw", [ s; d ] -> movx ~sign:true S8 S16 s d | "movzbw", [ s; d ] -> movx ~sign:false S8 S16 s d
  | "movsbl", [ s; d ] -> movx ~sign:true S8 S32 s d | "movzbl", [ s; d ] -> movx ~sign:false S8 S32 s d
  | "movsbq", [ s; d ] -> movx ~sign:true S8 S64 s d | "movzbq", [ s; d ] -> movx ~sign:false S8 S64 s d
  | "movswl", [ s; d ] -> movx ~sign:true S16 S32 s d | "movzwl", [ s; d ] -> movx ~sign:false S16 S32 s d
  | "movswq", [ s; d ] -> movx ~sign:true S16 S64 s d | "movzwq", [ s; d ] -> movx ~sign:false S16 S64 s d
  | "call", [ op ] | "callq", [ op ] -> call op
  | "jmp", [ op ] | "jmpq", [ op ] -> jmp op
  | "movabsq", [ s; d ] | "movabs", [ s; d ] -> movabs s d
  (* --- SSE --- *)
  | "movsd", [ s; d ] -> sse_move [ 0xF2 ] 0x10 0x11 s d
  | "movss", [ s; d ] -> sse_move [ 0xF3 ] 0x10 0x11 s d
  | "movaps", [ s; d ] -> sse_move [] 0x28 0x29 s d
  | "movapd", [ s; d ] -> sse_move [ 0x66 ] 0x28 0x29 s d
  | "movups", [ s; d ] -> sse_move [] 0x10 0x11 s d
  | "movupd", [ s; d ] -> sse_move [ 0x66 ] 0x10 0x11 s d
  | "movlpd", [ s; d ] -> sse_move [ 0x66 ] 0x12 0x13 s d
  | "movdqa", [ s; d ] -> sse_move [ 0x66 ] 0x6F 0x7F s d
  | "movdqu", [ s; d ] -> sse_move [ 0xF3 ] 0x6F 0x7F s d
  | ("movq" | "movd"), [ s; d ] when is_xmm s || is_xmm d ->
      (* between an xmm and a general register the width follows the
         register: gas accepts "movd %xmm1, %rax" for the 64-bit move *)
      let w = (mnemonic = "movq") in
      (match s, d with
       | _, _ when is_xmm s && is_xmm d -> sse [ 0xF3 ] [ 0x7E ] ~w:false ~reg:(xmm d).rnum s
       | _, _ when is_xmm d && is_gpr s -> sse [ 0x66 ] [ 0x6E ] ~w:((gpr s).rwidth = 64) ~reg:(xmm d).rnum s
       | _, _ when is_xmm s && is_gpr d -> sse [ 0x66 ] [ 0x7E ] ~w:((gpr d).rwidth = 64) ~reg:(xmm s).rnum d
       | Mem _, _ when w -> sse [ 0xF3 ] [ 0x7E ] ~w:false ~reg:(xmm d).rnum s
       | _, Mem _ when w -> sse [ 0x66 ] [ 0xD6 ] ~w:false ~reg:(xmm s).rnum d
       | Mem _, _ -> sse [ 0x66 ] [ 0x6E ] ~w:false ~reg:(xmm d).rnum s
       | _, Mem _ -> sse [ 0x66 ] [ 0x7E ] ~w:false ~reg:(xmm s).rnum d
       | _ -> bad "bad operands for %s" mnemonic)
  | "addsd", [ s; d ] -> sse_load [ 0xF2 ] 0x58 s d | "addss", [ s; d ] -> sse_load [ 0xF3 ] 0x58 s d
  | "mulsd", [ s; d ] -> sse_load [ 0xF2 ] 0x59 s d | "mulss", [ s; d ] -> sse_load [ 0xF3 ] 0x59 s d
  | "subsd", [ s; d ] -> sse_load [ 0xF2 ] 0x5C s d | "subss", [ s; d ] -> sse_load [ 0xF3 ] 0x5C s d
  | "divsd", [ s; d ] -> sse_load [ 0xF2 ] 0x5E s d | "divss", [ s; d ] -> sse_load [ 0xF3 ] 0x5E s d
  | "minsd", [ s; d ] -> sse_load [ 0xF2 ] 0x5D s d | "minss", [ s; d ] -> sse_load [ 0xF3 ] 0x5D s d
  | "maxsd", [ s; d ] -> sse_load [ 0xF2 ] 0x5F s d | "maxss", [ s; d ] -> sse_load [ 0xF3 ] 0x5F s d
  | "sqrtsd", [ s; d ] -> sse_load [ 0xF2 ] 0x51 s d | "sqrtss", [ s; d ] -> sse_load [ 0xF3 ] 0x51 s d
  | "ucomisd", [ s; d ] -> sse_load [ 0x66 ] 0x2E s d | "ucomiss", [ s; d ] -> sse_load [] 0x2E s d
  | "comisd", [ s; d ] -> sse_load [ 0x66 ] 0x2F s d | "comiss", [ s; d ] -> sse_load [] 0x2F s d
  | "andpd", [ s; d ] -> sse_load [ 0x66 ] 0x54 s d | "andps", [ s; d ] -> sse_load [] 0x54 s d
  | "andnpd", [ s; d ] -> sse_load [ 0x66 ] 0x55 s d | "andnps", [ s; d ] -> sse_load [] 0x55 s d
  | "orpd", [ s; d ] -> sse_load [ 0x66 ] 0x56 s d | "orps", [ s; d ] -> sse_load [] 0x56 s d
  | "xorpd", [ s; d ] -> sse_load [ 0x66 ] 0x57 s d | "xorps", [ s; d ] -> sse_load [] 0x57 s d
  | "pxor", [ s; d ] -> sse_load [ 0x66 ] 0xEF s d
  | "cvtsd2ss", [ s; d ] -> sse_load [ 0xF2 ] 0x5A s d | "cvtss2sd", [ s; d ] -> sse_load [ 0xF3 ] 0x5A s d
  | ("cvtsi2sd" | "cvtsi2sdl"), [ s; d ] -> sse [ 0xF2 ] [ 0x2A ] ~w:false ~reg:(xmm d).rnum s
  | "cvtsi2sdq", [ s; d ] -> sse [ 0xF2 ] [ 0x2A ] ~w:true ~reg:(xmm d).rnum s
  | ("cvtsi2ss" | "cvtsi2ssl"), [ s; d ] -> sse [ 0xF3 ] [ 0x2A ] ~w:false ~reg:(xmm d).rnum s
  | "cvtsi2ssq", [ s; d ] -> sse [ 0xF3 ] [ 0x2A ] ~w:true ~reg:(xmm d).rnum s
  | ("cvttsd2si" | "cvttsd2sil" | "cvttsd2siq"), [ s; d ] ->
      sse [ 0xF2 ] [ 0x2C ] ~w:((gpr d).rwidth = 64) ~reg:(gpr d).rnum s
  | ("cvttss2si" | "cvttss2sil" | "cvttss2siq"), [ s; d ] ->
      sse [ 0xF3 ] [ 0x2C ] ~w:((gpr d).rwidth = 64) ~reg:(gpr d).rnum s
  | ("cvtsd2si" | "cvtsd2sil" | "cvtsd2siq"), [ s; d ] ->
      sse [ 0xF2 ] [ 0x2D ] ~w:((gpr d).rwidth = 64) ~reg:(gpr d).rnum s
  | ("cvtss2si" | "cvtss2sil" | "cvtss2siq"), [ s; d ] ->
      sse [ 0xF3 ] [ 0x2D ] ~w:((gpr d).rwidth = 64) ~reg:(gpr d).rnum s
  | "pcmpeqd", [ s; d ] -> sse_load [ 0x66 ] 0x76 s d
  | ("psrlq" | "psrld" | "psllq" | "pslld" | "psrlw" | "psllw" | "psraw" | "psrad"), [ (Imm _ as i); d ] ->
      (* shift of a packed register by an immediate: 66 0F 7x /n ib *)
      let opcode, ext = match mnemonic with
        | "psrlq" -> 0x73, 2 | "psllq" -> 0x73, 6 | "psrld" -> 0x72, 2 | "pslld" -> 0x72, 6 | "psrad" -> 0x72, 4
        | "psrlw" -> 0x71, 2 | "psllw" -> 0x71, 6 | _ -> 0x71, 4 in
      let rm, _, _ = rm_of S64 d in
      (match build { default with legacy = [ 0x66 ]; reg = ext; rm = Some rm; opcode = [ 0x0F; opcode ] } with
       | Fixed (b, f) -> Fixed (b ^ imm8 i, f)
       | Branch _ -> assert false)
  | "pcmpeqb", [ s; d ] -> sse_load [ 0x66 ] 0x74 s d
  | "pmovmskb", [ s; d ] -> sse [ 0x66 ] [ 0xD7 ] ~w:false ~reg:(gpr d).rnum s
  | "movmskpd", [ s; d ] -> sse [ 0x66 ] [ 0x50 ] ~w:false ~reg:(gpr d).rnum s
  | "movmskps", [ s; d ] -> sse [] [ 0x50 ] ~w:false ~reg:(gpr d).rnum s
  | "roundsd", [ i; s; d ] -> sse ~imm:(imm8 i) [ 0x66 ] [ 0x3A; 0x0B ] ~w:false ~reg:(xmm d).rnum s
  | _, [ s; d ] when n > 5 && String.sub mnemonic 0 3 = "cmp" && String.sub mnemonic (n - 2) 2 = "sd"
                     && List.mem_assoc (String.sub mnemonic 3 (n - 5)) sse_predicates ->
      let pred = List.assoc (String.sub mnemonic 3 (n - 5)) sse_predicates in
      sse ~imm:(String.make 1 (Char.chr pred)) [ 0xF2 ] [ 0xC2 ] ~w:false ~reg:(xmm d).rnum s
  | _, _ when is_x87 mnemonic -> x87 mnemonic operands
  (* --- condition codes --- *)
  | _, [ op ] when n > 1 && mnemonic.[0] = 'j' && cc_of (String.sub mnemonic 1 (n - 1)) <> None ->
      jcc (Option.get (cc_of (String.sub mnemonic 1 (n - 1)))) op
  | _, [ op ] when n > 3 && String.sub mnemonic 0 3 = "set" && cc_of (String.sub mnemonic 3 (n - 3)) <> None ->
      setcc (Option.get (cc_of (String.sub mnemonic 3 (n - 3)))) op
  | _, [ s; d ] when n > 4 && String.sub mnemonic 0 4 = "cmov" ->
      let rest = String.sub mnemonic 4 (n - 4) in
      (match cc_of rest with
       | Some cc -> cmovcc cc (operand_size None operands) s d
       | None ->
           (match cc_of (strip rest 1), size_of_suffix (last rest) with
            | Some cc, Some size -> cmovcc cc size s d
            | _ -> bad "unknown instruction %s" mnemonic))
  (* --- sized mnemonics --- *)
  | _ ->
      let base, suffix =
        if List.mem mnemonic sized_mnemonics then mnemonic, None
        else if n > 1 && List.mem (strip mnemonic 1) sized_mnemonics && size_of_suffix (last mnemonic) <> None
        then strip mnemonic 1, size_of_suffix (last mnemonic)
        else bad "unknown instruction %s" mnemonic in
      sized base suffix operands

and sse_predicates = [ "eq", 0; "lt", 1; "le", 2; "unord", 3; "neq", 4; "nlt", 5; "nle", 6; "ord", 7 ]

and sized base suffix operands =
  let size () = operand_size suffix operands in
  match base, operands with
  | "mov", [ s; d ] -> mov (size ()) s d
  | "movabs", [ s; d ] -> movabs s d
  | "add", [ s; d ] -> alu 0 (size ()) s d | "or", [ s; d ] -> alu 1 (size ()) s d
  | "adc", [ s; d ] -> alu 2 (size ()) s d | "sbb", [ s; d ] -> alu 3 (size ()) s d
  | "and", [ s; d ] -> alu 4 (size ()) s d | "sub", [ s; d ] -> alu 5 (size ()) s d
  | "xor", [ s; d ] -> alu 6 (size ()) s d | "cmp", [ s; d ] -> alu 7 (size ()) s d
  | "test", [ s; d ] -> test (size ()) s d
  | "lea", [ s; d ] -> lea s d
  | "not", [ op ] -> group3 2 (size ()) op | "neg", [ op ] -> group3 3 (size ()) op
  | "mul", [ op ] -> group3 4 (size ()) op | "div", [ op ] -> group3 6 (size ()) op
  | "idiv", [ op ] -> group3 7 (size ()) op
  | "imul", _ -> imul (size ()) operands
  | "inc", [ op ] -> group5 0 (size ()) op | "dec", [ op ] -> group5 1 (size ()) op
  | ("shl" | "sal"), [ d ] -> shift 4 (size ()) None d | ("shl" | "sal"), [ c; d ] -> shift 4 (operand_size suffix [ d ]) (Some c) d
  | "shr", [ d ] -> shift 5 (size ()) None d | "shr", [ c; d ] -> shift 5 (operand_size suffix [ d ]) (Some c) d
  | "sar", [ d ] -> shift 7 (size ()) None d | "sar", [ c; d ] -> shift 7 (operand_size suffix [ d ]) (Some c) d
  | "rol", [ d ] -> shift 0 (size ()) None d | "rol", [ c; d ] -> shift 0 (operand_size suffix [ d ]) (Some c) d
  | "ror", [ d ] -> shift 1 (size ()) None d | "ror", [ c; d ] -> shift 1 (operand_size suffix [ d ]) (Some c) d
  | "push", [ op ] -> push_pop ~reg_op:0x50 ~mem_op:0xFF ~mem_ext:6 ~imm_ops:(Some (0x6A, 0x68)) op
  | "pop", [ op ] -> push_pop ~reg_op:0x58 ~mem_op:0x8F ~mem_ext:0 ~imm_ops:None op
  | "xchg", [ a; b ] -> xchg (size ()) a b
  | "cmpxchg", [ s; d ] -> reg_to_rm 0xB0 (size ()) s d
  | "xadd", [ s; d ] -> reg_to_rm 0xC0 (size ()) s d
  | "bswap", [ Reg r ] -> build { default with w = (size () = S64); reg = r.rnum; opcode = [ 0x0F; 0xC8 + (r.rnum land 7) ] }
  | ("bsf" | "bsr" | "popcnt" | "lzcnt" | "tzcnt"), [ s; d ] ->
      let r = gpr d in
      let rm, seg, _ = rm_of (size ()) s in
      let prefix = if base = "bsf" || base = "bsr" then [] else [ 0xF3 ] in
      let op = match base with "bsf" | "tzcnt" -> 0xBC | "bsr" | "lzcnt" -> 0xBD | _ -> 0xB8 in
      build { default with legacy = seg @ prefix @ size_prefix (size ()); w = (size () = S64); reg = r.rnum; rm = Some rm; opcode = [ 0x0F; op ] }
  | "call", [ op ] -> call op
  | "jmp", [ op ] -> jmp op
  | _ -> bad "bad operands for %s" base

(* lock, rep and friends are single bytes in front of the instruction *)
let prefix_byte = function
  | "lock" -> 0xF0 | "rep" | "repe" | "repz" -> 0xF3 | "repne" | "repnz" -> 0xF2
  | p -> bad "unknown prefix %s" p

let instruction (i : instruction) =
  let prefix = String.concat "" (List.map (fun p -> String.make 1 (Char.chr (prefix_byte p))) i.prefixes) in
  match encode i.mnemonic i.operands with
  | Fixed (s, fixups) when prefix = "" -> Fixed (s, fixups)
  | Fixed (s, fixups) ->
      let k = String.length prefix in
      Fixed (prefix ^ s, List.map (fun f -> { f with at = f.at + k; pcbase = f.pcbase + k }) fixups)
  | Branch _ as b -> if prefix = "" then b else bad "prefix on a branch"
