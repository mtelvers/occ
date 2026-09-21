(* Parsing GNU assembler source into Gas.statement values.

   A recursive-descent parser over Gas_lex tokens with one token of
   lookahead.  Each statement is parsed by the rule for its kind: a label
   ("name:"), a symbol assignment ("name = expr"), a directive (".name
   args") or an instruction ("mnemonic operands").  Directive arguments
   are parsed per directive, so a misuse is reported here rather than
   turning into wrong bytes later. *)

open Gas
open Gas_lex

type t = {
  lx : Gas_lex.t;
  mutable tok : token;
  mutable pending_prefixes : string list;   (* a "lock" or "rep" on a line of its own *)
  numeric : (int, int) Hashtbl.t;           (* local label number -> definitions so far *)
}

let error st fmt = Gas_lex.error st.lx fmt
let advance st = st.tok <- Gas_lex.next st.lx
let expect st tok what = if st.tok = tok then advance st else error st "expected %s" what

let accept st tok = if st.tok = tok then (advance st; true) else false

(* Numeric local labels: the k-th definition of "1:" becomes ".Lnum1.k";
   "1b" is the latest definition seen, "1f" the next one to come. *)
let numeric_name n k = Printf.sprintf ".Lnum%d.%d" n k
let define_numeric st n =
  let k = 1 + Option.value (Hashtbl.find_opt st.numeric n) ~default:0 in
  Hashtbl.replace st.numeric n k; numeric_name n k
let refer_numeric st n forward =
  let k = Option.value (Hashtbl.find_opt st.numeric n) ~default:0 in
  numeric_name n (if forward then k + 1 else k)

(* ---- Expressions ---------------------------------------------------------- *)

(* Precedence follows GNU as: multiplicative and shift operators bind
   tightest, then the bitwise operators, then + and -. *)
let rec expr st = additive st

and additive st =
  let rec loop lhs =
    match st.tok with
    | PLUS -> advance st; loop (Bin (Add, lhs, bitwise st))
    | MINUS -> advance st; loop (Bin (Sub, lhs, bitwise st))
    | _ -> lhs in
  loop (bitwise st)

and bitwise st =
  let rec loop lhs =
    match st.tok with
    | AMP -> advance st; loop (Bin (And, lhs, multiplicative st))
    | BAR -> advance st; loop (Bin (Or, lhs, multiplicative st))
    | CARET -> advance st; loop (Bin (Xor, lhs, multiplicative st))
    | _ -> lhs in
  loop (multiplicative st)

and multiplicative st =
  let rec loop lhs =
    match st.tok with
    | STAR -> advance st; loop (Bin (Mul, lhs, unary st))
    | SLASH -> advance st; loop (Bin (Div, lhs, unary st))
    | PERCENT -> advance st; loop (Bin (Mod, lhs, unary st))
    | SHL -> advance st; loop (Bin (Shl, lhs, unary st))
    | SHR -> advance st; loop (Bin (Shr, lhs, unary st))
    | _ -> lhs in
  loop (unary st)

and unary st =
  match st.tok with
  | MINUS -> advance st; Neg (unary st)
  | TILDE -> advance st; Not (unary st)
  | PLUS -> advance st; unary st
  | _ -> primary st

and primary st =
  match st.tok with
  | INT v -> advance st; Num v
  | IDENT "." -> advance st; Dot
  | IDENT name ->
      advance st;
      if st.tok = AT then begin
        advance st;
        match st.tok with
        | IDENT m -> advance st; Sym (name, Some m)
        | _ -> error st "expected a relocation modifier after @"
      end else Sym (name, None)
  | LOCALREF (n, fwd) -> advance st; Sym (refer_numeric st n fwd, None)
  | LPAREN -> advance st; let e = expr st in expect st RPAREN ")"; e
  | _ -> error st "expected an expression"

let constant st =
  (* an expression that must be a number now, for directive arguments *)
  let rec eval = function
    | Num v -> v
    | Neg e -> Int64.neg (eval e)
    | Not e -> Int64.lognot (eval e)
    | Bin (op, a, b) ->
        let a = eval a and b = eval b in
        (match op with
         | Add -> Int64.add a b | Sub -> Int64.sub a b | Mul -> Int64.mul a b
         | Div -> Int64.div a b | Mod -> Int64.rem a b | And -> Int64.logand a b
         | Or -> Int64.logor a b | Xor -> Int64.logxor a b
         | Shl -> Int64.shift_left a (Int64.to_int b) | Shr -> Int64.shift_right_logical a (Int64.to_int b))
    | Sym _ | Dot -> error st "expected a constant" in
  Int64.to_int (eval (expr st))

(* ---- Operands ------------------------------------------------------------- *)

(* Registers are named differently on the two machines, and written
   differently: %rax against a0.  [Gas] holds both tables. *)
let register_by_name name =
  match !Target.machine with
  | Target.Amd64 -> register_of_name name
  | Target.Riscv64 -> riscv_register_of_name name

let register st name =
  match register_by_name name with
  | Some r -> r
  | None -> error st "unknown register %s" name

(* "(base,index,scale)" with the opening parenthesis already consumed *)
let addressing st seg disp =
  let base = ref None and index = ref None and scale = ref 1 in
  (match st.tok with REG r -> advance st; base := Some (register st r) | _ -> ());
  if accept st COMMA then begin
    (match st.tok with
     | REG r -> advance st; index := Some (register st r)
     | _ -> error st "expected an index register");
    if accept st COMMA then
      (match st.tok with
       | INT (1L | 2L | 4L | 8L as s) -> advance st; scale := Int64.to_int s
       | _ -> error st "scale must be 1, 2, 4 or 8")
  end;
  expect st RPAREN ")";
  Mem { seg; disp; base = !base; index = !index; scale = !scale }

(* does the "(" ahead start an addressing mode rather than an expression? *)
let addressing_ahead st =
  st.tok = LPAREN && begin
    let saved = Gas_lex.save st.lx and tok = st.tok in
    advance st;
    let yes = (match st.tok with REG _ | COMMA -> true | _ -> false) in
    Gas_lex.restore st.lx saved; st.tok <- tok;
    yes
  end

let rec operand st =
  match st.tok with
  | DOLLAR -> advance st; Imm (expr st)
  | STAR -> advance st; Indirect (operand st)
  | REG r ->
      advance st;
      let r = register st r in
      if r.rclass = Segment then begin
        expect st COLON ":";
        memory st (Some r)
      end else if r.rclass = X87 && st.tok = LPAREN then begin
        (* %st(i) *)
        advance st;
        let i = match st.tok with INT n -> advance st; Int64.to_int n | _ -> error st "expected a stack register number" in
        expect st RPAREN ")";
        Reg { r with rnum = i; rname = Printf.sprintf "st(%d)" i }
      end else Reg r
  | _ -> memory st None

(* [disp](base,index,scale), or a bare expression: an absolute address or
   a branch target *)
and memory st seg =
  let disp = if addressing_ahead st then None else Some (expr st) in
  if accept st LPAREN then addressing st seg disp
  else Mem { seg; disp; base = None; index = None; scale = 1 }

(* RISC-V operands, which are a smaller language than x86's: a register
   by its bare name, an expression, or the machine's one addressing mode,
   offset(base).

   A relocation function -- %hi(sym), %pcrel_lo(sym), %tprel_add(sym) --
   becomes the symbol carrying that name as its modifier, which is how
   the assembler already says "this symbol, relocated that way".  The
   lexer reads %hi as a register token, since that is what a % starts on
   the other machine; here the parenthesis that follows tells it apart.

   A bare word that is not a register is left as an expression: a
   rounding mode (rtz) and a fence ordering (rw) reach the encoder that
   way, and it knows which of its mnemonics take one. *)
let rec rv_operand st =
  match st.tok with
  | IDENT name when register_by_name name <> None -> advance st; Reg (register st name)
  | _ ->
      let disp = if st.tok = LPAREN then None else Some (rv_expr st) in
      if accept st LPAREN then begin
        let base =
          match st.tok with
          | IDENT n -> advance st; register st n
          | _ -> error st "expected a base register" in
        expect st RPAREN ")";
        Mem { seg = None; disp; base = Some base; index = None; scale = 1 }
      end else
        match disp with Some e -> Imm e | None -> error st "expected an operand"

(* an expression, with the relocation functions recognised in it *)
and rv_expr st =
  match st.tok with
  | REG m ->
      (* %hi(...) and its relatives: the modifier belongs to the symbol *)
      advance st;
      expect st LPAREN "( after a relocation function";
      let e = expr st in
      expect st RPAREN ")";
      let rec attach = function
        | Sym (name, None) -> Sym (name, Some m)
        | Bin (op, a, b) -> Bin (op, attach a, b)
        | Num _ as e when m = "lo" || m = "hi" -> e   (* %hi of a constant *)
        | _ -> error st "%%%s must name a symbol" m in
      attach e
  | _ -> expr st

let operands st =
  match st.tok with
  | NEWLINE | EOF -> []
  | _ ->
      let one () = match !Target.machine with
        | Target.Amd64 -> operand st
        | Target.Riscv64 -> rv_operand st in
      let rec more acc =
        if accept st COMMA then more (one () :: acc) else List.rev acc in
      more [ one () ]

(* ---- Directives ----------------------------------------------------------- *)

let string_arg st = match st.tok with STRING s -> advance st; s | _ -> error st "expected a string"
let symbol_arg st = match st.tok with IDENT s -> advance st; s | _ -> error st "expected a symbol"

let symbol_list st =
  let rec more acc = if accept st COMMA then more (symbol_arg st :: acc) else List.rev acc in
  more [ symbol_arg st ]

let expr_list st =
  let rec more acc = if accept st COMMA then more (expr st :: acc) else List.rev acc in
  more [ expr st ]

let string_list st =
  let rec more acc = if accept st COMMA then more (string_arg st :: acc) else List.rev acc in
  more [ string_arg st ]

(* section names may contain "-", as in .note.GNU-stack *)
let section_name st =
  let rec go name =
    if st.tok = MINUS then begin advance st; go (name ^ "-" ^ symbol_arg st) end else name in
  go (symbol_arg st)

(* a register in a .cfi directive: a DWARF number, %name or bare name *)
let cfi_register st =
  match st.tok with
  | REG r | IDENT r -> advance st;
      let r = register st r in
      (match r.rclass with
       | Gpr -> dwarf_number_of_gpr.(r.rnum)
       | Xmm -> 17 + r.rnum
       | Rip -> 16
       | X87 -> 33 + r.rnum
       (* RISC-V numbers its registers as the hardware does, the
          floating-point ones following the integer ones *)
       | Ireg -> r.rnum
       | Freg -> 32 + r.rnum
       | Segment -> error st "no DWARF number for %%%s" r.rname)
  | _ -> constant st

let symbol_type st =
  (* @function, %function, "function", STT_FUNC *)
  (match st.tok with AT | PERCENT -> advance st | _ -> ());
  match st.tok with
  | IDENT "STT_FUNC" -> advance st; "function"
  | IDENT "STT_OBJECT" -> advance st; "object"
  | IDENT "STT_TLS" -> advance st; "tls_object"
  | IDENT "STT_NOTYPE" -> advance st; "notype"
  | IDENT t | REG t | STRING t -> advance st; t     (* %function lexes as a register *)
  | _ -> error st "expected a symbol type"

let directive st name =
  let one d = [ Directive d ] in
  let each f = List.map (fun s -> Directive (f s)) (symbol_list st) in
  let section ?flags ?stype sname = one (Section { sname; sflags = flags; stype; sextra = [] }) in
  match name with
  | ".text" -> section ".text" | ".data" -> section ".data" | ".bss" -> section ".bss"
  | ".section" ->
      let sname = section_name st in
      let sflags = if accept st COMMA then Some (string_arg st) else None in
      let stype =
        if sflags <> None && accept st COMMA then begin
          (match st.tok with
           | AT -> advance st; Some (symbol_arg st)
           | REG t -> advance st; Some t                (* %progbits lexes as a register *)
           | _ -> Some (symbol_arg st))
        end else None in
      let sextra = if stype <> None && accept st COMMA then expr_list st else [] in
      one (Section { sname; sflags; stype; sextra })
  | ".previous" -> one Previous
  | ".globl" | ".global" -> each (fun s -> Global s)
  | ".local" -> each (fun s -> Local s)
  | ".weak" -> each (fun s -> Weak s)
  | ".hidden" | ".protected" | ".internal" ->
      let v = String.sub name 1 (String.length name - 1) in
      each (fun s -> Visibility (s, v))
  | ".type" ->
      let s = symbol_arg st in expect st COMMA ","; one (Type (s, symbol_type st))
  | ".size" ->
      let s = symbol_arg st in expect st COMMA ","; one (Size (s, expr st))
  | ".set" | ".equ" ->
      let s = symbol_arg st in expect st COMMA ","; one (Set (s, expr st))
  | ".comm" ->
      let s = symbol_arg st in expect st COMMA ",";
      let size = expr st in
      let align = if accept st COMMA then Some (expr st) else None in
      one (Comm (s, size, align))
  | ".lcomm" ->
      let s = symbol_arg st in expect st COMMA ",";
      let size = expr st in
      let align = if accept st COMMA then Some (expr st) else None in
      [ Directive (Local s); Directive (Comm (s, size, align)) ]
  | ".byte" -> one (Data (1, expr_list st))
  | ".value" | ".short" | ".2byte" | ".half" -> one (Data (2, expr_list st))
  (* ".word" is two bytes on x86 and four on RISC-V, which is a trap worth
     naming: the same directive means different widths on the two
     machines, and gas reads it by the machine it was built for. *)
  | ".word" -> one (Data ((match !Target.machine with Target.Amd64 -> 2 | Target.Riscv64 -> 4), expr_list st))
  | ".long" | ".int" | ".4byte" -> one (Data (4, expr_list st))
  | ".quad" | ".8byte" | ".dword" -> one (Data (8, expr_list st))
  | ".ascii" -> one (Ascii (string_list st))
  | ".asciz" | ".string" -> one (Asciz (string_list st))
  | ".zero" | ".space" | ".skip" ->
      let n = expr st in
      let fill = if accept st COMMA then constant st else 0 in
      one (Zero (n, fill))
  | ".uleb128" -> one (Uleb128 (expr_list st))
  | ".sleb128" -> one (Sleb128 (expr_list st))
  | ".align" | ".balign" | ".p2align" ->
      let n = constant st in
      (* ".align" counts bytes on x86 and powers of two on RISC-V, which
         is why the code generators spell the same alignment differently.
         ".balign" is always bytes and ".p2align" always a power. *)
      let n =
        if name = ".p2align" || (name = ".align" && !Target.machine = Target.Riscv64)
        then 1 lsl n else n in
      let fill = if accept st COMMA then (if st.tok = COMMA then None else Some (constant st)) else None in
      if accept st COMMA then ignore (constant st);   (* max skip: not honoured *)
      one (Align (n, fill))
  | ".file" ->
      (match st.tok with
       | STRING f -> advance st; one (File (None, f))
       | INT n -> advance st;
           let f = string_arg st in
           (* DWARF 5 form: .file n "dir" "name" *)
           let f = match st.tok with STRING g -> advance st; Filename.concat f g | _ -> f in
           one (File (Some (Int64.to_int n), f))
       | _ -> error st "expected a file name")
  | ".loc" ->
      let file = constant st in
      let line = constant st in
      let col = match st.tok with INT _ -> constant st | _ -> 0 in
      (* is_stmt, prologue_end, view ... are accepted and ignored *)
      while st.tok <> NEWLINE && st.tok <> EOF do advance st done;
      one (Loc (file, line, col))
  | ".cfi_startproc" ->
      let simple = (match st.tok with IDENT "simple" -> advance st; true | _ -> false) in
      one (Cfi (Cfi_startproc simple))
  | ".cfi_endproc" -> one (Cfi Cfi_endproc)
  | ".cfi_def_cfa" -> let r = cfi_register st in expect st COMMA ","; one (Cfi (Cfi_def_cfa (r, constant st)))
  | ".cfi_def_cfa_register" -> one (Cfi (Cfi_def_cfa_register (cfi_register st)))
  | ".cfi_def_cfa_offset" -> one (Cfi (Cfi_def_cfa_offset (constant st)))
  | ".cfi_adjust_cfa_offset" -> one (Cfi (Cfi_adjust_cfa_offset (constant st)))
  | ".cfi_offset" -> let r = cfi_register st in expect st COMMA ","; one (Cfi (Cfi_offset (r, constant st)))
  | ".cfi_rel_offset" -> let r = cfi_register st in expect st COMMA ","; one (Cfi (Cfi_rel_offset (r, constant st)))
  | ".cfi_restore" -> one (Cfi (Cfi_restore (cfi_register st)))
  | ".cfi_same_value" -> one (Cfi (Cfi_same_value (cfi_register st)))
  | ".cfi_undefined" -> one (Cfi (Cfi_undefined (cfi_register st)))
  | ".cfi_register" -> let a = cfi_register st in expect st COMMA ","; one (Cfi (Cfi_register (a, cfi_register st)))
  | ".cfi_remember_state" -> one (Cfi Cfi_remember_state)
  | ".cfi_restore_state" -> one (Cfi Cfi_restore_state)
  | ".cfi_escape" -> one (Cfi (Cfi_escape (expr_list st)))
  | ".cfi_signal_frame" -> one (Cfi Cfi_signal_frame)
  | ".cfi_sections" -> while st.tok <> NEWLINE && st.tok <> EOF do advance st done; one (Ignored name)
  | ".ident" -> one (Ident (string_arg st))
  | ".noexecstack" | ".addrsig" | ".addrsig_sym" | ".build_attributes" ->
      while st.tok <> NEWLINE && st.tok <> EOF do advance st done; one (Ignored name)
  (* RISC-V.  ".option" chooses between addressing models and turns
     relaxation on and off, and ".attribute" records which extensions the
     file was built for; neither changes a byte of what we assemble, since
     this assembler emits the fixed sequences and never relaxes. *)
  | ".option" ->
      let what = match st.tok with IDENT w -> advance st; w | _ -> "" in
      while st.tok <> NEWLINE && st.tok <> EOF do advance st done;
      one (Riscv_option what)
  | ".attribute" | ".insn" ->
      while st.tok <> NEWLINE && st.tok <> EOF do advance st done; one (Ignored name)
  | _ -> error st "unknown directive %s" name

(* ---- Statements ----------------------------------------------------------- *)

let prefixes = [ "lock"; "rep"; "repz"; "repe"; "repnz"; "repne" ]

let statement st =
  match st.tok with
  | INT n ->
      advance st; expect st COLON ":";
      [ Label (define_numeric st (Int64.to_int n)) ]
  | IDENT name ->
      advance st;
      (match st.tok with
       | COLON -> advance st; [ Label name ]
       | EQUAL -> advance st; [ Directive (Set (name, expr st)) ]
       | _ when name.[0] = '.' -> directive st name
       | _ when List.mem name prefixes ->
           (match st.tok with
            | IDENT mnemonic ->
                advance st;
                let ops = operands st in
                [ Instruction { prefixes = st.pending_prefixes @ [ name ]; mnemonic; operands = ops } ]
            | _ -> st.pending_prefixes <- st.pending_prefixes @ [ name ]; [])
       | _ ->
           let ops = operands st in
           let p = st.pending_prefixes in
           st.pending_prefixes <- [];
           [ Instruction { prefixes = p; mnemonic = name; operands = ops } ])
  | _ -> error st "expected a statement"

let parse file text =
  let lx = Gas_lex.make file text in
  let st = { lx; tok = EOF; pending_prefixes = []; numeric = Hashtbl.create 8 } in
  advance st;
  let rec loop acc =
    match st.tok with
    | EOF -> List.rev acc
    | NEWLINE -> advance st; loop acc
    | _ ->
        let lineno = st.lx.line in
        let stmts = statement st in
        (* a label may share its line with the statement that follows *)
        (match stmts, st.tok with
         | [ Label _ ], _ | _, (NEWLINE | EOF) -> ()
         | _ -> error st "junk at end of statement");
        loop (List.rev_append (List.map (fun stmt -> { stmt; lineno }) stmts) acc) in
  loop []
