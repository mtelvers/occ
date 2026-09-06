(* The intermediate representation: three-address code over virtual
   registers, one function at a time.

   There is exactly one IR.  Values are machine scalars; aggregates live in
   memory and move by [Memcpy].  Control flow is explicit labels and
   jumps, so [Lower] does all the work of turning C's statements into
   branches and the back end does none.  Calls carry enough type
   information for the back end to apply the calling convention; atomic
   operations carry their memory order (7.17.3) so the back end can choose
   the fence discipline without knowing anything about C. *)

type reg = int

(* Machine scalar types.  Signedness is a property of operations, not of
   values, as on the hardware. *)
type ty = I8 | I16 | I32 | I64 | F32 | F64

type operand =
  | Reg of reg
  | Imm of int64
  | Fimm of float
  | Sym of string (* address of a global or function *)
  | Slot of int (* address of a stack slot *)

type binop =
  | Add | Sub | Mul | Sdiv | Udiv | Srem | Urem
  | And | Or | Xor | Shl | Sshr | Ushr
  | Fadd | Fsub | Fmul | Fdiv

type cond =
  | Eq | Ne | Slt | Sle | Sgt | Sge | Ult | Ule | Ugt | Uge
  | Feq | Fne | Flt | Fle | Fgt | Fge

type memory_order = Relaxed | Consume | Acquire | Release | Acq_rel | Seq_cst

type conv =
  | Sext of ty * ty | Zext of ty * ty | Trunc of ty * ty
  | Fext | Ftrunc
  | Stof of ty * ty | Utof of ty * ty | Ftos of ty * ty | Ftou of ty * ty

(* How the calling convention treats each eightbyte of an aggregate
   (System V ABI 3.2.3), decided by [Abi] from the C type: [Memory] means
   the whole object is passed on the stack. *)
type cls = Integer | Sse | Memory

(* Arguments and results as the calling convention sees them: scalars by
   value, aggregates by address with their size and classification. *)
type agg = { addr : operand; size : int; classes : cls list }

type arg = Scalar of ty * operand | Aggregate of agg

type result = Ret_scalar of ty * reg | Ret_aggregate of agg (* into this address *)

type ret_value = Rv_scalar of ty * operand | Rv_aggregate of agg

type instr =
  | Mov of ty * reg * operand
  | Binop of binop * ty * reg * operand * operand
  | Binop_overflow of binop * ty * bool * reg * reg * operand * operand
      (* op, type, signed, result, overflow flag (I32 0/1), a, b *)
  | Neg of ty * reg * operand
  | Not of ty * reg * operand
  | Cmp of cond * ty * reg * operand * operand (* 0 or 1 into an I32 register *)
  | Conv of conv * reg * operand
  | Load of ty * reg * operand
  | Store of ty * operand * operand (* addr, value *)
  | Memcpy of operand * operand * int (* dst, src, bytes *)
  | Memzero of operand * int
  | Call of result option * operand * arg list * bool (* callee, args, variadic *)
  | Label of string
  | Jump of string
  | Branch of operand * string * string (* if nonzero then else *)
  | Switch of ty * operand * (int64 * string) list * string
  | Ret of ret_value option
  (* 7.17 *)
  | Atomic_load of ty * reg * operand * memory_order
  | Atomic_store of ty * operand * operand * memory_order
  | Atomic_rmw of binop * ty * reg * operand * operand * memory_order (* returns the old value *)
  | Atomic_xchg of ty * reg * operand * operand * memory_order
  | Atomic_cmpxchg of ty * reg * operand * operand * operand * memory_order
      (* result 0/1, addr, expected (address of), desired *)
  | Fence of memory_order
  (* extensions the runtime needs; see doc/extensions.md *)
  | Va_start of operand (* address of the va_list *)
  | Va_arg of ty * reg * operand
  | Trap
  | Return_address of reg
  | Intrinsic of intrinsic * ty * reg * operand (* a library function computed inline *)
  | Line of Loc.t (* the source position of what follows, for debug line tables *)

and intrinsic = Fabs | Fsqrt

type slot = { size : int; align : int }

type param = P_scalar of ty * reg | P_aggregate of int * int * cls list (* slot, size, classes *)

type func = {
  name : string;
  params : param list;
  variadic : bool;
  returns_aggregate : (int * cls list) option; (* size and classes of an aggregate result *)
  slots : slot array;
  body : instr list;
  global : bool;
  discardable : bool; (* an unreferenced inline definition is not emitted *)
  loc : Loc.t; (* where the definition starts *)
  variables : reg list; (* registers holding C variables, favoured by the allocator *)
  params_dbg : (string * Ctype.t) list; (* parameter names and C types, in order, for DWARF *)
  ret_dbg : Ctype.t; (* the C return type *)
}

type data =
  | Bytes of string
  | Zeros of int
  | Addr of string * int64 (* symbol plus offset, 8 bytes *)

type global = {
  gname : string;
  gglobal : bool;
  galign : int;
  gtls : bool;
  gsize : int;
  ginit : data list option; (* None: tentative or extern-less common, goes in .bss *)
  gdefined : bool; (* false: only referenced *)
}

type program = { funcs : func list; globals : global list; source : string (* the translation unit's file *) }
