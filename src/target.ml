(* The one place the front end learns machine facts: which machine is
   being compiled for, and the sizes and alignments of the scalar types
   there.  Aggregates are laid out by [Env] from these.

   Both machines this compiler knows are LP64 and agree on every scalar
   size, long double included -- 16 bytes on each, though the bits
   differ: an 80-bit x87 value padded out on x86-64, and IEEE binary128
   on RISC-V.  So the sizes below need no machine of their own; what
   differs is the code generated for them, and the ABI's rules for
   passing aggregates (see abi.ml). *)

type machine =
  | Amd64                     (* x86-64 System V *)
  | Riscv64                   (* RV64, the lp64d ABI *)

(* Set once by the driver, from the command line or the machine it is
   running on: a compilation is for one machine. *)
let machine = ref Amd64

let name = function Amd64 -> "x86_64" | Riscv64 -> "riscv64"

let pointer_size = 8

let size_of_ikind : Ctype.ikind -> int = function
  | Bool | Char | SChar | UChar -> 1
  | Short | UShort -> 2
  | Int | UInt -> 4
  | Long | ULong | LLong | ULLong -> 8

let size_of_fkind : Ctype.fkind -> int = function
  | Float -> 4 | Double -> 8 | LongDouble -> 16

(* Scalars are aligned to their size, except long double (16). *)
let align_of_ikind = size_of_ikind
let align_of_fkind = size_of_fkind

(* 6.7.2.2p4: the underlying type of an enumeration is implementation-
   defined.  We follow gcc: unsigned int unless an enumerator is negative. *)
let enum_underlying ~has_negative : Ctype.t = if has_negative then Ctype.int else Ctype.uint

let max_align = 16 (* 6.2.8: the greatest fundamental alignment *)
