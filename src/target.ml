(* The one place the front end learns machine facts: sizes and alignments
   of the scalar types on x86-64 System V (ABI 3.1.2).  Aggregates are
   laid out by [Env] from these. *)

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
