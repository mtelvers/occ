(* C types, C11 6.2.5, with qualifiers (6.7.3).

   A type is a qualifier set applied to an unqualified type.  Structure,
   union and enumeration types are identified by a tag: two struct types
   are the same type only if they come from the same declaration
   (6.2.7p1), so identity is the tag id, and the name is carried only for
   diagnostics.  The members live in [Env], because they may be completed
   after the type is first mentioned (6.2.5p22). *)

type qual = { const : bool; volatile : bool; restrict : bool; atomic : bool }

let no_qual = { const = false; volatile = false; restrict = false; atomic = false }

(* 6.2.5p4-6, in order of conversion rank (6.3.1.1p1). *)
type ikind =
  | Bool
  | Char | SChar | UChar
  | Short | UShort
  | Int | UInt
  | Long | ULong
  | LLong | ULLong

type fkind = Float | Double | LongDouble

type tag = { id : int; name : string option }

type t = { q : qual; u : u }

and u =
  | Void
  | Integer of ikind
  | Floating of fkind
  | Pointer of t
  | Array of t * int option (* None: incomplete, 6.7.6.2 *)
  | Vla of t * int (* variable length array (6.7.6.2p4): element type and the id of its size, known at run time *)
  | Func of func
  | Struct of tag
  | Union of tag
  | Enum of tag

(* 6.7.6.3.  [params = None] is a function without a prototype, which the
   runtime still contains in a few places. *)
and func = { ret : t; params : param list option; variadic : bool }

and param = { pname : string option; ptype : t }

(* A laid-out member of a struct or union.  [bits = Some (bit_offset,
   width)] for a bit-field, whose [offset] is then that of the storage
   unit holding it.  Layouts live in [Env]; the record is here so that the
   typed syntax can refer to members without depending on the environment. *)
type field = { fname : string option; ftype : t; offset : int; bits : (int * int) option }

let unqualified u = { q = no_qual; u }
let void = unqualified Void
let int = unqualified (Integer Int)
let uint = unqualified (Integer UInt)
let long = unqualified (Integer Long)
let ulong = unqualified (Integer ULong)
let char = unqualified (Integer Char)
let bool = unqualified (Integer Bool)
let double = unqualified (Floating Double)
let pointer t = unqualified (Pointer t)
let array t n = unqualified (Array (t, n))
let vla t id = unqualified (Vla (t, id))

(* wchar_t, char16_t, char32_t, size_t and ptrdiff_t on x86-64 System V. *)
let wchar = int
let char16 = unqualified (Integer UShort)
let char32 = uint
let size_t = ulong
let ptrdiff_t = long

let strip t = { t with q = no_qual }
let is_qualified t = t.q <> no_qual
let merge_qual a b =
  { const = a.const || b.const; volatile = a.volatile || b.volatile;
    restrict = a.restrict || b.restrict; atomic = a.atomic || b.atomic }

(* ---- Classification (6.2.5p17-21) ----------------------------------------- *)

let is_integer t = match t.u with Integer _ | Enum _ -> true | _ -> false
let is_floating t = match t.u with Floating _ -> true | _ -> false
let is_arithmetic t = is_integer t || is_floating t
let is_pointer t = match t.u with Pointer _ -> true | _ -> false
let is_scalar t = is_arithmetic t || is_pointer t
let is_array t = match t.u with Array _ | Vla _ -> true | _ -> false

(* does the size of this type depend on a run-time value? *)
let rec has_vla t = match t.u with Vla _ -> true | Array (e, _) -> has_vla e | _ -> false
let is_function t = match t.u with Func _ -> true | _ -> false
let is_void t = t.u = Void
let is_record t = match t.u with Struct _ | Union _ -> true | _ -> false

let rank = function
  | Bool -> 0 | Char | SChar | UChar -> 1 | Short | UShort -> 2
  | Int | UInt -> 3 | Long | ULong -> 4 | LLong | ULLong -> 5

(* char is signed on x86-64 (6.2.5p15, implementation-defined). *)
let is_signed = function
  | Bool | UChar | UShort | UInt | ULong | ULLong -> false
  | Char | SChar | Short | Int | Long | LLong -> true

let to_unsigned = function
  | Char | SChar -> UChar | Short -> UShort | Int -> UInt | Long -> ULong | LLong -> ULLong | k -> k

let to_signed = function
  | UChar -> SChar | UShort -> Short | UInt -> Int | ULong -> Long | ULLong -> LLong | k -> k

(* ---- Compatibility (6.2.7) ---------------------------------------------------- *)

let rec compatible a b =
  a.q = b.q &&
  match a.u, b.u with
  | Void, Void -> true
  | Integer x, Integer y -> x = y
  | Floating x, Floating y -> x = y
  | Pointer x, Pointer y -> compatible x y
  | Array (x, n), Array (y, m) -> compatible x y && (n = None || m = None || n = m)
  | Vla (x, _), (Array (y, _) | Vla (y, _)) | Array (x, _), Vla (y, _) -> compatible x y   (* 6.7.6.2p6 *)
  | (Struct x | Union x | Enum x), (Struct y | Union y | Enum y) -> x.id = y.id
  | Func f, Func g ->
      compatible f.ret g.ret &&
      (match f.params, g.params with
       | Some ps, Some qs ->
           f.variadic = g.variadic && List.length ps = List.length qs
           && List.for_all2 (fun p q -> compatible (strip p.ptype) (strip q.ptype)) ps qs
       | _ -> true) (* 6.7.6.3p15, loosely: an unprototyped declaration is compatible *)
  | _ -> false

(* The composite type of two compatible types (6.2.7p3): array sizes and
   parameter lists are taken from whichever declaration has them. *)
let rec composite a b =
  match a.u, b.u with
  | Array (x, None), Array (y, n) | Array (x, n), Array (y, None) -> { a with u = Array (composite x y, n) }
  | Func f, Func g ->
      let params = match f.params, g.params with
        | Some ps, Some qs -> Some (List.map2 (fun p q -> { p with ptype = composite p.ptype q.ptype }) ps qs)
        | Some ps, None | None, Some ps -> Some ps
        | None, None -> None in
      { a with u = Func { ret = composite f.ret g.ret; params; variadic = f.variadic || g.variadic } }
  | Pointer x, Pointer y -> { a with u = Pointer (composite x y) }
  | _ -> a

(* ---- Printing, in C declarator syntax --------------------------------------- *)

let pp_qual ppf q =
  if q.const then Format.pp_print_string ppf "const ";
  if q.volatile then Format.pp_print_string ppf "volatile ";
  if q.restrict then Format.pp_print_string ppf "restrict ";
  if q.atomic then Format.pp_print_string ppf "_Atomic "

let ikind_to_string = function
  | Bool -> "_Bool" | Char -> "char" | SChar -> "signed char" | UChar -> "unsigned char"
  | Short -> "short" | UShort -> "unsigned short" | Int -> "int" | UInt -> "unsigned int"
  | Long -> "long" | ULong -> "unsigned long" | LLong -> "long long" | ULLong -> "unsigned long long"

let fkind_to_string = function Float -> "float" | Double -> "double" | LongDouble -> "long double"

let tag_to_string kw t = kw ^ " " ^ (match t.name with Some n -> n | None -> "<anonymous>")

(* [pp] prints a type as an abstract declarator: the base type, then the
   declarator built inside-out with parentheses where * binds looser than
   [] and () (6.7.6). *)
let rec pp ppf t =
  let rec split t inner =
    (* returns base type and the declarator text around [inner] *)
    match t.u with
    | Pointer p ->
        let inner = Format.asprintf "*%a%s" pp_qual t.q inner in
        let inner = match p.u with Array _ | Func _ -> "(" ^ inner ^ ")" | _ -> inner in
        split p inner
    | Array (e, n) ->
        split e (Format.asprintf "%s[%s]" inner (match n with Some n -> string_of_int n | None -> ""))
    | Vla (e, _) -> split e (inner ^ "[*]")
    | Func f ->
        let params = match f.params with
          | None -> ""
          | Some [] -> "void"
          | Some ps ->
              String.concat ", " (List.map (fun p -> Format.asprintf "%a" pp p.ptype) ps)
              ^ (if f.variadic then ", ..." else "") in
        split f.ret (Format.asprintf "%s(%s)" inner params)
    | _ -> t, inner in
  let base, decl = split t "" in
  Format.fprintf ppf "%a%s%s" pp_qual base.q
    (match base.u with
     | Void -> "void" | Integer k -> ikind_to_string k | Floating k -> fkind_to_string k
     | Struct tag -> tag_to_string "struct" tag | Union tag -> tag_to_string "union" tag
     | Enum tag -> tag_to_string "enum" tag | Pointer _ | Array _ | Vla _ | Func _ -> assert false)
    (if decl = "" then "" else " " ^ decl)

let to_string t = Format.asprintf "%a" pp t
