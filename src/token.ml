(* Tokens after translation phase 7 (C11 5.1.1.2): preprocessing tokens
   have been converted to tokens, so keywords are distinguished from
   identifiers and constants have been evaluated.

   The preprocessor works on a looser notion of token (6.4: pp-numbers,
   header names) and has its own type; see [Preprocess]. *)

(* 6.4.1 Keywords.  The last group are extensions the OCaml runtime relies
   on; they are listed in doc/extensions.md and nowhere else. *)
type keyword =
  | Auto | Break | Case | Char | Const | Continue | Default | Do | Double
  | Else | Enum | Extern | Float | For | Goto | If | Inline | Int | Long
  | Register | Restrict | Return | Short | Signed | Sizeof | Static | Struct
  | Switch | Typedef | Union | Unsigned | Void | Volatile | While
  | Alignas | Alignof | Atomic | Bool | Complex | Generic | Imaginary
  | Noreturn | Static_assert | Thread_local
  (* extensions *)
  | Attribute | Asm | Typeof

(* 6.4.6 Punctuators.  Digraphs are spelled the same as their primary
   form after phase 7, so they do not appear here. *)
type punct =
  | LBracket | RBracket | LParen | RParen | LBrace | RBrace
  | Dot | Arrow
  | PlusPlus | MinusMinus | Amp | Star | Plus | Minus | Tilde | Bang
  | Slash | Percent | LShift | RShift | Lt | Gt | Le | Ge | EqEq | BangEq
  | Caret | Bar | AmpAmp | BarBar
  | Question | Colon | Semi | Ellipsis
  | Eq | StarEq | SlashEq | PercentEq | PlusEq | MinusEq | LShiftEq
  | RShiftEq | AmpEq | CaretEq | BarEq
  | Comma | Hash | HashHash

(* 6.4.4.1 The type of an integer constant is determined by its value and
   suffix; we keep both and let elaboration apply the table in 6.4.4.1p5. *)
type int_suffix = { unsigned : bool; longs : int (* 0, 1 or 2 *) }

type float_suffix = F_none | F_f | F_l

(* 6.4.5 Encoding prefixes on string literals. *)
type encoding = Plain | Utf8 | Wide | Char16 | Char32

type t =
  | Keyword of keyword
  | Ident of string
  | Int_const of { value : string; radix : int; suffix : int_suffix }
  | Float_const of { text : string; suffix : float_suffix }
  | Char_const of { chars : int list; enc : encoding }
  | String_lit of { bytes : string; enc : encoding }
  | Punct of punct
  | Eof

type loc_token = { tok : t; loc : Loc.t }

let keywords : (string * keyword) list =
  [ "auto", Auto; "break", Break; "case", Case; "char", Char; "const", Const;
    "continue", Continue; "default", Default; "do", Do; "double", Double;
    "else", Else; "enum", Enum; "extern", Extern; "float", Float; "for", For;
    "goto", Goto; "if", If; "inline", Inline; "int", Int; "long", Long;
    "register", Register; "restrict", Restrict; "return", Return;
    "short", Short; "signed", Signed; "sizeof", Sizeof; "static", Static;
    "struct", Struct; "switch", Switch; "typedef", Typedef; "union", Union;
    "unsigned", Unsigned; "void", Void; "volatile", Volatile; "while", While;
    "_Alignas", Alignas; "_Alignof", Alignof; "_Atomic", Atomic;
    "_Bool", Bool; "_Complex", Complex; "_Generic", Generic;
    "_Imaginary", Imaginary; "_Noreturn", Noreturn;
    "_Static_assert", Static_assert; "_Thread_local", Thread_local;
    (* extensions, see doc/extensions.md *)
    "__attribute__", Attribute; "__asm__", Asm; "asm", Asm;
    "__inline", Inline; "__inline__", Inline; "__restrict", Restrict;
    "__signed__", Signed; "__signed", Signed; "__volatile__", Volatile; "__volatile", Volatile;
    "__const", Const; "__const__", Const; "__restrict__", Restrict; "__asm", Asm; "__attribute", Attribute;
    "__alignof__", Alignof; "__alignof", Alignof;
    "typeof", Typeof; "__typeof__", Typeof; "__typeof", Typeof ]

let keyword_of_string s = List.assoc_opt s keywords

(* A readable rendering for --dump=tokens; not a re-lexable one. *)
let punct_to_string = function
  | LBracket -> "[" | RBracket -> "]" | LParen -> "(" | RParen -> ")" | LBrace -> "{"
  | RBrace -> "}" | Dot -> "." | Arrow -> "->" | PlusPlus -> "++" | MinusMinus -> "--"
  | Amp -> "&" | Star -> "*" | Plus -> "+" | Minus -> "-" | Tilde -> "~" | Bang -> "!"
  | Slash -> "/" | Percent -> "%" | LShift -> "<<" | RShift -> ">>" | Lt -> "<" | Gt -> ">"
  | Le -> "<=" | Ge -> ">=" | EqEq -> "==" | BangEq -> "!=" | Caret -> "^" | Bar -> "|"
  | AmpAmp -> "&&" | BarBar -> "||" | Question -> "?" | Colon -> ":" | Semi -> ";"
  | Ellipsis -> "..." | Eq -> "=" | StarEq -> "*=" | SlashEq -> "/=" | PercentEq -> "%="
  | PlusEq -> "+=" | MinusEq -> "-=" | LShiftEq -> "<<=" | RShiftEq -> ">>=" | AmpEq -> "&="
  | CaretEq -> "^=" | BarEq -> "|=" | Comma -> "," | Hash -> "#" | HashHash -> "##"

let keyword_to_string k =
  fst (List.find (fun (_, k') -> k' = k) keywords)

let pp ppf = function
  | Keyword k -> Format.fprintf ppf "keyword %s" (keyword_to_string k)
  | Ident s -> Format.fprintf ppf "ident %s" s
  | Int_const { value; radix; suffix } ->
      Format.fprintf ppf "int %s (radix %d%s%s)" value radix
        (if suffix.unsigned then " unsigned" else "") (String.make suffix.longs 'L')
  | Float_const { text; suffix } ->
      Format.fprintf ppf "float %s%s" text (match suffix with F_none -> "" | F_f -> "f" | F_l -> "l")
  | Char_const { chars; _ } ->
      Format.fprintf ppf "char %s" (String.concat "," (List.map string_of_int chars))
  | String_lit { bytes; _ } -> Format.fprintf ppf "string %S" bytes
  | Punct p -> Format.fprintf ppf "punct %s" (punct_to_string p)
  | Eof -> Format.fprintf ppf "eof"
