(* The syntax of the shell command language (IEEE Std 1003.1-2017, XCU 2).

   A word keeps its quoting, because whether a character was quoted
   decides three later questions: whether an expansion happens inside it
   (2.2.2), whether the result is split into fields (2.6.5), and whether
   it is a pattern (2.6.6).  A word is therefore a list of parts, each
   knowing how it was written, and not a string. *)

type word = part list

and part =
  | Str of string                    (* unquoted literal text *)
  | Single of string                 (* '...', where nothing is special *)
  | Double of part list              (* "...": expansions, but no splitting *)
  | Esc of char                      (* \c outside quotes *)
  | Param of pexp                    (* $x or ${...} *)
  | Subst of program                 (* $(...) or `...` *)
  | Arith of part list               (* $(( ... )) *)

and pexp = { pname : string; pop : pop }

and pop =
  | Get                              (* ${x} *)
  | Length                           (* ${#x} *)
  | Default of bool * word           (* ${x-w}  ${x:-w}   (colon = true) *)
  | Assign of bool * word            (* ${x=w}  ${x:=w} *)
  | Fail of bool * word              (* ${x?w}  ${x:?w} *)
  | Alt of bool * word               (* ${x+w}  ${x:+w} *)
  | Prefix of bool * word            (* ${x#pat}  ${x##pat}  (longest = true) *)
  | Suffix of bool * word            (* ${x%pat}  ${x%%pat} *)

(* A redirection (2.7).  [rfd] is the descriptor it acts on, which each
   operator gives a default for; the word is the target, or for a
   here-document the body, already read by the lexer. *)
and redirect = { rfd : int; rop : rop; rword : word }

and rop =
  | Out                              (* > *)
  | Out_force                        (* >| *)
  | Append                           (* >> *)
  | In                               (* < *)
  | In_out                           (* <> *)
  | Dup_out                          (* >& *)
  | Dup_in                           (* <& *)
  | Here of bool                     (* << or <<-; true if the body expands *)

and command =
  | Simple of simple
  | Group of program * redirect list          (* { ...; } *)
  | Subshell of program * redirect list       (* ( ... ) *)
  | For of string * word list option * program * redirect list
  | Case of word * (word list * program) list * redirect list
  | If of (program * program) list * program option * redirect list
  | Loop of loop
  | Funcdef of string * command

and simple = {
  assigns : (string * word) list;
  words : word list;
  redirs : redirect list;
  line : int;                        (* for $LINENO and for diagnostics *)
}

and loop = { until : bool; cond : program; body : program; lredirs : redirect list }

and pipeline = { negate : bool; parts : command list }

(* an AND-OR list (2.9.3): the first pipeline and the ones that follow,
   each with the operator that joined it (true for &&) *)
and and_or = { first : pipeline; rest : (bool * pipeline) list }

and stmt = { ao : and_or; async : bool }

and program = stmt list

(* the redirections of any command, for the executor *)
let redirs_of = function
  | Simple s -> s.redirs
  | Group (_, r) | Subshell (_, r) | For (_, _, _, r) | Case (_, _, r)
  | If (_, _, r) -> r
  | Loop l -> l.lredirs
  | Funcdef _ -> []
