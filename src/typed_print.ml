(* A readable rendering of [Typed] for --dump=typed: every expression with
   its type, so the conversions elaboration inserted can be seen. *)

open Typed

let p ppf fmt = Format.fprintf ppf fmt

let sym ppf (s : symbol) =
  p ppf "%s#%d" s.name s.id

let rec expr ppf (e : expr) =
  (match e.e with
   | Var s -> sym ppf s
   | Int v -> p ppf "%Ld" v
   | Float f -> p ppf "%h" f
   | String s -> p ppf "%S" s
   | Convert x -> p ppf "(convert %a)" expr x
   | Unop (op, x) -> p ppf "(%s %a)" (Syntax_print.unop op) expr x
   | Binop (op, a, b) -> p ppf "(%s %a %a)" (Syntax_print.binop op) expr a expr b
   | Assign (l, r) -> p ppf "(= %a %a)" expr l expr r
   | Compound_assign (op, l, r, ty) -> p ppf "(%s= %a %a in %a)" (Syntax_print.binop op) expr l expr r Ctype.pp ty
   | Cond (c, a, b) -> p ppf "(?: %a %a %a)" expr c expr a expr b
   | Call (f, args) -> p ppf "(call %a%a)" expr f (fun ppf -> List.iter (fun a -> p ppf " %a" expr a)) args
   | Deref x -> p ppf "(* %a)" expr x
   | Addr x -> p ppf "(& %a)" expr x
   | Member (x, f) -> p ppf "(. %a %s@%d%s)" expr x (Option.value f.fname ~default:"<anon>") f.offset
                        (match f.bits with Some (b, w) -> Printf.sprintf " bits %d+%d" b w | None -> "")
   | Compound_literal (s, _) -> p ppf "(compound-literal %a)" sym s
   | Va_arg (ap, ty) -> p ppf "(va_arg %a %a)" expr ap Ctype.pp ty
   | Builtin (name, args) -> p ppf "(%s%a)" name (fun ppf -> List.iter (fun a -> p ppf " %a" expr a)) args
   | Atomic_op (op, _, args) ->
       let name = match op with
         | Load -> "atomic-load" | Store -> "atomic-store" | Exchange -> "atomic-exchange"
         | Compare_exchange _ -> "atomic-compare-exchange" | Fetch op -> "atomic-fetch-" ^ Syntax_print.binop op
         | Fence -> "atomic-fence" | Signal_fence -> "atomic-signal-fence" in
       p ppf "(%s%a)" name (fun ppf -> List.iter (fun a -> p ppf " %a" expr a)) args);
  p ppf ":%a%s" Ctype.pp e.ty (if e.lvalue then "&" else "")

let rec init ppf = function
  | Init_scalar e -> expr ppf e
  | Init_string s -> p ppf "%S" s
  | Init_agg items ->
      p ppf "{%a}" (Format.pp_print_list ~pp_sep:(fun ppf () -> p ppf ", ")
                      (fun ppf (it : init_item) -> p ppf "@%d: %a" it.off init it.init)) items

let rec stmt indent ppf (s : stmt) =
  let ind = String.make indent ' ' in
  let sub = stmt (indent + 2) in
  match s with
  | Expr e -> p ppf "%s%a@." ind expr e
  | Decl (sy, i) -> p ppf "%sdeclare %a : %a%a@." ind sym sy Ctype.pp sy.ty (fun ppf -> function Some i -> p ppf " = %a" init i | None -> ()) i
  | Block ss -> p ppf "%sblock@.%a" ind (fun ppf -> List.iter (sub ppf)) ss
  | If (c, a, b) -> p ppf "%sif %a@.%a" ind expr c sub a; (match b with Some b -> p ppf "%selse@.%a" ind sub b | None -> ())
  | Switch (c, body) -> p ppf "%sswitch %a@.%a" ind expr c sub body
  | Case (v, body) -> p ppf "%scase %Ld:@.%a" ind v sub body
  | Default body -> p ppf "%sdefault:@.%a" ind sub body
  | While (c, body) -> p ppf "%swhile %a@.%a" ind expr c sub body
  | Do (body, c) -> p ppf "%sdo@.%a%swhile %a@." ind sub body ind expr c
  | For (i, c, st, body) ->
      p ppf "%sfor@." ind;
      (match i with Some i -> sub ppf i | None -> ());
      p ppf "%s  cond %a; step %a@.%a" ind (fun ppf -> function Some e -> expr ppf e | None -> ()) c
        (fun ppf -> function Some e -> expr ppf e | None -> ()) st sub body
  | Label (l, body) -> p ppf "%s%s:@.%a" ind l sub body
  | Goto l -> p ppf "%sgoto %s@." ind l
  | Continue -> p ppf "%scontinue@." ind
  | Break -> p ppf "%sbreak@." ind
  | Return None -> p ppf "%sreturn@." ind
  | Return (Some e) -> p ppf "%sreturn %a@." ind expr e
  | Asm a -> p ppf "%sasm %S@." ind a.template

let translation_unit ppf (tu : translation_unit) =
  List.iter (fun (g : global) ->
      p ppf "%s %a : %a%a@." (if g.defined then "global" else "extern") sym g.gsym Ctype.pp g.gsym.ty
        (fun ppf -> function Some i -> p ppf " = %a" init i | None -> ()) g.ginit) tu.globals;
  List.iter (fun (f : func) ->
      p ppf "function %a : %a (%a)%s@." sym f.fsym Ctype.pp f.fsym.ty
        (Format.pp_print_list ~pp_sep:(fun ppf () -> p ppf ", ") sym) f.params (if f.inline then " inline" else "");
      stmt 2 ppf f.body) tu.funcs
