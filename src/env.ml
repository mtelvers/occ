type entry =
  | Var of Typed.symbol
  | Typedef of Ctype.t
  | Enum_const of int64 * Ctype.t

type field = Ctype.field = { fname : string option; ftype : Ctype.t; offset : int; bits : (int * int) option }
type layout = { fields : field list; size : int; align : int }

type tag_info = {
  tag : Ctype.tag;
  kind : [ `Struct | `Union | `Enum ];
  mutable layout : layout option;
  mutable underlying : Ctype.t option;
}

type scope = { names : (string, entry) Hashtbl.t; tags : (string, tag_info) Hashtbl.t }

type t = {
  mutable scopes : scope list;
  infos : (int, tag_info) Hashtbl.t;
  mutable next_tag : int;
  mutable next_sym : int;
}

let new_scope () = { names = Hashtbl.create 16; tags = Hashtbl.create 4 }
let create () = { scopes = [ new_scope () ]; infos = Hashtbl.create 64; next_tag = 0; next_sym = 0 }
let push env = env.scopes <- new_scope () :: env.scopes
let pop env = env.scopes <- List.tl env.scopes
let at_file_scope env = List.length env.scopes = 1

let in_enclosing_scope env f =
  let saved = env.scopes in
  env.scopes <- List.tl saved;
  Fun.protect ~finally:(fun () -> env.scopes <- saved) f

let declare env name e = Hashtbl.replace (List.hd env.scopes).names name e
let rec find f = function [] -> None | s :: rest -> (match f s with Some x -> Some x | None -> find f rest)
let lookup env name = find (fun s -> Hashtbl.find_opt s.names name) env.scopes
let lookup_here env name = Hashtbl.find_opt (List.hd env.scopes).names name

let fresh_symbol env name ty storage : Typed.symbol =
  env.next_sym <- env.next_sym + 1;
  { Typed.id = env.next_sym; name; ty; storage; align = None; asm_name = None; link = Typed.no_attrs () }

let declare_tag env name info = Hashtbl.replace (List.hd env.scopes).tags name info
let lookup_tag env name = find (fun s -> Hashtbl.find_opt s.tags name) env.scopes
let lookup_tag_here env name = Hashtbl.find_opt (List.hd env.scopes).tags name

let new_tag env kind name =
  env.next_tag <- env.next_tag + 1;
  let info = { tag = { Ctype.id = env.next_tag; name }; kind; layout = None; underlying = None } in
  Hashtbl.replace env.infos env.next_tag info;
  info

let tag_info env (tag : Ctype.tag) = Hashtbl.find env.infos tag.id

(* ---- Sizes ---------------------------------------------------------------- *)

let rec size_align env (t : Ctype.t) =
  match t.u with
  | Void | Func _ -> None
  | Integer k -> Some (Target.size_of_ikind k, Target.align_of_ikind k)
  | Floating k -> Some (Target.size_of_fkind k, Target.align_of_fkind k)
  | Pointer _ -> Some (Target.pointer_size, Target.pointer_size)
  | Array (_, None) | Vla _ -> None   (* a variable length array has no size at compile time *)
  | Array (e, Some n) ->
      (match size_align env e with Some (s, a) -> Some (s * n, a) | None -> None)
  | Struct tag | Union tag ->
      (match (tag_info env tag).layout with Some l -> Some (l.size, l.align) | None -> None)
  | Enum tag ->
      (match (tag_info env tag).underlying with Some u -> size_align env u | None -> None)

let is_complete env (t : Ctype.t) = (match t.u with Ctype.Vla _ -> true | _ -> false) || size_align env t <> None

let size_of env loc t =
  match size_align env t with
  | Some (s, _) -> s
  | None -> Diag.error loc "invalid application of 'sizeof' to incomplete type '%a'" Ctype.pp t

let align_of env loc t =
  match size_align env t with
  | Some (_, a) -> a
  | None -> Diag.error loc "invalid application of '_Alignof' to incomplete type '%a'" Ctype.pp t

let round_up n a = (n + a - 1) / a * a

(* System V x86-64 layout (ABI 3.1.2).  Members are placed at increasing
   bit positions.  A non-bit-field is aligned to its type; a bit-field of
   a type of S bits may start at any position provided it does not cross
   an S-bit boundary, and its storage unit is the aligned S-bit unit that
   contains it.  A zero-width bit-field pads to the next unit boundary
   (6.7.2.1p11-12).  The aggregate's alignment is the maximum of its
   members' and its size is padded to it. *)
let layout_struct env ~is_union members loc =
  let bitpos = ref 0 and align = ref 1 and size = ref 0 in
  let fields =
    List.filter_map (fun (fname, ftype, width, align_spec) ->
        let s, a =
          match size_align env ftype with
          | Some sa -> sa
          | None ->
              (* a flexible array member (6.7.2.1p18) has size 0 here *)
              (match ftype.u with Array (e, None) -> 0, snd (Option.get (size_align env e))
               | _ -> Diag.error loc "field has incomplete type '%a'" Ctype.pp ftype) in
        (* 6.7.5p4: an alignment specifier may only strengthen alignment *)
        let a = match align_spec with Some n when n > a -> n | _ -> a in
        align := max !align a;
        let unit_bits = 8 * s in
        match width with
        | Some 0 ->
            if not is_union then bitpos := round_up !bitpos unit_bits;
            None
        | Some w ->
            let pos = if is_union then 0 else !bitpos in
            let pos = if pos mod unit_bits + w > unit_bits then round_up pos unit_bits else pos in
            let offset = pos / unit_bits * s in
            if is_union then size := max !size s
            else (bitpos := pos + w; size := max !size (offset + s));
            Some { fname; ftype; offset; bits = Some (pos mod unit_bits, w) }
        | None ->
            if is_union then begin
              size := max !size s;
              Some { fname; ftype; offset = 0; bits = None }
            end else begin
              let pos = round_up !bitpos (8 * a) in
              bitpos := pos + 8 * s;
              size := max !size (pos / 8 + s);
              Some { fname; ftype; offset = pos / 8; bits = None }
            end)
      members in
  { fields; size = round_up !size !align; align = !align }

let rec find_field env (t : Ctype.t) name =
  match t.u with
  | Struct tag | Union tag ->
      (match (tag_info env tag).layout with
       | None -> None
       | Some l ->
           let rec go = function
             | [] -> None
             | f :: rest ->
                 if f.fname = Some name then Some [ f ]
                 else if f.fname = None && Ctype.is_record f.ftype then
                   (match find_field env f.ftype name with Some path -> Some (f :: path) | None -> go rest)
                 else go rest in
           go l.fields)
  | _ -> None
