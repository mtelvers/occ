(* DWARF 4: the tree of debugging information entries.

   This is not machine knowledge, which is why it is here rather than in
   a back end: the assembler builds .debug_line from the .file and .loc
   directives and .eh_frame from the .cfi directives, and what is left
   -- one compile unit, a subprogram per function with its formal
   parameters, and the types they mention -- is the same on every
   machine.  What the two back ends supply is where a parameter lives,
   since a register's DWARF number is the machine's own.

   Only as much of the format as parameters and results need (see
   doc/phases.md).  Abbreviation codes are fixed: 1 compile unit, 2
   subprogram with a return type, 3 subprogram returning void, 4 formal
   parameter, 5 base type, 6 pointer type, 7 structure declaration, 8
   union declaration. *)

type ty =
  | Void
  | Base of string * int * int (* name, DW_ATE encoding, byte size *)
  | Pointer (* to void: pointee types are not described *)
  | Struct of string
  | Union of string

type location =
  | At_cfa_offset of int (* DW_OP_fbreg *)
  | In_register of int (* DW_OP_regN, by the machine's DWARF numbering *)

type param = { pname : string; ptype : ty; ploc : location }

type func = {
  dfile : int; (* index in the .file table *)
  dline : int;
  dparams : param list;
  dret : ty;
}

(* one entry per function that has debugging information *)
type subprogram = { sname : string; sglobal : bool; sinfo : func }

(* What DWARF is told about a C type: scalars exactly, aggregates by name. *)
let of_ctype (t : Ctype.t) : ty =
  match t.u with
  | Ctype.Void -> Void
  | Ctype.Integer k ->
      let size = Target.size_of_ikind k in
      let enc = match k with
        | Ctype.Bool -> 2 | Ctype.Char | Ctype.SChar -> 6 | Ctype.UChar -> 8
        | k when Ctype.is_signed k -> 5 | _ -> 7 in
      Base (Ctype.ikind_to_string k, enc, size)
  | Ctype.Floating k -> Base (Ctype.fkind_to_string k, 4, Target.size_of_fkind k)
  | Ctype.Enum _ -> Base ("unsigned int", 7, 4)
  | Ctype.Pointer _ | Ctype.Array _ | Ctype.Vla _ | Ctype.Func _ -> Pointer
  | Ctype.Struct tag -> Struct (Option.value tag.name ~default:"<anonymous>")
  | Ctype.Union tag -> Union (Option.value tag.name ~default:"<anonymous>")

(* A string as the assembler wants to see it, which the data of both
   machines needs as much as the debugging strings do. *)
let escape s =
  let b = Buffer.create (String.length s * 2) in
  String.iter (fun c ->
      match c with
      | '"' -> Buffer.add_string b "\\\""
      | '\\' -> Buffer.add_string b "\\\\"
      | '\n' -> Buffer.add_string b "\\n"
      | c when Char.code c < 32 || Char.code c >= 127 -> Buffer.add_string b (Printf.sprintf "\\%03o" (Char.code c))
      | c -> Buffer.add_char b c) s;
  Buffer.contents b

let abbrevs = [
  (* code, tag, has children, [attribute, form] *)
  1, 0x11, true,  [ 0x25, 0x08; 0x13, 0x0b; 0x03, 0x08; 0x1b, 0x08; 0x11, 0x01; 0x12, 0x07; 0x10, 0x17 ];
  2, 0x2e, true,  [ 0x3f, 0x0c; 0x03, 0x08; 0x3a, 0x0b; 0x3b, 0x05; 0x49, 0x13; 0x11, 0x01; 0x12, 0x07; 0x40, 0x18 ];
  3, 0x2e, true,  [ 0x3f, 0x0c; 0x03, 0x08; 0x3a, 0x0b; 0x3b, 0x05; 0x11, 0x01; 0x12, 0x07; 0x40, 0x18 ];
  4, 0x05, false, [ 0x03, 0x08; 0x49, 0x13; 0x02, 0x18 ];
  5, 0x24, false, [ 0x0b, 0x0b; 0x3e, 0x0b; 0x03, 0x08 ];
  6, 0x0f, false, [ 0x0b, 0x0b ];
  7, 0x13, false, [ 0x03, 0x08; 0x3c, 0x19 ];
  8, 0x17, false, [ 0x03, 0x08; 0x3c, 0x19 ];
]

let sleb128_size v =
  let rec go v n =
    let v' = Int64.shift_right v 7 in
    if (v' = 0L && Int64.logand v 0x40L = 0L) || (v' = -1L && Int64.logand v 0x40L <> 0L) then n + 1
    else go v' (n + 1) in
  go (Int64.of_int v) 0

let info ppf (subprograms : subprogram list) ~source =
  let p fmt = Format.fprintf ppf fmt in
  (* one DIE per distinct type, labelled for ref4 references *)
  let types = Hashtbl.create 16 in
  let type_label t =
    match t with
    | Void -> None
    | _ -> (match Hashtbl.find_opt types t with
        | Some l -> Some l
        | None -> let l = Printf.sprintf ".Ltype%d" (Hashtbl.length types) in Hashtbl.replace types t l; Some l) in
  (* collect types first so their labels exist when parameters refer to them *)
  List.iter (fun s ->
      ignore (type_label s.sinfo.dret);
      List.iter (fun pr -> ignore (type_label pr.ptype)) s.sinfo.dparams) subprograms;
  p "\t.section .debug_info,\"\",@progbits@.";
  p ".Ldebug_info0:@.";
  p "\t.long\t.Ldebug_info_end - .Ldebug_info_start@.";
  p ".Ldebug_info_start:@.";
  p "\t.short\t4@.\t.long\t.Ldebug_abbrev0@.\t.byte\t8@.";
  (* compile unit *)
  p "\t.uleb128 1@.\t.string\t\"occ 0.1\"@.\t.byte\t0x0c@.\t.string\t\"%s\"@.\t.string\t\"%s\"@." (escape source) (escape (Sys.getcwd ()));
  p "\t.quad\t.Ltext0@.\t.quad\t.Letext0-.Ltext0@.\t.long\t.Ldebug_line0@.";
  List.iter (fun s ->
      let d = s.sinfo in
      let ret = type_label d.dret in
      p "\t.uleb128 %d@." (if ret = None then 3 else 2);
      p "\t.byte\t%d@.\t.string\t\"%s\"@.\t.byte\t%d@.\t.short\t%d@." (if s.sglobal then 1 else 0) (escape s.sname) d.dfile d.dline;
      (match ret with Some l -> p "\t.long\t%s - .Ldebug_info0@." l | None -> ());
      p "\t.quad\t.LFB.%s@.\t.quad\t.LFE.%s - .LFB.%s@." s.sname s.sname s.sname;
      p "\t.uleb128 1@.\t.byte\t0x9c@."; (* frame base: DW_OP_call_frame_cfa *)
      List.iter (fun pr ->
          match type_label pr.ptype with
          | None -> ()
          | Some l ->
              p "\t.uleb128 4@.\t.string\t\"%s\"@.\t.long\t%s - .Ldebug_info0@." (escape pr.pname) l;
              (match pr.ploc with
               | At_cfa_offset off -> p "\t.uleb128 %d@.\t.byte\t0x91@.\t.sleb128 %d@." (1 + sleb128_size off) off
               | In_register n -> p "\t.uleb128 1@.\t.byte\t0x%x@." (0x50 + n))) d.dparams;
      p "\t.byte\t0@." (* end of children *)) subprograms;
  Hashtbl.iter (fun t l ->
      p "%s:@." l;
      match t with
      | Base (name, enc, size) -> p "\t.uleb128 5@.\t.byte\t%d@.\t.byte\t%d@.\t.string\t\"%s\"@." size enc (escape name)
      | Pointer -> p "\t.uleb128 6@.\t.byte\t8@."
      | Struct name -> p "\t.uleb128 7@.\t.string\t\"%s\"@." (escape name)
      | Union name -> p "\t.uleb128 8@.\t.string\t\"%s\"@." (escape name)
      | Void -> ()) types;
  p "\t.byte\t0@."; (* end of the compile unit's children *)
  p ".Ldebug_info_end:@.";
  p "\t.section .debug_abbrev,\"\",@progbits@.";
  p ".Ldebug_abbrev0:@.";
  List.iter (fun (code, tag, children, attrs) ->
      p "\t.uleb128 %d@.\t.uleb128 0x%x@.\t.byte\t%d@." code tag (if children then 1 else 0);
      List.iter (fun (a, f) -> p "\t.uleb128 0x%x@.\t.uleb128 0x%x@." a f) attrs;
      p "\t.byte\t0@.\t.byte\t0@.") abbrevs;
  p "\t.byte\t0@.";
  p "\t.section .debug_line,\"\",@progbits@.";
  p ".Ldebug_line0:@."
