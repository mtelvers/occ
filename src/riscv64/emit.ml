(* Printing an RV64 program in the syntax GNU as accepts.  The only
   module that knows how the assembler spells things, as on the other
   machine.

   RISC-V assembly is written destination first, like the machine's own
   manuals and unlike AT&T syntax for x86: `add a0,a1,a2' puts a1+a2 in
   a0.  A load or a store names an offset and a base register in
   parentheses, and that is the only way to reach memory.

   One spelling differs in a way worth naming: `.align' here counts
   powers of two, not bytes, so an object wanting eight-byte alignment
   asks for `.align 3'. *)

open Asm

let reg = function
  | Zero -> "zero"
  | RA -> "ra" | SP -> "sp" | GP -> "gp" | TP -> "tp"
  | T n -> "t" ^ string_of_int n
  | S n -> "s" ^ string_of_int n
  | A n -> "a" ^ string_of_int n
  | FT n -> "ft" ^ string_of_int n
  | FS n -> "fs" ^ string_of_int n
  | FA n -> "fa" ^ string_of_int n

let operand = function
  | Imm v -> Int64.to_string v
  | Reg r -> reg r
  | Mem (r, off) -> Printf.sprintf "%d(%s)" off (reg r)
  | Sym (s, 0) -> s
  | Sym (s, n) when n > 0 -> Printf.sprintf "%s+%d" s n
  | Sym (s, n) -> Printf.sprintf "%s-%d" s (- n)

let instr ppf = function
  | Label l -> Format.fprintf ppf "%s:@." l
  | Directive (d, []) -> Format.fprintf ppf "\t.%s@." d
  | Directive (d, args) -> Format.fprintf ppf "\t.%s\t%s@." d (String.concat ", " args)
  | Op (m, []) -> Format.fprintf ppf "\t%s@." m
  | Op (m, ops) -> Format.fprintf ppf "\t%s\t%s@." m (String.concat "," (List.map operand ops))
  | Raw text -> Format.fprintf ppf "%s@." text
  | Loc (file, line) -> Format.fprintf ppf "\t.loc\t%d %d@." file line

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

let section_directive = function
  | Data -> "\t.data"
  | Bss -> "\t.bss"
  | Rodata -> "\t.section .rodata"
  | Tdata -> "\t.section .tdata,\"awT\",@progbits"
  | Tbss -> "\t.section .tbss,\"awT\",@nobits"

(* the binding and visibility directives shared by data and functions *)
let linkage ppf ~global ~weak ~hidden name =
  let p fmt = Format.fprintf ppf fmt in
  if weak then p "\t.weak\t%s@." name else if global then p "\t.globl\t%s@." name;
  if hidden then p "\t.hidden\t%s@." name

(* the power of two the assembler wants, from an alignment in bytes *)
let align_log2 n =
  let rec go k n = if n <= 1 then k else go (k + 1) (n / 2) in
  go 0 (max 1 n)

let data ppf (d : data) =
  let p fmt = Format.fprintf ppf fmt in
  linkage ppf ~global:d.dglobal ~weak:d.dweak ~hidden:d.dhidden d.dname;
  if d.ddecl then () else
  (match d.dalias with
   | Some target ->
       p "\t.type\t%s, @%s@." d.dname (if d.dfunc then "function" else if d.dtls then "tls_object" else "object");
       p "\t.set\t%s, %s@." d.dname target
   | None ->
       p "%s@." (section_directive d.section);
       p "\t.align\t%d@." (align_log2 d.dalign);
       p "\t.type\t%s, @%s@." d.dname (match d.section with Tdata | Tbss -> "tls_object" | _ -> "object");
       p "\t.size\t%s, %d@." d.dname d.size;
       p "%s:@." d.dname;
       List.iter (function
           | Bytes s -> p "\t.ascii\t\"%s\"@." (escape s)
           | Zeros n -> p "\t.zero\t%d@." n
           | Quad_sym (s, 0L) -> p "\t.quad\t%s@." s
           | Quad_sym (s, o) -> p "\t.quad\t%s%+Ld@." s o
           | Quad v -> p "\t.quad\t%Ld@." v
           | Long v -> p "\t.long\t%ld@." v) d.items)

let func ppf (f : func) =
  let p fmt = Format.fprintf ppf fmt in
  p "\t.text@.";
  linkage ppf ~global:f.global ~weak:f.weak ~hidden:f.hidden f.name;
  p "\t.type\t%s, @function@." f.name;
  p "%s:@." f.name;
  if f.debug then p ".LFB.%s:@." f.name;
  List.iter (instr ppf) f.body;
  if f.debug then p ".LFE.%s:@." f.name;
  p "\t.size\t%s, .-%s@." f.name f.name

let program ppf (prog : program) =
  (* the .file table comes first, before any .loc refers to it *)
  List.iter (fun (n, name) -> Format.fprintf ppf "\t.file\t%d \"%s\"@." n (escape name)) prog.files;
  List.iter (fun text -> Format.fprintf ppf "%s@." text) prog.asm_blocks;
  List.iter (data ppf) prog.data;
  (* Debug information here is the line table only: the .file and .loc
     directives, which the assembler turns into .debug_line.  The tree
     of debugging information entries that the other machine also emits
     is not written yet. *)
  ignore prog.source;
  List.iter (func ppf) prog.funcs;
  let array name entries =
    if entries <> [] then begin
      Format.fprintf ppf "\t.section %s,\"aw\",@init_array@." name;
      List.iter (fun (prio, fn) -> ignore prio; Format.fprintf ppf "\t.align\t3@.\t.quad\t%s@." fn)
        (List.stable_sort (fun (a, _) (b, _) -> compare a b) entries)
    end in
  array ".init_array" prog.init_array;
  array ".fini_array" prog.fini_array;
  Format.fprintf ppf "\t.section .note.GNU-stack,\"\",@progbits@."
