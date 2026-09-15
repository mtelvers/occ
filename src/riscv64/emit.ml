(* Printing an RV64 program in the syntax GNU as accepts.  The only
   module that knows how the assembler spells things, as on the other
   machine.

   RISC-V assembly is written destination first, like the machine's own
   manuals and unlike AT&T syntax for x86: `add a0,a1,a2' puts a1+a2 in
   a0.  A load or a store names an offset and a base register in
   parentheses, and that is the only way to reach memory. *)

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

let program ppf (p : program) =
  List.iter (instr ppf) p.data;
  List.iter (fun (f : func) -> List.iter (instr ppf) f.body) p.funcs
