(* LEB128 variable-length integers (DWARF 4, section 7.6). *)

let uleb b v =
  let rec go v =
    let byte = v land 0x7f and rest = v lsr 7 in
    if rest = 0 then Buffer.add_char b (Char.chr byte)
    else begin Buffer.add_char b (Char.chr (byte lor 0x80)); go rest end in
  if v < 0 then invalid_arg "Leb.uleb";
  go v

let sleb b v =
  let rec go v =
    let byte = v land 0x7f and rest = v asr 7 in
    let done_ = (rest = 0 && byte land 0x40 = 0) || (rest = -1 && byte land 0x40 <> 0) in
    if done_ then Buffer.add_char b (Char.chr byte)
    else begin Buffer.add_char b (Char.chr (byte lor 0x80)); go rest end in
  go v

let u16 b v = Buffer.add_char b (Char.chr (v land 0xff)); Buffer.add_char b (Char.chr ((v lsr 8) land 0xff))
let u32 b v = u16 b (v land 0xffff); u16 b ((v lsr 16) land 0xffff)
let u64 b v = u32 b (v land 0xffffffff); u32 b ((v lsr 32) land 0xffffffff)
