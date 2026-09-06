type t = { file : string; line : int; col : int }

let none = { file = "<none>"; line = 0; col = 0 }

let pp ppf { file; line; col } =
  if line = 0 then Format.pp_print_string ppf file
  else Format.fprintf ppf "%s:%d:%d" file line col
