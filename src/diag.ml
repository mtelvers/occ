exception Error of Loc.t * string

let error loc fmt =
  Format.kasprintf (fun msg -> raise (Error (loc, msg))) fmt

let warning loc fmt =
  Format.kfprintf
    (fun ppf -> Format.fprintf ppf "@.")
    Format.err_formatter
    ("%a: warning: " ^^ fmt)
    Loc.pp loc

let report loc msg = Format.eprintf "%a: error: %s@." Loc.pp loc msg
