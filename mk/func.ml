(* Pure string helpers behind make's built-in functions (GNU make manual
   chapter 8): pattern matching for patsubst and pattern rules, and the
   file-name splitting the text functions use.  The functions themselves
   live in Expand, since most expand their arguments and so are mutually
   recursive with expansion. *)

let names = [
  "subst"; "patsubst"; "strip"; "findstring"; "filter"; "filter-out";
  "sort"; "word"; "wordlist"; "words"; "firstword"; "lastword";
  "dir"; "notdir"; "suffix"; "basename"; "addsuffix"; "addprefix"; "join"; "wildcard"; "realpath"; "abspath";
  "if"; "or"; "and"; "foreach"; "call"; "value"; "eval"; "origin"; "flavor";
  "error"; "warning"; "info"; "shell" ]

let ends_with suf s =
  let ls = String.length s and lf = String.length suf in
  ls >= lf && String.sub s (ls - lf) lf = suf

(* A pattern is a string with at most one '%'.  [pattern_match] returns
   the stem the '%' stands for, or None. *)
let pattern_match pat s =
  match String.index_opt pat '%' with
  | None -> if pat = s then Some "" else None
  | Some p ->
      let prefix = String.sub pat 0 p and suffix = String.sub pat (p + 1) (String.length pat - p - 1) in
      let ls = String.length s in
      if ls >= p + (String.length pat - p - 1)
         && String.length s >= String.length prefix + String.length suffix
         && String.sub s 0 (String.length prefix) = prefix
         && ends_with suffix s
      then Some (String.sub s (String.length prefix) (ls - String.length prefix - String.length suffix))
      else None

(* Substitute a matched stem into a replacement pattern (its '%', if any). *)
let pattern_subst repl stem =
  match String.index_opt repl '%' with
  | None -> repl
  | Some p -> String.sub repl 0 p ^ stem ^ String.sub repl (p + 1) (String.length repl - p - 1)

(* file-name functions (8.3) operate on the last path component *)
let dir s = match String.rindex_opt s '/' with Some i -> String.sub s 0 (i + 1) | None -> "./"
let notdir s = match String.rindex_opt s '/' with Some i -> String.sub s (i + 1) (String.length s - i - 1) | None -> s

(* the suffix is the last "." in the last component, if any *)
let suffix s =
  let base = notdir s in
  match String.rindex_opt base '.' with Some i -> String.sub base i (String.length base - i) | None -> ""

let basename s =
  match String.rindex_opt s '.' with
  | Some i when (match String.rindex_opt s '/' with Some j -> j < i | None -> true) -> String.sub s 0 i
  | _ -> s

(* replace every occurrence of [from] in [s] with [into] *)
let subst from into s =
  if from = "" then s
  else begin
    let b = Buffer.create (String.length s) in
    let lf = String.length from and n = String.length s in
    let i = ref 0 in
    while !i < n do
      if !i + lf <= n && String.sub s !i lf = from then (Buffer.add_string b into; i := !i + lf)
      else (Buffer.add_char b s.[!i]; incr i)
    done;
    Buffer.contents b
  end

(* GNU make holds a file name without any leading "./": a rule for
   `runtime/sak' and a prerequisite written `./runtime/sak' name the same
   file, and a pattern-specific variable for `runtime/%' has to apply to
   both. *)
let normalise name =
  let n = String.length name in
  let rec skip i =
    if i + 2 <= n && name.[i] = '.' && name.[i + 1] = '/' then begin
      let j = ref (i + 2) in
      while !j < n && name.[!j] = '/' do incr j done;
      if !j < n then skip !j else i
    end else i in
  let start = skip 0 in
  if start = 0 then name else String.sub name start (n - start)
