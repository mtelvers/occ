(* basename, dirname, realpath, pwd and the trivial ones: echo, printf,
   test, true, false (IEEE Std 1003.1-2017, XCU).

   basename and dirname are defined on strings, not on the file system:
   `dirname /a/b' is "/a" whether or not either exists, and the standard
   spells out the steps, including that trailing slashes are removed
   first and that a name that is all slashes gives "/". *)

open Util

(* XCU basename, step by step *)
let base name suffix =
  if name = "" then ""
  else if String.for_all (fun c -> c = '/') name then "/"
  else begin
    let n = ref (String.length name) in
    while !n > 1 && name.[!n - 1] = '/' do decr n done;
    let name = String.sub name 0 !n in
    let name = match String.rindex_opt name '/' with
      | Some k -> String.sub name (k + 1) (String.length name - k - 1)
      | None -> name in
    match suffix with
    | Some s when s <> name && s <> "" && String.length name > String.length s
                  && String.sub name (String.length name - String.length s) (String.length s) = s ->
        String.sub name 0 (String.length name - String.length s)
    | _ -> name
  end

let basename _argv opts operands =
  ignore opts;
  match operands with
  | [] -> die 1 "usage: basename string [suffix]"
  | [ name ] -> emit_line (base name None); flush_out (); 0
  | name :: suffix :: _ -> emit_line (base name (Some suffix)); flush_out (); 0

(* XCU dirname *)
let dir name =
  if name = "" then "."
  else if String.for_all (fun c -> c = '/') name then "/"
  else begin
    let n = ref (String.length name) in
    while !n > 1 && name.[!n - 1] = '/' do decr n done;
    let name = String.sub name 0 !n in
    match String.rindex_opt name '/' with
    | None -> "."
    | Some 0 -> "/"
    | Some k ->
        let k = ref k in
        while !k > 1 && name.[!k - 1] = '/' do decr k done;
        String.sub name 0 !k
  end

let dirname _argv opts operands =
  ignore opts;
  match operands with
  | [] -> die 1 "usage: dirname string"
  | name :: _ -> emit_line (dir name); flush_out (); 0

(* realpath: the name with every symbolic link and . and .. resolved *)
let rec resolve ?(depth = 0) path =
  if depth > 40 then None
  else begin
    let path = if Filename.is_relative path then Filename.concat (Unix.getcwd ()) path else path in
    let parts = List.filter (fun p -> p <> "" && p <> ".") (String.split_on_char '/' path) in
    let rec walk acc = function
      | [] -> Some acc
      | ".." :: rest -> walk (match String.rindex_opt acc '/' with
          | Some 0 -> "/"
          | Some k -> String.sub acc 0 k
          | None -> "/") rest
      | p :: rest ->
          let here = if acc = "/" then "/" ^ p else acc ^ "/" ^ p in
          (match Unix.lstat here with
           | { Unix.st_kind = Unix.S_LNK; _ } ->
               (match Unix.readlink here with
                | link ->
                    let link = if Filename.is_relative link then
                        (if acc = "/" then "/" ^ link else acc ^ "/" ^ link)
                      else link in
                    (match resolve ~depth:(depth + 1) link with
                     | Some target -> walk target rest
                     | None -> None)
                | exception _ -> None)
           | _ -> walk here rest
           | exception _ -> if rest = [] then Some here else None) in
    walk "/" parts
  end

let realpath _argv opts operands =
  let quiet = Posix.Getopt.has opts "q" in
  let status = ref 0 in
  List.iter (fun name ->
      match resolve name with
      | Some p when Sys.file_exists p || Posix.Getopt.has opts "m" -> emit_line p
      | Some p when not (Posix.Getopt.has opts "e") -> emit_line p
      | _ ->
          if not quiet then warn "%s: %s" name (Unix.error_message Unix.ENOENT);
          status := 1)
    (match operands with [] -> die 1 "usage: realpath file..." | l -> l);
  flush_out ();
  !status

let pwd _argv opts operands =
  ignore operands;
  let physical = Posix.Getopt.has opts "P" in
  let logical =
    if physical then None
    else match Sys.getenv_opt "PWD" with
      | Some p when p <> "" && not (Filename.is_relative p) ->
          (match Unix.stat p, Unix.stat "." with
           | a, b when a.Unix.st_dev = b.Unix.st_dev && a.Unix.st_ino = b.Unix.st_ino -> Some p
           | _ -> None
           | exception _ -> None)
      | _ -> None in
  emit_line (match logical with Some p -> p | None -> Unix.getcwd ());
  flush_out ();
  0

(* ---------- the ones the shell also has ---------- *)

let echo argv _opts _operands =
  emit (Posix.Fmt.echo (List.tl (Array.to_list argv)));
  flush_out ();
  0

let printf argv _opts _operands =
  match List.tl (Array.to_list argv) with
  | [] -> die 2 "usage: printf format [argument...]"
  | format :: args ->
      let (text, errors) = Posix.Fmt.printf format args in
      emit text;
      flush_out ();
      List.iter (fun m -> warn "%s" m) errors;
      if errors = [] then 0 else 1

let test argv _opts _operands =
  let name = Filename.basename argv.(0) in
  let args = List.tl (Array.to_list argv) in
  let args =
    if name = "[" then
      match List.rev args with
      | "]" :: rest -> List.rev rest
      | _ -> warn "missing `]'"; raise (Fail 2)
    else args in
  match Posix.Ptest.evaluate args with
  | true -> 0
  | false -> 1
  | exception Posix.Ptest.Error msg -> warn "%s" msg; 2

let true_ _argv _opts _operands = 0
let false_ _argv _opts _operands = 1
