(* Machinery the utilities share.

   Each utility is a function from its arguments to its exit status, so
   that they can all live in one binary (bin/occutils.ml) reached through
   links named after them, the way the scripts call them.  Diagnostics go
   to standard error prefixed with the name the utility was called by, and
   a utility gives up by raising [Fail], which the binary turns into that
   exit status: an OCaml exception unwinds the open files on the way out,
   where a bare exit would not.

   Exit statuses follow XCU 1.4 unless a utility's own description differs:
   0 for success, 1 for "did its job but found nothing", above 1 for an
   error. *)

exception Fail of int

let prog = ref "occutils"

let warn fmt =
  Printf.ksprintf (fun s -> Printf.eprintf "%s: %s\n" !prog s) fmt

let die status fmt =
  Printf.ksprintf (fun s -> Printf.eprintf "%s: %s\n" !prog s; raise (Fail status)) fmt

(* the message of a failed system call, which reads the same as the one the
   other utilities print because it comes from the same strerror text *)
let sys_message = function
  | Sys_error msg -> msg
  | Unix.Unix_error (e, _, arg) ->
      if arg = "" then Unix.error_message e else arg ^ ": " ^ Unix.error_message e
  | e -> Printexc.to_string e

(* ---------- input ---------- *)

(* A '-' operand names standard input (XCU 1.4). *)
let open_input name =
  if name = "-" then stdin else open_in_bin name

let close_input ic = if ic != stdin then close_in ic

(* Read one line, keeping its newline; the last line of a file that does
   not end in one is still a line. *)
let read_line_raw ic =
  let b = Buffer.create 128 in
  let rec go () =
    match input_char ic with
    | '\n' -> Buffer.add_char b '\n'; Some (Buffer.contents b)
    | c -> Buffer.add_char b c; go ()
    | exception End_of_file -> if Buffer.length b = 0 then None else Some (Buffer.contents b) in
  go ()

(* the line without its newline, and whether one was there *)
let chop s =
  let n = String.length s in
  if n > 0 && s.[n - 1] = '\n' then (String.sub s 0 (n - 1), true) else (s, false)

let each_line f ic =
  let rec go n =
    match read_line_raw ic with
    | None -> ()
    | Some raw -> let (line, nl) = chop raw in f n line nl; go (n + 1) in
  go 1

let read_all ic =
  let b = Buffer.create 65536 in
  let chunk = Bytes.create 65536 in
  let rec go () =
    let k = input ic chunk 0 65536 in
    if k > 0 then (Buffer.add_subbytes b chunk 0 k; go ()) in
  go ();
  Buffer.contents b

let lines_of_file name =
  let ic = open_input name in
  let out = ref [] in
  let rec go () =
    match read_line_raw ic with
    | None -> ()
    | Some raw -> out := fst (chop raw) :: !out; go () in
  go ();
  close_input ic;
  List.rev !out

(* ---------- output ---------- *)

let out = Buffer.create 65536

let flush_out () =
  if Buffer.length out > 0 then (print_string (Buffer.contents out); Buffer.clear out);
  flush stdout

let emit s =
  Buffer.add_string out s;
  if Buffer.length out > 32768 then flush_out ()

let emit_line s = emit s; emit "\n"

(* ---------- operands ---------- *)

(* the operands, with standard input standing in for an empty list *)
let inputs = function [] -> [ "-" ] | files -> files

let name_of file = if file = "-" then "(standard input)" else file
