(* xargs: build command lines from the standard input (IEEE Std
   1003.1-2017, XCU xargs).

   The input is a list of arguments separated by blanks or newlines, in
   which a quote groups several words and a backslash quotes one
   character; -0 replaces all that with a plain NUL separator, which is
   the only form that can carry every possible file name.  The utility is
   run once per batch, with as many arguments as fit. *)

open Util

let read_items ~null ic =
  let items = ref [] and b = Buffer.create 64 in
  let started = ref false in
  let flush () = if !started then (items := Buffer.contents b :: !items; Buffer.clear b; started := false) in
  let rec go () =
    match input_char ic with
    | exception End_of_file -> flush ()
    | c ->
        if null then begin
          if c = '\000' then flush () else (Buffer.add_char b c; started := true)
        end else begin
          match c with
          | ' ' | '\t' | '\n' -> flush ()
          | '\\' ->
              (match input_char ic with
               | c -> Buffer.add_char b c; started := true
               | exception End_of_file -> ())
          | '\'' | '"' ->
              let quote = c in
              started := true;
              let rec inside () =
                match input_char ic with
                | c when c = quote -> ()
                | c -> Buffer.add_char b c; inside ()
                | exception End_of_file -> warn "unmatched %c" quote in
              inside ()
          | c -> Buffer.add_char b c; started := true
        end;
        go () in
  go ();
  List.rev !items

let run argv =
  match argv with
  | [] -> 0
  | name :: _ ->
      flush_out ();
      (match Unix.fork () with
       | 0 ->
           (try Unix.execvp name (Array.of_list argv)
            with _ -> Printf.eprintf "xargs: %s: not found\n" name; exit 127)
       | pid ->
           (match Unix.waitpid [] pid with
            | (_, Unix.WEXITED c) -> c
            | (_, Unix.WSIGNALED s) -> 128 + s
            | _ -> 1))

let main _argv opts operands =
  let null = Posix.Getopt.has opts "0" in
  let no_run_if_empty = Posix.Getopt.has opts "r" in
  let verbose = Posix.Getopt.has opts "t" in
  let per_batch = match Posix.Getopt.arg opts "n" with
    | Some s -> (match int_of_string_opt s with Some v -> v | None -> die 1 "%s: invalid number" s)
    | None -> max_int in
  let replace = Posix.Getopt.arg opts "I" in
  let command = match operands with [] -> [ "echo" ] | l -> l in
  let items = read_items ~null stdin in
  if items = [] && (no_run_if_empty || replace <> None) then 0
  else begin
    let status = ref 0 in
    let announce argv = if verbose then (prerr_string (String.concat " " argv); prerr_newline ()) in
    match replace with
    | Some marker ->
        (* -I runs the utility once per item, with the marker replaced *)
        List.iter (fun item ->
            let argv = List.map (fun w ->
                if w = marker then item
                else begin
                  let b = Buffer.create (String.length w) in
                  let m = String.length marker in
                  let i = ref 0 in
                  while !i < String.length w do
                    if m > 0 && !i + m <= String.length w && String.sub w !i m = marker
                    then (Buffer.add_string b item; i := !i + m)
                    else (Buffer.add_char b w.[!i]; incr i)
                  done;
                  Buffer.contents b
                end) command in
            announce argv;
            let c = run argv in
            if c <> 0 then status := c) items;
        !status
    | None ->
        let rec batches remaining =
          match remaining with
          | [] -> ()
          | _ ->
              let rec take k acc = function
                | rest when k = 0 -> (List.rev acc, rest)
                | [] -> (List.rev acc, [])
                | x :: rest -> take (k - 1) (x :: acc) rest in
              (* a batch is bounded by -n and by the length a command line
                 can hold *)
              let limit = min per_batch 5000 in
              let (chunk, rest) = take limit [] remaining in
              let argv = command @ chunk in
              announce argv;
              let c = run argv in
              if c <> 0 then status := c;
              batches rest in
        if items = [] then (let argv = command in announce argv; run argv)
        else (batches items; !status)
  end
