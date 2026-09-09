(* grep: select the lines of the input that match a pattern (XCU grep).

   The patterns come from -e, from the lines of a -f file, or from the
   first operand, and a line is selected when any of them matches (or,
   with -v, when none does).  A pattern is a basic regular expression, an
   extended one with -E, or a plain string with -F.

   The exit status is 0 if any line was selected, 1 if none was, and 2 if
   a file could not be read, which is what makes `grep -q pattern file ||
   fallback' work in the scripts. *)

open Util

type kind = Bre | Ere | Fixed

type matcher = string -> int -> (int * int) option

let fixed_matcher ~icase pat : matcher =
  let fold s = if icase then String.lowercase_ascii s else s in
  let pat = fold pat in
  let m = String.length pat in
  fun s from ->
    let s = fold s in
    let n = String.length s in
    let rec go i =
      if i + m > n then None
      else if String.sub s i m = pat then Some (i, i + m)
      else go (i + 1) in
    go from

let regex_matcher ~kind ~icase pat : matcher =
  let re = Posix.Regex.compile ~ere:(kind = Ere) ~icase pat in
  fun s from ->
    match Posix.Regex.search re s from with
    | Some groups -> Some groups.(0)
    | None -> None

let is_word c = Posix.Regex.is_word c

(* Does the pattern match somewhere in the line, honouring -x (the match
   must be the whole line) and -w (it must be bounded by non-word
   characters)?  Both may need later matches to be tried, so the search
   walks forward until one fits. *)
let find ?(from = 0) ~whole ~word (m : matcher) line =
  let n = String.length line in
  let rec go from =
    if from > n then None
    else
      match m line from with
      | None -> None
      | Some (a, b) ->
          if whole then (if a = 0 && b = n then Some (a, b) else None)
          else if word
                  && not ((a = 0 || not (is_word line.[a - 1]))
                          && (b = n || not (is_word line.[b])))
          then go (if b > a then a + 1 else from + 1)
          else Some (a, b) in
  go from

let main_opts _argv opts operands =
  let has = Posix.Getopt.has opts in
  let kind = if has "F" then Fixed else if has "E" then Ere else Bre in
  let icase = has "i" and invert = has "v" in
  let quiet = has "q" || has "quiet" || has "silent" in
  let count_only = has "c" and list_only = has "l" and number = has "n" in
  let only = has "o" and whole = has "x" and word = has "w" in
  let silent = has "s" in
  (* --line-buffered passes a line on as soon as it matches, rather than
     when a block of output has gathered.  The reference grep waits for
     the block unless asked, whatever it is reading, and this follows
     it: a script that wants to watch a pipeline says so. *)
  if has "line-buffered" then streaming := true;
  (* patterns from -e and -f, else the first operand *)
  let from_e = Posix.Getopt.all opts "e" in
  let from_f = List.concat_map lines_of_file (Posix.Getopt.all opts "f") in
  let patterns, files =
    if from_e = [] && Posix.Getopt.all opts "f" = [] then
      match operands with
      | [] -> die 2 "no pattern given"
      | pat :: rest -> (String.split_on_char '\n' pat, rest)
    else (from_e @ from_f, operands) in
  let matchers =
    List.map (fun p ->
        match kind with
        | Fixed -> fixed_matcher ~icase p
        | k -> (try regex_matcher ~kind:k ~icase p with
            | Posix.Regex.Error msg -> die 2 "%s" msg)) patterns in
  let files = inputs files in
  let show_name = has "H" || (List.length files > 1 && not (has "h")) in
  let any = ref false and errors = ref false in
  let scan file =
    match open_input file with
    | exception e -> if not silent then warn "%s" (sys_message e); errors := true
    | ic ->
        let count = ref 0 in
        let stop = ref false in
        let prefix n =
          (if show_name then emit (name_of file ^ ":"));
          if number then emit (string_of_int n ^ ":") in
        each_line (fun n line _nl ->
            if not !stop then begin
              let hit = List.exists (fun m -> find ~whole ~word m line <> None) matchers in
              if hit <> invert then begin
                any := true;
                incr count;
                if quiet then stop := true
                else if list_only then (emit_line (name_of file); stop := true)
                else if count_only then ()
                else if only && not invert then
                  (* Every match on the line, each on its own line.  The
                     search continues in the line itself rather than in a
                     copy of the tail, or a '^' would match again; an empty
                     match prints nothing, as it covers no text. *)
                  List.iter (fun m ->
                      let rec go from =
                        if from <= String.length line then
                          match find ~from ~whole ~word m line with
                          | None -> ()
                          | Some (a, b) ->
                              if b > a then begin
                                prefix n;
                                emit_line (String.sub line a (b - a));
                                go b
                              end else go (a + 1) in
                      go 0) matchers
                else (prefix n; emit_line line)
              end
            end) ic;
        close_input ic;
        if count_only && not quiet && not list_only then begin
          if show_name then emit (name_of file ^ ":");
          emit_line (string_of_int !count)
        end in
  List.iter scan files;
  flush_out ();
  if !errors then 2 else if !any then 0 else 1
