(* diff: compare two files line by line (IEEE Std 1003.1-2017, XCU diff),
   with the unified output GNU diff produces for -u, which is what the
   test suite's failure reports are read as.

   The comparison finds a longest common subsequence, since a difference
   report is only useful if it is the smallest one.  The algorithm is
   Myers' (1986): the edit graph is searched by increasing edit distance,
   keeping for each diagonal the furthest point reached, which finds a
   shortest edit script in O(ND) time rather than the O(NM) a full table
   would take.  The trace of those furthest points is kept so that the
   script itself, and not just its length, can be recovered. *)

open Util

type edit = Keep of int * int | Add of int | Remove of int

(* The shortest edit script between two arrays, as a list of edits in
   order.  [v] holds, per diagonal, how far along a the search has
   reached; a copy is kept per step so the path can be walked back. *)
let script (a : string array) (b : string array) =
  let n = Array.length a and m = Array.length b in
  let max_d = n + m in
  (* The furthest point reached on each diagonal k, which runs from -d
     to d, held in an array indexed by k + offset.  The room for one
     diagonal either side matters: the step for k looks at k-1 and k+1,
     so with two empty inputs -- d and k both zero -- a table of just
     the diagonals in play would be read past its end. *)
  let offset = max_d + 1 in
  let trace = ref [] in
  let v = Array.make (2 * max_d + 3) 0 in
  let finished = ref false in
  let steps = ref 0 in
  let d = ref 0 in
  while not !finished && !d <= max_d do
    trace := Array.copy v :: !trace;
    let k = ref (- !d) in
    while not !finished && !k <= !d do
      let down = !k = - !d || (!k <> !d && v.(!k - 1 + offset) < v.(!k + 1 + offset)) in
      let x = ref (if down then v.(!k + 1 + offset) else v.(!k - 1 + offset) + 1) in
      let y = ref (!x - !k) in
      (* follow the diagonal as far as the lines agree *)
      while !x < n && !y < m && a.(!x) = b.(!y) do incr x; incr y done;
      v.(!k + offset) <- !x;
      if !x >= n && !y >= m then (finished := true; steps := !d);
      k := !k + 2
    done;
    if not !finished then incr d
  done;
  let trace = Array.of_list (List.rev !trace) in
  (* walk back from the end, one edit distance at a time *)
  let edits = ref [] in
  let x = ref n and y = ref m in
  for step = !steps downto 1 do
    let v = trace.(step) in
    let k = !x - !y in
    let down = k = -step || (k <> step && v.(k - 1 + offset) < v.(k + 1 + offset)) in
    let prev_k = if down then k + 1 else k - 1 in
    let prev_x = v.(prev_k + offset) in
    let prev_y = prev_x - prev_k in
    while !x > prev_x && !y > prev_y do
      decr x; decr y;
      edits := Keep (!x, !y) :: !edits
    done;
    if down then (decr y; edits := Add !y :: !edits)
    else (decr x; edits := Remove !x :: !edits)
  done;
  while !x > 0 && !y > 0 do
    decr x; decr y;
    edits := Keep (!x, !y) :: !edits
  done;
  !edits

type hunk = { a_start : int; a_len : int; b_start : int; b_len : int; body : (char * string) list }

(* group the edits into hunks with [context] unchanged lines around each *)
let hunks ~context (a : string array) (b : string array) edits =
  let items = List.map (fun e ->
      match e with
      | Keep (i, j) -> (' ', a.(i), i, j)
      | Remove i -> ('-', a.(i), i, -1)
      | Add j -> ('+', b.(j), -1, j)) edits in
  let items = Array.of_list items in
  let n = Array.length items in
  let changed k = let (c, _, _, _) = items.(k) in c <> ' ' in
  let out = ref [] in
  let k = ref 0 in
  while !k < n do
    if changed !k then begin
      let first = max 0 (!k - context) in
      let last = ref !k in
      (* extend while another change is within twice the context *)
      let scanning = ref true in
      while !scanning do
        let j = ref (!last + 1) in
        let found = ref None in
        while !found = None && !j < n && !j <= !last + 2 * context + 1 do
          if changed !j then found := Some !j else incr j
        done;
        match !found with
        | Some j -> last := j
        | None -> scanning := false
      done;
      let stop = min (n - 1) (!last + context) in
      let body = ref [] in
      let a_start = ref (-1) and b_start = ref (-1) in
      let a_len = ref 0 and b_len = ref 0 in
      for idx = first to stop do
        let (c, text, i, j) = items.(idx) in
        body := (c, text) :: !body;
        if c <> '+' then begin
          if !a_start < 0 then a_start := i;
          incr a_len
        end;
        if c <> '-' then begin
          if !b_start < 0 then b_start := j;
          incr b_len
        end
      done;
      out := { a_start = (if !a_start < 0 then 0 else !a_start);
               a_len = !a_len;
               b_start = (if !b_start < 0 then 0 else !b_start);
               b_len = !b_len;
               body = List.rev !body } :: !out;
      k := stop + 1
    end else incr k
  done;
  List.rev !out

let read_lines file =
  let ic = open_input file in
  let out = ref [] in
  let rec go () =
    match read_line_raw ic with
    | None -> ()
    | Some raw -> out := fst (chop raw) :: !out; go () in
  go ();
  close_input ic;
  Array.of_list (List.rev !out)

let normalise ~ignore_space ~ignore_case s =
  let s = if ignore_case then String.lowercase_ascii s else s in
  if ignore_space then
    (* every run of blanks counts as none, which is what --ignore-all-space
       asks for *)
    String.concat "" (List.filter (fun p -> p <> "")
                        (String.split_on_char ' ' (String.map (fun c -> if c = '\t' then ' ' else c) s)))
  else s

let timestamp file =
  match Unix.stat file with
  | st ->
      let t = Unix.localtime st.Unix.st_mtime in
      (* the fraction comes from a double, which holds rather less than a
         nanosecond of resolution at present dates *)
      let fraction = st.Unix.st_mtime -. Float.of_int (int_of_float st.Unix.st_mtime) in
      Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d.%09d +0000"
        (t.Unix.tm_year + 1900) (t.Unix.tm_mon + 1) t.Unix.tm_mday
        t.Unix.tm_hour t.Unix.tm_min t.Unix.tm_sec
        (int_of_float (fraction *. 1e9))
  | exception _ -> ""

let main _argv opts operands =
  let brief = Posix.Getopt.has opts "q" || Posix.Getopt.has opts "brief"
              || Posix.Getopt.has opts "quiet" in
  let unified = Posix.Getopt.has opts "u" || Posix.Getopt.has opts "unified" in
  let ignore_space = Posix.Getopt.has opts "w" || Posix.Getopt.has opts "ignore-all-space" in
  let ignore_case = Posix.Getopt.has opts "i" in
  (* --color=WHEN (GNU): the added and removed lines in green and red,
     the command or hunk line in cyan, the file headers in bold, and
     nothing else touched.  `auto' means only when the output is a
     terminal, which in a build it is not. *)
  let colour =
    match Posix.Getopt.arg opts "color" with
    | Some ("always" | "") -> true
    | Some "auto" -> (try Unix.isatty Unix.stdout with _ -> false)
    | _ -> false in
  let paint code text = if colour then "\027[" ^ code ^ "m" ^ text ^ "\027[0m" else text in
  let removed_line text = paint "31" text
  and added_line text = paint "32" text
  and command_line text = paint "36" text
  and header_line text = paint "1" text in
  let context = match Posix.Getopt.arg opts "U" with
    | Some s -> (match int_of_string_opt s with Some v -> v | None -> 3)
    | None -> 3 in
  match operands with
  | [ left; right ] | [ left; right; _ ] ->
      (* a directory operand means the file of that name inside it *)
      let pick one other =
        match Unix.stat one with
        | { Unix.st_kind = Unix.S_DIR; _ } -> Filename.concat one (Filename.basename other)
        | _ -> one
        | exception _ -> one in
      let left = pick left right and right = pick right left in
      let a = match read_lines left with
        | l -> l
        | exception e -> warn "%s" (sys_message e); raise (Fail 2) in
      let b = match read_lines right with
        | l -> l
        | exception e -> warn "%s" (sys_message e); raise (Fail 2) in
      let key = Array.map (normalise ~ignore_space ~ignore_case) in
      let ka = key a and kb = key b in
      (* Two questions are answered by comparing the lines, and the
         reference diff answers them that way before it looks for an
         edit script: whether the files are the same at all, which they
         usually are where the OCaml test suite uses diff, and -q, which
         asks only whether they differ.  Looking for a shortest edit
         script over a large file to answer either would cost a hundred
         times as much. *)
      if ka = kb then 0
      else if brief then begin
        emit_line (Printf.sprintf "Files %s and %s differ" left right);
        flush_out ();
        1
      end else begin
        let edits = script ka kb in
        let groups = hunks ~context a b edits in
        if unified then begin
          emit_line (header_line (Printf.sprintf "--- %s\t%s" left (timestamp left)));
          emit_line (header_line (Printf.sprintf "+++ %s\t%s" right (timestamp right)));
          List.iter (fun h ->
              let count start len = if len = 0 then Printf.sprintf "%d,0" start
                else if len = 1 then string_of_int (start + 1)
                else Printf.sprintf "%d,%d" (start + 1) len in
              emit_line (command_line (Printf.sprintf "@@ -%s +%s @@"
                           (count h.a_start h.a_len) (count h.b_start h.b_len)));
              List.iter (fun (c, text) ->
                  let line = String.make 1 c ^ text in
                  emit_line (match c with
                      | '-' -> removed_line line
                      | '+' -> added_line line
                      | _ -> line)) h.body)
            groups
        end else begin
          (* the default output: a command and the lines it applies to *)
          List.iter (fun h ->
              let removed = List.filter (fun (c, _) -> c = '-') h.body in
              let added = List.filter (fun (c, _) -> c = '+') h.body in
              let range start len =
                if len = 0 then string_of_int start
                else if len = 1 then string_of_int (start + 1)
                else Printf.sprintf "%d,%d" (start + 1) (start + len) in
              let a_from = ref h.a_start and b_from = ref h.b_start in
              (* the ranges of the changed lines only *)
              let rec first_change ac bc = function
                | (' ', _) :: rest -> first_change (ac + 1) (bc + 1) rest
                | _ -> (ac, bc) in
              let (skip_a, skip_b) = first_change 0 0 h.body in
              a_from := h.a_start + skip_a;
              b_from := h.b_start + skip_b;
              let nr = List.length removed and na = List.length added in
              let command =
                if nr > 0 && na > 0 then
                  Printf.sprintf "%sc%s" (range !a_from nr) (range !b_from na)
                else if nr > 0 then
                  Printf.sprintf "%sd%d" (range !a_from nr) !b_from
                else
                  Printf.sprintf "%da%s" !a_from (range !b_from na) in
              emit_line (command_line command);
              List.iter (fun (_, text) -> emit_line (removed_line ("< " ^ text))) removed;
              if nr > 0 && na > 0 then emit_line "---";
              List.iter (fun (_, text) -> emit_line (added_line ("> " ^ text))) added)
            groups
        end;
        flush_out ();
        1
      end
  | _ -> die 2 "usage: diff [-u] [-q] file1 file2"
