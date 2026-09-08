(* The utilities that read text and write text: cat, tee, head, tail, wc,
   cut, tr, sort, uniq and cmp (IEEE Std 1003.1-2017, XCU).

   They share two habits worth stating once.  A file operand of '-' is
   the standard input (XCU 1.4), and with more than one operand the ones
   that report per file print a header naming it; both are what the
   scripts depend on when they write `cat a b > c' or `wc -l < f'.
   Comparison is by byte value: the reference runs are made under the C
   locale, where the collating order is the numeric order of the bytes,
   so nothing here consults a locale. *)

open Util

(* ---------- cat ---------- *)

let cat _argv opts operands =
  ignore opts;
  let status = ref 0 in
  List.iter (fun file ->
      match open_input file with
      | exception e -> warn "%s" (sys_message e); status := 1
      | ic ->
          let chunk = Bytes.create 65536 in
          let rec copy () =
            let k = input ic chunk 0 65536 in
            if k > 0 then (emit (Bytes.sub_string chunk 0 k); copy ()) in
          copy ();
          close_input ic)
    (inputs operands);
  flush_out ();
  !status

(* ---------- tee ---------- *)

let tee _argv opts operands =
  let append = Posix.Getopt.has opts "a" in
  let channels = List.filter_map (fun name ->
      match (if append then open_out_gen [ Open_wronly; Open_creat; Open_append; Open_binary ] 0o666 name
             else open_out_bin name) with
      | oc -> Some oc
      | exception e -> warn "%s" (sys_message e); None) operands in
  let expected = List.length operands in
  let chunk = Bytes.create 65536 in
  let rec copy () =
    let k = input stdin chunk 0 65536 in
    if k > 0 then begin
      print_string (Bytes.sub_string chunk 0 k);
      List.iter (fun oc -> output_bytes oc (Bytes.sub chunk 0 k)) channels;
      copy ()
    end in
  copy ();
  flush stdout;
  List.iter close_out channels;
  if List.length channels = expected then 0 else 2

(* ---------- head and tail ---------- *)

(* `-n 10', `-n +10' and the obsolete `-10' all name a count; the sign is
   kept because tail reads it as "from line 10 on". *)
let count_arg opts letter default =
  match Posix.Getopt.arg opts letter with
  | Some s ->
      let plus = s <> "" && s.[0] = '+' in
      let body = if plus || (s <> "" && s.[0] = '-') then String.sub s 1 (String.length s - 1) else s in
      (match int_of_string_opt body with
       | Some v -> (v, plus)
       | None -> die 1 "%s: invalid number of lines" s)
  | None -> (default, false)

let with_header multiple first file f =
  if multiple then begin
    if not first then emit "\n";
    emit ("==> " ^ name_of file ^ " <==\n")
  end;
  f ()

let head argv opts operands =
  (* the obsolete form, which the build's scripts do not use but which
     costs one line to accept *)
  let operands, opts =
    match Array.to_list argv with
    | _ :: a :: rest when String.length a > 1 && a.[0] = '-'
                          && String.for_all Posix.Regex.is_digit (String.sub a 1 (String.length a - 1)) ->
        (rest, ("n", Some (String.sub a 1 (String.length a - 1))) :: opts)
    | _ -> (operands, opts) in
  let (lines, _) = count_arg opts "n" 10 in
  let bytes = match Posix.Getopt.arg opts "c" with
    | Some s -> (match int_of_string_opt s with Some v -> Some v | None -> die 1 "%s: invalid number" s)
    | None -> None in
  let files = inputs operands in
  let multiple = List.length files > 1 && not (Posix.Getopt.has opts "q") in
  let status = ref 0 in
  let first = ref true in
  List.iter (fun file ->
      match open_input file with
      | exception e -> warn "%s" (sys_message e); status := 1
      | ic ->
          with_header multiple !first file (fun () ->
              match bytes with
              | Some n ->
                  let buf = Bytes.create n in
                  let k = input ic buf 0 n in
                  emit (Bytes.sub_string buf 0 k)
              | None ->
                  let left = ref lines in
                  let rec go () =
                    if !left > 0 then
                      match read_line_raw ic with
                      | None -> ()
                      | Some raw -> emit raw; decr left; go () in
                  go ());
          first := false;
          close_input ic)
    files;
  flush_out ();
  !status

let tail argv opts operands =
  let operands, opts =
    match Array.to_list argv with
    | _ :: a :: rest when String.length a > 1 && a.[0] = '-'
                          && String.for_all Posix.Regex.is_digit (String.sub a 1 (String.length a - 1)) ->
        (rest, ("n", Some (String.sub a 1 (String.length a - 1))) :: opts)
    | _ -> (operands, opts) in
  let (lines, from_start) = count_arg opts "n" 10 in
  let files = inputs operands in
  let multiple = List.length files > 1 && not (Posix.Getopt.has opts "q") in
  let status = ref 0 in
  let first = ref true in
  List.iter (fun file ->
      match open_input file with
      | exception e -> warn "%s" (sys_message e); status := 1
      | ic ->
          with_header multiple !first file (fun () ->
              if from_start then begin
                (* `-n +k' starts at line k *)
                let seen = ref 0 in
                let rec go () =
                  match read_line_raw ic with
                  | None -> ()
                  | Some raw -> incr seen; if !seen >= lines then emit raw; go () in
                go ()
              end else begin
                (* the last k lines, kept in a ring so that the input is
                   read once and only k lines are held *)
                let ring = Array.make (max lines 1) "" in
                let count = ref 0 in
                let rec go () =
                  match read_line_raw ic with
                  | None -> ()
                  | Some raw -> ring.(!count mod Array.length ring) <- raw; incr count; go () in
                go ();
                let total = min !count lines in
                for k = 0 to total - 1 do
                  emit ring.((!count - total + k) mod Array.length ring)
                done
              end);
          first := false;
          close_input ic)
    files;
  flush_out ();
  !status

(* ---------- wc ---------- *)

let wc _argv opts operands =
  let want_l = Posix.Getopt.has opts "l" and want_w = Posix.Getopt.has opts "w"
  and want_c = Posix.Getopt.has opts "c" and want_m = Posix.Getopt.has opts "m" in
  let none = not (want_l || want_w || want_c || want_m) in
  let files = inputs operands in
  let status = ref 0 in
  let tot_l = ref 0 and tot_w = ref 0 and tot_c = ref 0 in
  (* The counts are right-aligned in a field wide enough for the largest
     that could appear, which the total size of the inputs bounds.  When
     an input is not a file -- a pipe, say -- there is no size to go on
     and a fixed width is used instead. *)
  let fields_printed =
    (if none then 3 else 0)
    + (if want_l then 1 else 0) + (if want_w then 1 else 0)
    + (if want_c || want_m then 1 else 0) in
  let width =
    if List.length files = 1 && fields_printed = 1 then 1
    else begin
      let known = ref true and total = ref 0 in
      List.iter (fun f ->
          match (if f = "-" then Unix.fstat Unix.stdin else Unix.stat f) with
          | { Unix.st_kind = Unix.S_REG; st_size; _ } -> total := !total + st_size
          | _ -> known := false
          | exception _ -> ()) files;
      if !known then String.length (string_of_int !total) else 7
    end in
  let report l w c name =
    let b = Buffer.create 48 in
    let field first v =
      if not first then Buffer.add_char b ' ';
      Buffer.add_string b (Printf.sprintf "%*d" width v) in
    let first = ref true in
    if none || want_l then (field !first l; first := false);
    if none || want_w then (field !first w; first := false);
    if none || want_c || want_m then (field !first c; first := false);
    if name <> "" then Buffer.add_string b (" " ^ name);
    emit_line (Buffer.contents b) in
  List.iter (fun file ->
      match open_input file with
      | exception e -> warn "%s" (sys_message e); status := 1
      | ic ->
          let l = ref 0 and w = ref 0 and c = ref 0 and inword = ref false in
          let chunk = Bytes.create 65536 in
          let rec go () =
            let k = input ic chunk 0 65536 in
            if k > 0 then begin
              for i = 0 to k - 1 do
                let ch = Bytes.get chunk i in
                incr c;
                if ch = '\n' then incr l;
                if Posix.Regex.is_space ch then inword := false
                else if not !inword then (inword := true; incr w)
              done;
              go ()
            end in
          go ();
          close_input ic;
          tot_l := !tot_l + !l; tot_w := !tot_w + !w; tot_c := !tot_c + !c;
          report !l !w !c (if file = "-" then "" else file))
    files;
  if List.length files > 1 then report !tot_l !tot_w !tot_c "total";
  flush_out ();
  !status

(* ---------- cut ---------- *)

(* A list is "1,3-5,7-": single positions and ranges, counting from one,
   with an open end. *)
let parse_list s =
  List.filter_map (fun piece ->
      if piece = "" then None
      else match String.index_opt piece '-' with
        | None ->
            (match int_of_string_opt piece with
             | Some v -> Some (v, v)
             | None -> die 1 "%s: invalid list" s)
        | Some k ->
            let lo = String.sub piece 0 k in
            let hi = String.sub piece (k + 1) (String.length piece - k - 1) in
            let lo = if lo = "" then 1 else (match int_of_string_opt lo with
                | Some v -> v | None -> die 1 "%s: invalid list" s) in
            let hi = if hi = "" then max_int else (match int_of_string_opt hi with
                | Some v -> v | None -> die 1 "%s: invalid list" s) in
            Some (lo, hi))
    (String.split_on_char ',' s)

let in_list ranges k = List.exists (fun (lo, hi) -> k >= lo && k <= hi) ranges

let cut _argv opts operands =
  let by_char = match Posix.Getopt.arg opts "c" with
    | Some l -> Some (parse_list l)
    | None -> (match Posix.Getopt.arg opts "b" with Some l -> Some (parse_list l) | None -> None) in
  let by_field = match Posix.Getopt.arg opts "f" with Some l -> Some (parse_list l) | None -> None in
  let delim = match Posix.Getopt.arg opts "d" with
    | Some d when d <> "" -> d.[0]
    | _ -> '\t' in
  let only_delimited = Posix.Getopt.has opts "s" in
  if by_char = None && by_field = None then die 1 "one of -b, -c or -f is required";
  let status = ref 0 in
  List.iter (fun file ->
      match open_input file with
      | exception e -> warn "%s" (sys_message e); status := 1
      | ic ->
          each_line (fun _ line _ ->
              match by_char, by_field with
              | Some ranges, _ ->
                  let b = Buffer.create (String.length line) in
                  String.iteri (fun i c -> if in_list ranges (i + 1) then Buffer.add_char b c) line;
                  emit_line (Buffer.contents b)
              | None, Some ranges ->
                  if not (String.contains line delim) then
                    (if not only_delimited then emit_line line)
                  else begin
                    let parts = String.split_on_char delim line in
                    let kept = List.filteri (fun i _ -> in_list ranges (i + 1)) parts in
                    emit_line (String.concat (String.make 1 delim) kept)
                  end
              | None, None -> ()) ic;
          close_input ic)
    (inputs operands);
  flush_out ();
  !status

(* ---------- tr ---------- *)

(* A set is a string in which a-z is a range, [:class:] a character class
   and [c*n] a repeat; the escapes are those of XCU tr. *)
let tr_set s =
  let out = ref [] in
  let n = String.length s in
  let i = ref 0 in
  let literal () =
    if s.[!i] = '\\' && !i + 1 < n then begin
      let c = match s.[!i + 1] with
        | 'a' -> '\007' | 'b' -> '\b' | 'f' -> '\012' | 'n' -> '\n'
        | 'r' -> '\r' | 't' -> '\t' | 'v' -> '\011' | '\\' -> '\\'
        | d when d >= '0' && d <= '7' ->
            let k = ref (!i + 1) and v = ref 0 and digits = ref 0 in
            while !digits < 3 && !k < n && s.[!k] >= '0' && s.[!k] <= '7' do
              v := !v * 8 + (Char.code s.[!k] - 48); incr k; incr digits
            done;
            i := !k - 2;
            Char.chr (!v land 255)
        | d -> d in
      i := !i + 2;
      c
    end else (let c = s.[!i] in incr i; c) in
  while !i < n do
    if s.[!i] = '[' && !i + 1 < n && s.[!i + 1] = ':' then begin
      match String.index_from_opt s (!i + 2) ':' with
      | Some j when j + 1 < n && s.[j + 1] = ']' ->
          let pred = Posix.Regex.class_pred (String.sub s (!i + 2) (j - !i - 2)) in
          for k = 0 to 255 do if pred (Char.chr k) then out := Char.chr k :: !out done;
          i := j + 2
      | _ -> out := s.[!i] :: !out; incr i
    end
    else begin
      let c = literal () in
      if !i < n && s.[!i] = '-' && !i + 1 < n && s.[!i + 1] <> ']' then begin
        incr i;
        let hi = literal () in
        for k = Char.code c to Char.code hi do out := Char.chr k :: !out done
      end else out := c :: !out
    end
  done;
  List.rev !out

let tr _argv opts operands =
  let delete = Posix.Getopt.has opts "d" and squeeze = Posix.Getopt.has opts "s" in
  let complement = Posix.Getopt.has opts "c" || Posix.Getopt.has opts "C" in
  let set1, set2 = match operands with
    | [ a ] -> (tr_set a, [])
    | [ a; b ] -> (tr_set a, tr_set b)
    | _ -> die 1 "usage: tr [-cdst] set1 [set2]" in
  let member = Array.make 256 false in
  List.iter (fun c -> member.(Char.code c) <- true) set1;
  let member = if complement then Array.map not member else member in
  (* the last character of set2 is repeated to fill it out (XCU tr) *)
  let mapped = Array.init 256 (fun k -> Char.chr k) in
  if not delete && set2 <> [] then begin
    let arr1 = Array.of_list (List.filteri (fun i _ -> ignore i; true)
                                (if complement then
                                   List.filter (fun c -> member.(Char.code c)) (List.init 256 Char.chr)
                                 else set1)) in
    let arr2 = Array.of_list set2 in
    Array.iteri (fun i c ->
        let target = if i < Array.length arr2 then arr2.(i) else arr2.(Array.length arr2 - 1) in
        mapped.(Char.code c) <- target) arr1
  end;
  let squeeze_set =
    if squeeze then
      (if delete || set2 = [] then member
       else begin
         let m = Array.make 256 false in
         List.iter (fun c -> m.(Char.code c) <- true) set2;
         m
       end)
    else Array.make 256 false in
  let last = ref (-1) in
  let chunk = Bytes.create 65536 in
  let b = Buffer.create 65536 in
  let rec go () =
    let k = input stdin chunk 0 65536 in
    if k > 0 then begin
      for i = 0 to k - 1 do
        let c = Bytes.get chunk i in
        let code = Char.code c in
        if delete && member.(code) then ()
        else begin
          let c = if delete then c else mapped.(code) in
          if squeeze && squeeze_set.(Char.code c) && !last = Char.code c then ()
          else (Buffer.add_char b c; last := Char.code c);
          if not (squeeze_set.(Char.code c)) then last := -1
        end
      done;
      emit (Buffer.contents b);
      Buffer.clear b;
      go ()
    end in
  go ();
  flush_out ();
  0

(* ---------- sort ---------- *)

(* A sort key is `-k f[,t]', which names fields counted from one; without
   -t a field ends at a run of blanks, and the blanks belong to the field
   that follows. *)
type key = { kfrom : int; kto : int }

let field_of line delim k =
  match delim with
  | Some d ->
      let parts = String.split_on_char d line in
      (match List.nth_opt parts (k - 1) with Some p -> p | None -> "")
  | None ->
      let n = String.length line in
      let i = ref 0 and field = ref 0 and result = ref "" in
      while !field < k && !i < n do
        while !i < n && Posix.Regex.is_space line.[!i] do incr i done;
        let start = !i in
        while !i < n && not (Posix.Regex.is_space line.[!i]) do incr i done;
        incr field;
        if !field = k then result := String.sub line start (!i - start)
      done;
      !result

let numeric_value s =
  let s = String.trim s in
  let n = String.length s in
  let i = ref 0 in
  if !i < n && (s.[!i] = '-' || s.[!i] = '+') then incr i;
  while !i < n && (Posix.Regex.is_digit s.[!i] || s.[!i] = '.') do incr i done;
  match float_of_string_opt (String.sub s 0 !i) with
  | Some v -> v
  | None -> 0.0

let sort _argv opts operands =
  let numeric = Posix.Getopt.has opts "n" and reverse = Posix.Getopt.has opts "r" in
  let unique = Posix.Getopt.has opts "u" and fold = Posix.Getopt.has opts "f" in
  let check = Posix.Getopt.has opts "c" and blanks = Posix.Getopt.has opts "b" in
  let delim = match Posix.Getopt.arg opts "t" with Some d when d <> "" -> Some d.[0] | _ -> None in
  let keys = List.map (fun spec ->
      let spec = List.hd (String.split_on_char '.' spec) in
      match String.index_opt spec ',' with
      | Some k ->
          let f = String.sub spec 0 k and t = String.sub spec (k + 1) (String.length spec - k - 1) in
          let t = List.hd (String.split_on_char '.' t) in
          { kfrom = (match int_of_string_opt f with Some v -> v | None -> 1);
            kto = (match int_of_string_opt t with Some v -> v | None -> max_int) }
      | None ->
          { kfrom = (match int_of_string_opt spec with Some v -> v | None -> 1); kto = max_int })
      (Posix.Getopt.all opts "k") in
  let lines = List.concat_map (fun file ->
      match lines_of_file file with
      | l -> l
      | exception e -> warn "%s" (sys_message e); raise (Fail 2)) (inputs operands) in
  let key_text line =
    if keys = [] then (if blanks then String.trim line else line)
    else String.concat " " (List.map (fun k ->
        if k.kto = max_int && k.kfrom = 1 && delim = None then line
        else begin
          let parts = ref [] in
          let last = if k.kto = max_int then k.kfrom else k.kto in
          for f = k.kfrom to last do parts := field_of line delim f :: !parts done;
          String.concat " " (List.rev !parts)
        end) keys) in
  let compare_lines a b =
    let ka = key_text a and kb = key_text b in
    let ka, kb = if fold then String.lowercase_ascii ka, String.lowercase_ascii kb else ka, kb in
    let c =
      if numeric then compare (numeric_value ka) (numeric_value kb)
      else compare ka kb in
    let c = if c <> 0 then c else compare a b in
    if reverse then -c else c in
  if check then begin
    let rec ordered = function
      | a :: (b :: _ as rest) -> if compare_lines a b > 0 then false else ordered rest
      | _ -> true in
    if ordered lines then 0 else (warn "disorder"; 1)
  end else begin
    let sorted = List.stable_sort compare_lines lines in
    let rec keep previous acc = function
      | [] -> List.rev acc
      | l :: rest ->
          let same = match previous with
            | Some p -> compare_lines p l = 0
            | None -> false in
          keep (Some l) (if unique && same then acc else l :: acc) rest in
    let result = keep None [] sorted in
    (* -o names a file to write, which may be one of the inputs, so the
       whole result is in hand before it is opened *)
    (match Posix.Getopt.arg opts "o" with
     | Some name ->
         let oc = open_out_bin name in
         List.iter (fun l -> output_string oc l; output_char oc '\n') result;
         close_out oc
     | None -> List.iter emit_line result; flush_out ());
    0
  end

(* ---------- uniq ---------- *)

let uniq _argv opts operands =
  let show_count = Posix.Getopt.has opts "c" in
  let only_repeated = Posix.Getopt.has opts "d" and only_unique = Posix.Getopt.has opts "u" in
  let fold = Posix.Getopt.has opts "i" in
  let skip_fields = match Posix.Getopt.arg opts "f" with
    | Some s -> (match int_of_string_opt s with Some v -> v | None -> 0)
    | None -> 0 in
  let skip_chars = match Posix.Getopt.arg opts "s" with
    | Some s -> (match int_of_string_opt s with Some v -> v | None -> 0)
    | None -> 0 in
  let input_file, output_file = match operands with
    | [] -> ("-", None)
    | [ a ] -> (a, None)
    | a :: b :: _ -> (a, Some b) in
  let significant line =
    let n = String.length line in
    let i = ref 0 in
    for _ = 1 to skip_fields do
      while !i < n && Posix.Regex.is_space line.[!i] do incr i done;
      while !i < n && not (Posix.Regex.is_space line.[!i]) do incr i done
    done;
    let i = min n (!i + skip_chars) in
    let s = String.sub line i (n - i) in
    if fold then String.lowercase_ascii s else s in
  let lines = lines_of_file input_file in
  let groups = ref [] in
  List.iter (fun l ->
      match !groups with
      | (first, count) :: rest when significant first = significant l ->
          groups := (first, count + 1) :: rest
      | _ -> groups := (l, 1) :: !groups) lines;
  let groups = List.rev !groups in
  let show (line, count) =
    let wanted =
      if only_repeated then count > 1
      else if only_unique then count = 1
      else true in
    if wanted then
      if show_count then emit_line (Printf.sprintf "%7d %s" count line)
      else emit_line line in
  (match output_file with
   | None -> List.iter show groups; flush_out ()
   | Some name ->
       let oc = open_out_bin name in
       List.iter (fun g ->
           let wanted =
             if only_repeated then snd g > 1
             else if only_unique then snd g = 1
             else true in
           if wanted then begin
             if show_count then output_string oc (Printf.sprintf "%7d %s\n" (snd g) (fst g))
             else output_string oc (fst g ^ "\n")
           end) groups;
       close_out oc);
  0

(* ---------- cmp ---------- *)

let cmp _argv opts operands =
  let silent = Posix.Getopt.has opts "s" in
  let show_all = Posix.Getopt.has opts "l" in
  match operands with
  | [ a; b ] | [ a; b; _ ] ->
      let ic1 = open_input a and ic2 = open_input b in
      let byte = ref 0 and line = ref 1 in
      let status = ref 0 in
      let finished = ref false in
      while not !finished do
        let c1 = (try Some (input_char ic1) with End_of_file -> None) in
        let c2 = (try Some (input_char ic2) with End_of_file -> None) in
        match c1, c2 with
        | None, None -> finished := true
        | None, Some _ ->
            if not silent then warn "EOF on %s" a;
            status := 1; finished := true
        | Some _, None ->
            if not silent then warn "EOF on %s" b;
            status := 1; finished := true
        | Some x, Some y ->
            incr byte;
            if x <> y then begin
              status := 1;
              if show_all then
                emit_line (Printf.sprintf "%d %o %o" !byte (Char.code x) (Char.code y))
              else begin
                if not silent then
                  emit_line (Printf.sprintf "%s %s differ: char %d, line %d" a b !byte !line);
                finished := true
              end
            end;
            if x = '\n' then incr line
      done;
      close_input ic1; close_input ic2;
      flush_out ();
      !status
  | _ -> die 2 "usage: cmp [-l] [-s] file1 file2"
