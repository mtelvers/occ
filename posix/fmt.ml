(* The output formatting of echo and printf (IEEE Std 1003.1-2017, XCU
   echo and printf).

   Both exist twice over: as a shell built-in and as a utility a script
   may call by name.  They behave the same because both call this, which
   turns arguments into the bytes to be written and nothing else.

   echo here always interprets the escapes of XCU echo and knows only
   -n, which is the behaviour of the shell the build's scripts were
   written against; the XSI form with -e is not accepted, so `echo -e x'
   prints "-e x" as it does there. *)

(* the escapes of echo; the flag says whether \c stopped the output *)
let echo_escapes s =
  let b = Buffer.create (String.length s) in
  let n = String.length s in
  let stop = ref false in
  let i = ref 0 in
  while not !stop && !i < n do
    if s.[!i] = '\\' && !i + 1 < n then
      match s.[!i + 1] with
      | 'a' -> Buffer.add_char b '\007'; i := !i + 2
      | 'b' -> Buffer.add_char b '\b'; i := !i + 2
      | 'c' -> stop := true
      | 'f' -> Buffer.add_char b '\012'; i := !i + 2
      | 'n' -> Buffer.add_char b '\n'; i := !i + 2
      | 'r' -> Buffer.add_char b '\r'; i := !i + 2
      | 't' -> Buffer.add_char b '\t'; i := !i + 2
      | 'v' -> Buffer.add_char b '\011'; i := !i + 2
      | '\\' -> Buffer.add_char b '\\'; i := !i + 2
      | '0' ->
          let k = ref (!i + 2) and v = ref 0 and digits = ref 0 in
          while !digits < 3 && !k < n && s.[!k] >= '0' && s.[!k] <= '7' do
            v := !v * 8 + (Char.code s.[!k] - 48); incr k; incr digits
          done;
          Buffer.add_char b (Char.chr (!v land 255)); i := !k
      | c -> Buffer.add_char b '\\'; Buffer.add_char b c; i := !i + 2
    else (Buffer.add_char b s.[!i]; incr i)
  done;
  (Buffer.contents b, !stop)

let echo args =
  let newline, args = match args with
    | "-n" :: rest -> false, rest
    | _ -> true, args in
  let b = Buffer.create 128 in
  let cut = ref false in
  List.iteri (fun i a ->
      if not !cut then begin
        if i > 0 then Buffer.add_char b ' ';
        let (text, stop) = echo_escapes a in
        Buffer.add_string b text;
        if stop then cut := true
      end) args;
  if newline && not !cut then Buffer.add_char b '\n';
  Buffer.contents b

(* the escapes of printf, whose octal form is \ddd rather than echo's
   \0ddd *)
let printf_escapes s =
  let b = Buffer.create (String.length s) in
  let n = String.length s in
  let i = ref 0 in
  while !i < n do
    if s.[!i] = '\\' && !i + 1 < n then
      match s.[!i + 1] with
      | 'a' -> Buffer.add_char b '\007'; i := !i + 2
      | 'b' -> Buffer.add_char b '\b'; i := !i + 2
      | 'f' -> Buffer.add_char b '\012'; i := !i + 2
      | 'n' -> Buffer.add_char b '\n'; i := !i + 2
      | 'r' -> Buffer.add_char b '\r'; i := !i + 2
      | 't' -> Buffer.add_char b '\t'; i := !i + 2
      | 'v' -> Buffer.add_char b '\011'; i := !i + 2
      | '\\' -> Buffer.add_char b '\\'; i := !i + 2
      | c when c >= '0' && c <= '7' ->
          let k = ref (!i + 1) and v = ref 0 and digits = ref 0 in
          while !digits < 4 && !k < n && s.[!k] >= '0' && s.[!k] <= '7' do
            v := !v * 8 + (Char.code s.[!k] - 48); incr k; incr digits
          done;
          Buffer.add_char b (Char.chr (!v land 255)); i := !k
      | c -> Buffer.add_char b '\\'; Buffer.add_char b c; i := !i + 2
    else (Buffer.add_char b s.[!i]; incr i)
  done;
  Buffer.contents b

(* A numeric argument to printf: an integer constant, or a character
   after a quote, whose value is that character. *)
let number arg =
  if arg = "" then 0
  else if arg.[0] = '\'' || arg.[0] = '"' then
    (if String.length arg > 1 then Char.code arg.[1] else 0)
  else begin
    let neg = arg.[0] = '-' in
    let body = if neg || arg.[0] = '+' then String.sub arg 1 (String.length arg - 1) else arg in
    let v =
      match int_of_string_opt body with
      | Some v -> v
      | None ->
          let k = ref 0 in
          while !k < String.length body && Regex.is_digit body.[!k] do incr k done;
          (match int_of_string_opt (String.sub body 0 !k) with Some v -> v | None -> 0) in
    if neg then -v else v
  end

let pad ~left ~zero ~width s =
  let n = String.length s in
  if n >= width then s
  else if left then s ^ String.make (width - n) ' '
  else if zero then
    (* zeros go after the sign, not before it *)
    if n > 0 && (s.[0] = '-' || s.[0] = '+')
    then String.make 1 s.[0] ^ String.make (width - n) '0' ^ String.sub s 1 (n - 1)
    else String.make (width - n) '0' ^ s
  else String.make (width - n) ' ' ^ s

(* printf: the output, and any diagnostics.  The format is used again from
   the start while arguments remain, and a conversion with no argument
   left behaves as if given an empty string or zero (XCU printf).

   [numeric] says, for the argument at that position, whether it is a
   number rather than a string, which changes one conversion: %c takes
   the first character of a string but the character of that code from a
   number.  The shell's printf has only strings, so it leaves this out;
   awk, whose values carry a type, supplies it. *)
let printf ?(numeric = fun _ -> false) format args =
  let b = Buffer.create 256 in
  let errors = ref [] in
  let queue = ref args in
  let used = ref false in
  let taken = ref 0 in
  let next () =
    match !queue with
    | [] -> ""
    | a :: t -> queue := t; used := true; incr taken; a in
  let last_was_numeric () = numeric (!taken - 1) in
  let n = String.length format in
  let rec pass () =
    let i = ref 0 in
    while !i < n do
      if format.[!i] = '\\' then begin
        let k = ref (!i + 1) in
        while !k < n && format.[!k] <> '\\' && format.[!k] <> '%' do incr k done;
        Buffer.add_string b (printf_escapes (String.sub format !i (!k - !i)));
        i := !k
      end
      else if format.[!i] = '%' then begin
        if !i + 1 < n && format.[!i + 1] = '%' then (Buffer.add_char b '%'; i := !i + 2)
        else begin
          let k = ref (!i + 1) in
          let left = ref false and zero = ref false and plus = ref false
          and space = ref false and alt = ref false in
          let more = ref true in
          while !more && !k < n do
            (match format.[!k] with
             | '-' -> left := true | '0' -> zero := true | '+' -> plus := true
             | ' ' -> space := true | '#' -> alt := true
             | _ -> more := false);
            if !more then incr k
          done;
          let count () =
            if !k < n && format.[!k] = '*' then (incr k; number (next ()))
            else begin
              let start = !k in
              while !k < n && Regex.is_digit format.[!k] do incr k done;
              if !k = start then -1 else int_of_string (String.sub format start (!k - start))
            end in
          let width = count () in
          let prec =
            if !k < n && format.[!k] = '.' then (incr k; let p = count () in if p < 0 then 0 else p)
            else -1 in
          while !k < n && (match format.[!k] with
              | 'l' | 'h' | 'L' | 'q' | 'j' | 'z' | 't' -> true | _ -> false) do incr k done;
          if !k >= n then begin
            errors := Printf.sprintf "%s: missing conversion character" format :: !errors;
            i := n
          end else begin
            let conv = format.[!k] in
            let integral = String.contains "diouxX" conv in
            let text =
              match conv with
              | 'd' | 'i' ->
                  let v = number (next ()) in
                  let s = string_of_int v in
                  if v >= 0 && !plus then "+" ^ s
                  else if v >= 0 && !space then " " ^ s
                  else s
              | 'u' -> Printf.sprintf "%u" (number (next ()))
              | 'o' ->
                  let s = Printf.sprintf "%o" (number (next ())) in
                  if !alt && (s = "" || s.[0] <> '0') then "0" ^ s else s
              | 'x' ->
                  let s = Printf.sprintf "%x" (number (next ())) in
                  if !alt then "0x" ^ s else s
              | 'X' ->
                  let s = Printf.sprintf "%X" (number (next ())) in
                  if !alt then "0X" ^ s else s
              | 'c' ->
                  let a = next () in
                  if last_was_numeric () then
                    (let v = number a in
                     if v = 0 then "" else String.make 1 (Char.chr (v land 255)))
                  else if a = "" then "" else String.make 1 a.[0]
              | 's' ->
                  let a = next () in
                  if prec >= 0 && prec < String.length a then String.sub a 0 prec else a
              | 'b' ->
                  let a = printf_escapes (next ()) in
                  if prec >= 0 && prec < String.length a then String.sub a 0 prec else a
              | 'e' | 'E' | 'f' | 'F' | 'g' | 'G' ->
                  let a = next () in
                  let v = match float_of_string_opt (String.trim a) with Some v -> v | None -> 0.0 in
                  let p = if prec < 0 then 6 else prec in
                  (match conv with
                   | 'f' | 'F' -> Printf.sprintf "%.*f" p v
                   | 'e' -> Printf.sprintf "%.*e" p v
                   | 'E' -> String.uppercase_ascii (Printf.sprintf "%.*e" p v)
                   | 'g' -> Printf.sprintf "%.*g" p v
                   | _ -> String.uppercase_ascii (Printf.sprintf "%.*g" p v))
              | c ->
                  errors := Printf.sprintf "%%%c: invalid conversion" c :: !errors;
                  "" in
            let text =
              if integral && prec >= 0 && String.length text < prec
              then String.make (prec - String.length text) '0' ^ text
              else text in
            Buffer.add_string b (pad ~left:!left ~zero:!zero ~width:(max width 0) text);
            i := !k + 1
          end
        end
      end
      else (Buffer.add_char b format.[!i]; incr i)
    done;
    if !queue <> [] && !used then (used := false; pass ()) in
  pass ();
  (Buffer.contents b, List.rev !errors)
