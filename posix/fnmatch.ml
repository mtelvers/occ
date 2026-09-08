(* Pattern matching notation (IEEE Std 1003.1-2017, XCU 2.13) and the
   pathname expansion built on it (2.6.6).

   The notation is not the regular-expression one: '*' matches any string,
   '?' any single character, and a bracket expression as in XBD 9.3.5.  A
   backslash quotes the next character (2.13.3), which is also how the
   shell's expander marks a character that came from quoted text and must
   not be taken as a pattern.

   Two rules apply when a pattern is matched against a pathname (2.13.3):
   a '/' must be matched by a literal '/', never by a wildcard, and a '.'
   at the start of a name must be matched by a literal '.'.  Pathname
   expansion matches one component at a time, so [pathname] is only needed
   where a whole path is matched at once. *)

(* the bracket expression at [i] (just after '['): its membership test and
   the index just past it, or None if there is no closing ']' *)
let bracket pat i =
  let n = String.length pat in
  let negated = i < n && (pat.[i] = '!' || pat.[i] = '^') in
  let i = if negated then i + 1 else i in
  let set = Array.make 256 false in
  let rec loop i first =
    if i >= n then None
    else if pat.[i] = ']' && not first then Some (i + 1)
    else if pat.[i] = '[' && i + 1 < n && pat.[i + 1] = ':' then begin
      match String.index_from_opt pat (i + 2) ':' with
      | Some j when j + 1 < n && pat.[j + 1] = ']' ->
          let pred = Regex.class_pred (String.sub pat (i + 2) (j - i - 2)) in
          for k = 0 to 255 do if pred (Char.chr k) then set.(k) <- true done;
          loop (j + 2) false
      | _ -> None
    end else begin
      (* inside a bracket expression a backslash quotes, as the shell needs
         to pass through a quoted ']' or '-' *)
      let c, i = if pat.[i] = '\\' && i + 1 < n then pat.[i + 1], i + 1 else pat.[i], i in
      if i + 2 < n && pat.[i + 1] = '-' && pat.[i + 2] <> ']' then begin
        let hi, i = if pat.[i + 2] = '\\' && i + 3 < n then pat.[i + 3], i + 3 else pat.[i + 2], i + 2 in
        for k = Char.code c to Char.code hi do set.(k) <- true done;
        loop (i + 1) false
      end else (set.(Char.code c) <- true; loop (i + 1) false)
    end in
  match loop i true with
  | None -> None
  | Some stop ->
      Some ((fun c -> if negated then not set.(Char.code c) else set.(Char.code c)), stop)

let matches ?(pathname = false) ?(period = false) pat s =
  let np = String.length pat and ns = String.length s in
  (* a '.' that must be matched literally: at the start of the string, and
     after a '/' when whole pathnames are being matched *)
  let leading j = j = 0 || (pathname && j > 0 && s.[j - 1] = '/') in
  let plain j = pathname && s.[j] = '/' in
  let rec go i j =
    if i >= np then j >= ns
    else
      match pat.[i] with
      | '*' ->
          (* the shortest tail that works; a '*' never spans a '/' when
             matching a whole pathname *)
          let rec try_from j =
            if go (i + 1) j then true
            else if j >= ns then false
            else if plain j then false
            else if period && leading j && s.[j] = '.' then false
            else try_from (j + 1) in
          if period && leading j && j < ns && s.[j] = '.' then go (i + 1) j
          else try_from j
      | '?' ->
          j < ns && not (plain j)
          && not (period && leading j && s.[j] = '.')
          && go (i + 1) (j + 1)
      | '[' ->
          (match bracket pat (i + 1) with
           | None -> j < ns && s.[j] = '[' && go (i + 1) (j + 1)   (* an unmatched '[' is literal *)
           | Some (mem, stop) ->
               j < ns && not (plain j)
               && not (period && leading j && s.[j] = '.')
               && mem s.[j] && go stop (j + 1))
      | '\\' when i + 1 < np -> j < ns && s.[j] = pat.[i + 1] && go (i + 2) (j + 1)
      | c -> j < ns && s.[j] = c && go (i + 1) (j + 1) in
  go 0 0

(* Does the string hold a pattern character that is not quoted?  A word
   with none is not a pattern and is left alone (2.6.6). *)
let is_pattern s =
  let n = String.length s in
  let rec go i =
    i < n
    && (match s.[i] with
        | '\\' -> go (i + 2)
        | '*' | '?' | '[' -> true
        | _ -> go (i + 1)) in
  go 0

(* remove the quoting backslashes (2.2.1) *)
let unescape s =
  let b = Buffer.create (String.length s) in
  let n = String.length s in
  let rec go i =
    if i < n then
      if s.[i] = '\\' && i + 1 < n then (Buffer.add_char b s.[i + 1]; go (i + 2))
      else (Buffer.add_char b s.[i]; go (i + 1)) in
  go 0;
  Buffer.contents b

(* split a pattern on its unquoted slashes *)
let components pat =
  let n = String.length pat in
  let out = ref [] and b = Buffer.create 16 and i = ref 0 in
  let flush () = if Buffer.length b > 0 then (out := Buffer.contents b :: !out; Buffer.clear b) in
  while !i < n do
    (match pat.[!i] with
     | '\\' when !i + 1 < n ->
         Buffer.add_char b '\\'; Buffer.add_char b pat.[!i + 1]; i := !i + 2
     | '/' -> flush (); incr i
     | c -> Buffer.add_char b c; incr i)
  done;
  flush ();
  List.rev !out

let read_dir d =
  match Sys.readdir (if d = "" then "." else d) with
  | entries -> Array.to_list entries
  | exception _ -> []

(* Pathname expansion (2.6.6): the sorted list of existing pathnames the
   pattern matches, or None if it matches nothing, in which case the shell
   keeps the word as it stands. *)
let glob pat =
  if pat = "" then None
  else begin
    let absolute = pat.[0] = '/' in
    let trailing = pat.[String.length pat - 1] = '/' in
    let comps = components pat in
    let start = if absolute then [ "/" ] else [ "" ] in
    let join d c = if d = "" then c else if d = "/" then "/" ^ c else d ^ "/" ^ c in
    let is_dir p = match Sys.is_directory p with b -> b | exception _ -> false in
    let rec walk dirs = function
      | [] -> dirs
      | comp :: rest ->
          let last = rest = [] in
          (* an intermediate component must name a directory to go on *)
          let usable p = if last && not trailing then true else is_dir p in
          let out =
            List.concat_map (fun d ->
                if is_pattern comp then begin
                  (* '.' and '..' are entries like any other, but only a
                     pattern with a literal leading '.' reaches them *)
                  let entries = "." :: ".." :: read_dir d in
                  List.filter (fun e -> matches ~period:true comp e) entries
                  |> List.sort compare
                  |> List.map (join d)
                  |> List.filter usable
                end else begin
                  (* a component with no pattern character is taken as it
                     is, but it still has to exist *)
                  let p = join d (unescape comp) in
                  if (if last && not trailing then Sys.file_exists p || is_dir p else is_dir p)
                  then [ p ] else []
                end) dirs in
          walk out rest in
    match walk start comps with
    | [] -> None
    | hits ->
        let hits = if trailing then List.map (fun h -> h ^ "/") hits else hits in
        Some (List.sort compare hits)
  end
