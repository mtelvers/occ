(* Word expansion (IEEE Std 1003.1-2017, XCU 2.6), in the order 2.6 sets
   out: tilde expansion, then parameter expansion, command substitution
   and arithmetic expansion, then field splitting (2.6.5), then pathname
   expansion (2.6.6), then quote removal (2.6.7).

   The order is only half the difficulty.  The other half is that each
   step must know where the characters it is looking at came from: a '*'
   is a pattern if it was written plainly, and a literal if it was
   quoted or came out of a variable that was quoted; a space splits a
   field only if it came out of an *unquoted* expansion, never if it was
   written in the word.  So a field is built here as text with one mark
   per character:

     'L'  written plainly in the word: a pattern character, never a
          field delimiter
     'E'  the result of an unquoted expansion: both
     'Q'  quoted, or the result of a quoted expansion: neither

   Splitting then looks only at 'E' characters, pattern matching escapes
   only 'Q' ones, and quote removal is just dropping the marks. *)

open Ast

(* Command substitution runs a program, which is the executor's job; it
   fills this in. *)
let subst_hook : (State.t -> Ast.program -> string) ref =
  ref (fun _ _ -> failwith "Expand.subst_hook: not set")

exception Error of string

let err fmt = Printf.ksprintf (fun s -> raise (Error s)) fmt

(* ---------- the field being built ---------- *)

type acc = {
  mutable fields : (string * string) list;   (* finished, reversed: text, marks *)
  txt : Buffer.t;
  mrk : Buffer.t;
  mutable pending : bool;                    (* a field exists, even if empty *)
}

let fresh () =
  { fields = []; txt = Buffer.create 64; mrk = Buffer.create 64; pending = false }

let add a mark s =
  if s <> "" then begin
    Buffer.add_string a.txt s;
    Buffer.add_string a.mrk (String.make (String.length s) mark);
    a.pending <- true
  end

let push a =
  a.fields <- (Buffer.contents a.txt, Buffer.contents a.mrk) :: a.fields;
  Buffer.clear a.txt;
  Buffer.clear a.mrk;
  a.pending <- false

(* ---------- reading a parameter (2.5) ---------- *)

let joined st sep = String.concat sep st.State.params

let param_value st name =
  match name with
  | "@" | "*" ->
      let ifs = State.ifs st in
      Some (joined st (if ifs = "" then "" else String.make 1 ifs.[0]))
  | "#" -> Some (string_of_int (List.length st.State.params))
  | "?" -> Some (string_of_int st.State.status)
  | "$" -> Some (string_of_int st.State.pid)
  | "!" -> Some (string_of_int st.State.last_bg)
  | "0" -> Some st.State.arg0
  | "-" ->
      (* the options in force, as single letters (2.5.2) *)
      let o = st.State.opts in
      let b = Buffer.create 8 in
      let f flag c = if flag then Buffer.add_char b c in
      f o.State.errexit 'e'; f o.State.nounset 'u'; f o.State.xtrace 'x';
      f o.State.noglob 'f'; f o.State.verbose 'v'; f o.State.noexec 'n';
      f o.State.noclobber 'C'; f o.State.allexport 'a';
      Some (Buffer.contents b)
  | name when String.for_all Posix.Regex.is_digit name ->
      State.positional st (int_of_string name)
  | name -> State.get st name

(* is the parameter set, and if so is it non-null? *)
let param_state st name =
  match param_value st name with
  | None -> `Unset
  | Some "" -> `Null
  | Some v -> `Set v

(* ---------- prefix and suffix removal (2.6.2) ---------- *)

let trim_prefix ~longest pat s =
  let n = String.length s in
  let candidates = List.init (n + 1) (fun k -> if longest then n - k else k) in
  match List.find_opt (fun k -> Posix.Fnmatch.matches pat (String.sub s 0 k)) candidates with
  | Some k -> String.sub s k (n - k)
  | None -> s

let trim_suffix ~longest pat s =
  let n = String.length s in
  let candidates = List.init (n + 1) (fun k -> if longest then n - k else k) in
  match List.find_opt (fun k -> Posix.Fnmatch.matches pat (String.sub s (n - k) k)) candidates with
  | Some k -> String.sub s 0 (n - k)
  | None -> s

(* ---------- the walk ---------- *)

let rec part st a ~quoted p =
  match p with
  | Str s -> add a (if quoted then 'Q' else 'L') s
  | Single s -> a.pending <- true; add a 'Q' s
  | Esc c -> add a 'Q' (String.make 1 c)
  | Double ps -> a.pending <- true; List.iter (part st a ~quoted:true) ps
  | Param pe -> parameter st a ~quoted pe
  | Subst prog ->
      let text = !subst_hook st prog in
      (* the trailing newlines of the output are removed (2.6.3) *)
      let n = ref (String.length text) in
      while !n > 0 && text.[!n - 1] = '\n' do decr n done;
      add a (if quoted then 'Q' else 'E') (String.sub text 0 !n)
  | Arith ps ->
      let text = to_string st ps in
      let value =
        try Arith.eval ~get:(fun n -> match param_value st n with Some v -> v | None -> "")
              ~set:(fun n v -> State.set st n v) text
        with Arith.Error msg -> err "arithmetic: %s" msg in
      add a (if quoted then 'Q' else 'E') (string_of_int value)

and parameter st a ~quoted pe =
  let mark = if quoted then 'Q' else 'E' in
  let name = pe.pname in
  match pe.pop with
  | Get when name = "@" ->
      (* "$@" gives one field per parameter and nothing at all when there
         are none, which is what lets `set -- ; f "$@"' pass no arguments *)
      List.iteri (fun i p ->
          if i > 0 then push a;
          a.pending <- true;
          add a mark p) st.State.params
  | Get ->
      (match param_value st name with
       | Some v -> add a mark v
       | None ->
           if st.State.opts.State.nounset then err "%s: parameter not set" name)
  | Length ->
      let len = match name with
        | "@" | "*" -> List.length st.State.params
        | _ -> (match param_value st name with Some v -> String.length v | None -> 0) in
      add a mark (string_of_int len)
  | Default (colon, w) ->
      (match param_state st name, colon with
       | `Set v, _ -> add a mark v
       | `Null, false -> add a mark ""
       | _ -> List.iter (part st a ~quoted) w)
  | Assign (colon, w) ->
      (match param_state st name, colon with
       | `Set v, _ -> add a mark v
       | `Null, false -> add a mark ""
       | _ ->
           let v = to_string st w in
           State.set st name v;
           add a mark v)
  | Fail (colon, w) ->
      (match param_state st name, colon with
       | `Set v, _ -> add a mark v
       | `Null, false -> add a mark ""
       | _ ->
           let msg = to_string st w in
           err "%s: %s" name (if msg = "" then "parameter not set" else msg))
  | Alt (colon, w) ->
      (match param_state st name, colon with
       | `Unset, _ -> ()
       | `Null, true -> ()
       | _ -> List.iter (part st a ~quoted) w)
  | Prefix (longest, w) ->
      let pat = to_pattern st w in
      (match param_value st name with
       | Some v -> add a mark (trim_prefix ~longest pat v)
       | None -> ())
  | Suffix (longest, w) ->
      let pat = to_pattern st w in
      (match param_value st name with
       | Some v -> add a mark (trim_suffix ~longest pat v)
       | None -> ())

(* the word as one string, with the quoting removed: for an assignment's
   value, a redirection's target, a here-document, a case subject *)
and to_string st word =
  let a = fresh () in
  List.iter (part st a ~quoted:false) (tilde st word);
  if a.pending then push a;
  String.concat " " (List.rev_map fst a.fields |> List.rev)

(* the word as a pattern, with the quoted characters escaped so that
   Fnmatch takes them literally: for `case', ${x#pat} and friends *)
and to_pattern st word =
  let a = fresh () in
  List.iter (part st a ~quoted:false) word;
  if a.pending then push a;
  let one (txt, mrk) =
    let b = Buffer.create (String.length txt) in
    String.iteri (fun i c ->
        if mrk.[i] = 'Q' || c = '\\' then Buffer.add_char b '\\';
        Buffer.add_char b c) txt;
    Buffer.contents b in
  String.concat " " (List.rev_map one a.fields |> List.rev)

(* Tilde expansion (2.6.1), which happens before everything else and only
   at the start of a word.  Its result is not a pattern and is not split,
   so it is marked as quoted. *)
and tilde st word =
  match word with
  | Str s :: rest when s <> "" && s.[0] = '~' ->
      let stop = match String.index_opt s '/' with Some k -> k | None -> String.length s in
      let user = String.sub s 1 (stop - 1) in
      let home =
        if user = "" then State.get st "HOME"
        else (match Unix.getpwnam user with
            | pw -> Some pw.Unix.pw_dir
            | exception Not_found -> None) in
      (match home with
       | Some dir when dir <> "" ->
           Single dir :: Str (String.sub s stop (String.length s - stop)) :: rest
       | _ -> word)
  | _ -> word

(* ---------- field splitting (2.6.5) ---------- *)

let split st (txt, mrk) =
  let ifs = State.ifs st in
  if ifs = "" then [ (txt, mrk) ]
  else begin
    let n = String.length txt in
    let delim i = mrk.[i] = 'E' && String.contains ifs txt.[i] in
    let white i = delim i && (txt.[i] = ' ' || txt.[i] = '\t' || txt.[i] = '\n') in
    let out = ref [] and tb = Buffer.create n and mb = Buffer.create n in
    let close () =
      out := (Buffer.contents tb, Buffer.contents mb) :: !out;
      Buffer.clear tb; Buffer.clear mb in
    let i = ref 0 in
    (* IFS white space at the ends of the result is discarded *)
    while !i < n && white !i do incr i done;
    while !i < n do
      if delim !i then begin
        (* a delimiter is a run of IFS white space, which may have one
           other IFS character in it *)
        while !i < n && white !i do incr i done;
        if !i < n && delim !i then incr i;
        while !i < n && white !i do incr i done;
        if !i < n then close ()
      end else begin
        Buffer.add_char tb txt.[!i];
        Buffer.add_char mb mrk.[!i];
        incr i
      end
    done;
    if Buffer.length tb > 0 || !out = [] then close ();
    List.rev !out
  end

(* ---------- pathname expansion (2.6.6) ---------- *)

let pathnames st (txt, mrk) =
  if st.State.opts.State.noglob then [ txt ]
  else begin
    let b = Buffer.create (String.length txt) in
    String.iteri (fun i c ->
        if mrk.[i] = 'Q' || c = '\\' then Buffer.add_char b '\\';
        Buffer.add_char b c) txt;
    let pat = Buffer.contents b in
    if not (Posix.Fnmatch.is_pattern pat) then [ txt ]
    else match Posix.Fnmatch.glob pat with
      | Some hits -> hits
      | None -> [ txt ]        (* no match: the word is left as it was *)
  end

(* ---------- the whole of 2.6 ---------- *)

let fields st word =
  let a = fresh () in
  List.iter (part st a ~quoted:false) (tilde st word);
  if a.pending then push a;
  List.rev a.fields
  |> List.concat_map (split st)
  |> List.concat_map (pathnames st)

let words st ws = List.concat_map (fields st) ws
