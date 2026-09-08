(* Option parsing to the Utility Syntax Guidelines (IEEE Std 1003.1-2017,
   XBD 12.2).

   The guidelines are what makes `sed -ne p', `sed -n -e p' and
   `sed -n -ep' the same command: options are single characters after a
   '-', several may be grouped in one argument, and an option's argument
   may be joined to it or be the next argument.  A '--' argument ends the
   options, a lone '-' is an operand, and by guideline 9 no option follows
   the first operand.

   The GNU utilities the build calls also take a few long options
   (--color=always, --gnu, --version), so [long] names those a utility
   accepts.  They also read options that come after an operand, against
   guideline 9, and since these utilities have to behave as the ones the
   scripts were written against, [permute] does the same and is on by
   default; a utility whose operands may start with '-' turns it off. *)

exception Error of string

(* Whether an option takes an argument.  A few of these utilities have
   one whose argument is optional and must be joined to it, as sed's -i
   is: `sed -i' and `sed -i.bak' are both valid, and `sed -i script' is
   not the script being given to -i. *)
type arg_kind = No_arg | Required | Optional

type spec = {
  short : (char * arg_kind) list;
  long : (string * bool) list;
}

(* "n" for no argument, "e:" for one, "i::" for an optional one *)
let short_spec s =
  let n = String.length s in
  let rec go i acc =
    if i >= n then List.rev acc
    else if i + 2 < n && s.[i + 1] = ':' && s.[i + 2] = ':' then go (i + 3) ((s.[i], Optional) :: acc)
    else if i + 1 < n && s.[i + 1] = ':' then go (i + 2) ((s.[i], Required) :: acc)
    else go (i + 1) ((s.[i], No_arg) :: acc) in
  go 0 []

let spec ?(long = []) shorts = { short = short_spec shorts; long }

(* An option is named by the string a utility matches on: one character
   for a short option, the full name for a long one. *)
type opt = string * string option

let parse ?(permute = true) sp argv =
  let n = Array.length argv in
  let opts = ref [] and operands = ref [] in
  let i = ref 0 in
  let stop = ref false in
  while not !stop && !i < n do
    let a = argv.(!i) in
    if a = "--" then (incr i; stop := true)
    else if String.length a >= 2 && a.[0] = '-' && a.[1] = '-' then begin
      (* --name or --name=value *)
      let body = String.sub a 2 (String.length a - 2) in
      let name, inline =
        match String.index_opt body '=' with
        | Some k -> String.sub body 0 k, Some (String.sub body (k + 1) (String.length body - k - 1))
        | None -> body, None in
      (match List.assoc_opt name sp.long with
       | None -> raise (Error (Printf.sprintf "unrecognized option '--%s'" name))
       | Some takes_arg ->
           let arg =
             match inline, takes_arg with
             | Some v, true -> Some v
             | Some _, false -> raise (Error (Printf.sprintf "option '--%s' doesn't allow an argument" name))
             | None, false -> None
             | None, true ->
                 incr i;
                 if !i >= n then raise (Error (Printf.sprintf "option '--%s' requires an argument" name));
                 Some argv.(!i) in
           opts := (name, arg) :: !opts);
      incr i
    end
    else if String.length a >= 2 && a.[0] = '-' then begin
      (* a group of short options, the last of which may take an argument *)
      let j = ref 1 in
      let len = String.length a in
      while !j < len do
        let c = a.[!j] in
        match List.assoc_opt c sp.short with
        | None -> raise (Error (Printf.sprintf "invalid option -- '%c'" c))
        | Some No_arg -> opts := (String.make 1 c, None) :: !opts; incr j
        | Some Optional ->
            (* only text joined to the letter counts as its argument *)
            if !j + 1 < len then begin
              opts := (String.make 1 c, Some (String.sub a (!j + 1) (len - !j - 1))) :: !opts;
              j := len
            end else (opts := (String.make 1 c, Some "") :: !opts; incr j)
        | Some Required ->
            if !j + 1 < len then begin
              (* the rest of this argument is the option's argument *)
              opts := (String.make 1 c, Some (String.sub a (!j + 1) (len - !j - 1))) :: !opts;
              j := len
            end else begin
              incr i;
              if !i >= n then raise (Error (Printf.sprintf "option requires an argument -- '%c'" c));
              opts := (String.make 1 c, Some argv.(!i)) :: !opts;
              j := len
            end
      done;
      incr i
    end
    else if permute then (operands := a :: !operands; incr i)
    else stop := true      (* guideline 9: the first operand ends the options *)
  done;
  while !i < n do operands := argv.(!i) :: !operands; incr i done;
  (List.rev !opts, List.rev !operands)

(* helpers for the utilities *)
let has opts name = List.mem_assoc name opts
let arg opts name = match List.assoc_opt name opts with Some a -> a | None -> None
let all opts name = List.filter_map (fun (k, v) -> if k = name then v else None) opts
let count opts name = List.length (List.filter (fun (k, _) -> k = name) opts)
