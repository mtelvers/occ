(* The utilities that change the file hierarchy: rm, cp, mv, mkdir,
   rmdir, ln, touch, chmod and mktemp (IEEE Std 1003.1-2017, XCU).

   None of them prompts.  The standard has them ask before overwriting or
   removing in some cases when the input is a terminal, and -f or -i
   settle it otherwise; a build is never at a terminal, so the -f
   behaviour is the only one that matters and -i is accepted and has no
   effect.  What does matter is that -f means "and do not complain if it
   was not there", which is why `rm -f' appears 349 times in the build. *)

open Util

let is_dir p = match Unix.stat p with
  | { Unix.st_kind = Unix.S_DIR; _ } -> true
  | _ -> false
  | exception _ -> false

let exists p = match Unix.lstat p with _ -> true | exception _ -> false

let entries d =
  match Sys.readdir d with
  | a -> Array.to_list a
  | exception _ -> []

(* ---------- rm ---------- *)

let rec remove_tree ~force path status =
  if is_dir path && not (match Unix.lstat path with
      | { Unix.st_kind = Unix.S_LNK; _ } -> true | _ -> false | exception _ -> false)
  then begin
    List.iter (fun e -> remove_tree ~force (Filename.concat path e) status) (entries path);
    match Unix.rmdir path with
    | () -> ()
    | exception e -> if not force then (warn "%s" (sys_message e); status := 1)
  end else
    match Unix.unlink path with
    | () -> ()
    | exception e -> if not force then (warn "%s" (sys_message e); status := 1)

let rm _argv opts operands =
  let force = Posix.Getopt.has opts "f" in
  let recursive = Posix.Getopt.has opts "r" || Posix.Getopt.has opts "R" in
  let status = ref 0 in
  List.iter (fun path ->
      if not (exists path) then
        (if not force then (warn "cannot remove '%s': %s" path (Unix.error_message Unix.ENOENT); status := 1))
      else if is_dir path && not recursive then
        (warn "cannot remove '%s': Is a directory" path; status := 1)
      else if recursive then remove_tree ~force path status
      else match Unix.unlink path with
        | () -> ()
        | exception e -> if not force then (warn "%s" (sys_message e); status := 1))
    operands;
  !status

(* ---------- cp and mv ---------- *)

let copy_file ~preserve src dst =
  let st = Unix.stat src in
  match st.Unix.st_kind with
  | Unix.S_LNK -> Unix.symlink (Unix.readlink src) dst
  | _ ->
      let ic = open_in_bin src in
      (* A new copy is created with the source's permission bits, which
         the file mode creation mask then reduces, so an executable stays
         executable; an existing destination keeps the mode it had.
         Getting this wrong leaves a copied program unrunnable. *)
      let oc = open_out_gen [ Open_wronly; Open_creat; Open_trunc; Open_binary ]
          st.Unix.st_perm dst in
      let chunk = Bytes.create 65536 in
      let rec go () =
        let k = input ic chunk 0 65536 in
        if k > 0 then (output oc chunk 0 k; go ()) in
      go ();
      close_in ic;
      close_out oc;
      if preserve then begin
        (try Unix.chmod dst st.Unix.st_perm with _ -> ());
        (try Unix.utimes dst st.Unix.st_atime st.Unix.st_mtime with _ -> ())
      end

let rec copy_tree ~preserve ~recursive src dst status =
  match Unix.lstat src with
  | exception e -> warn "%s" (sys_message e); status := 1
  | { Unix.st_kind = Unix.S_LNK; _ } when recursive ->
      (try (if exists dst then Unix.unlink dst); Unix.symlink (Unix.readlink src) dst
       with e -> warn "%s" (sys_message e); status := 1)
  | { Unix.st_kind = Unix.S_DIR; _ } ->
      if not recursive then (warn "-r not specified; omitting directory '%s'" src; status := 1)
      else begin
        if not (is_dir dst) then
          (try Unix.mkdir dst 0o777 with e -> warn "%s" (sys_message e); status := 1);
        List.iter (fun e ->
            copy_tree ~preserve ~recursive (Filename.concat src e) (Filename.concat dst e) status)
          (entries src)
      end
  | _ ->
      (match copy_file ~preserve src dst with
       | () -> ()
       | exception e -> warn "%s" (sys_message e); status := 1)

let destination dst src = if is_dir dst then Filename.concat dst (Filename.basename src) else dst

let cp _argv opts operands =
  let archive = Posix.Getopt.has opts "a" in
  let preserve = archive || Posix.Getopt.has opts "p" in
  let recursive = archive || Posix.Getopt.has opts "r" || Posix.Getopt.has opts "R" in
  let force = Posix.Getopt.has opts "f" in
  match List.rev operands with
  | [] | [ _ ] -> die 1 "usage: cp [-fpRr] source... target"
  | dst :: sources_rev ->
      let sources = List.rev sources_rev in
      if List.length sources > 1 && not (is_dir dst) then
        die 1 "target '%s' is not a directory" dst;
      let status = ref 0 in
      List.iter (fun src ->
          let target = destination dst src in
          if force && exists target && not (is_dir target) then (try Unix.unlink target with _ -> ());
          copy_tree ~preserve ~recursive src target status) sources;
      !status

let mv _argv opts operands =
  let force = Posix.Getopt.has opts "f" in
  ignore force;
  match List.rev operands with
  | [] | [ _ ] -> die 1 "usage: mv [-f] source... target"
  | dst :: sources_rev ->
      let sources = List.rev sources_rev in
      if List.length sources > 1 && not (is_dir dst) then
        die 1 "target '%s' is not a directory" dst;
      let status = ref 0 in
      List.iter (fun src ->
          let target = destination dst src in
          match Unix.rename src target with
          | () -> ()
          | exception Unix.Unix_error (Unix.EXDEV, _, _) ->
              (* across file systems a rename cannot work, so copy and
                 remove, which is what mv is defined to do (XCU mv) *)
              copy_tree ~preserve:true ~recursive:true src target status;
              remove_tree ~force:true src status
          | exception e -> warn "%s" (sys_message e); status := 1) sources;
      !status

(* ---------- mkdir and rmdir ---------- *)

let mode_arg opts default =
  match Posix.Getopt.arg opts "m" with
  | Some m -> (match int_of_string_opt ("0o" ^ m) with
      | Some v -> v
      | None -> die 1 "%s: invalid mode" m)
  | None -> default

let mkdir _argv opts operands =
  let parents = Posix.Getopt.has opts "p" in
  let mode = mode_arg opts 0o777 in
  let status = ref 0 in
  let rec make path =
    if path = "" || path = "/" || is_dir path then ()
    else begin
      if parents then make (Filename.dirname path);
      match Unix.mkdir path mode with
      | () -> ()
      | exception Unix.Unix_error (Unix.EEXIST, _, _) when parents -> ()
      | exception e -> warn "%s" (sys_message e); status := 1
    end in
  List.iter (fun path ->
      if parents then make path
      else match Unix.mkdir path mode with
        | () -> ()
        | exception e -> warn "%s" (sys_message e); status := 1)
    operands;
  !status

let rmdir _argv opts operands =
  let parents = Posix.Getopt.has opts "p" in
  let status = ref 0 in
  let rec remove path =
    match Unix.rmdir path with
    | () -> if parents then (let up = Filename.dirname path in if up <> path && up <> "." then remove up)
    | exception e -> warn "%s" (sys_message e); status := 1 in
  List.iter remove operands;
  !status

(* ---------- ln ---------- *)

let ln _argv opts operands =
  let symbolic = Posix.Getopt.has opts "s" in
  let force = Posix.Getopt.has opts "f" in
  match List.rev operands with
  | [] -> die 1 "usage: ln [-fs] source... target"
  | [ src ] ->
      let target = Filename.basename src in
      if force && exists target then (try Unix.unlink target with _ -> ());
      (match (if symbolic then Unix.symlink src target else Unix.link src target) with
       | () -> 0
       | exception e -> warn "%s" (sys_message e); 1)
  | dst :: sources_rev ->
      let sources = List.rev sources_rev in
      let status = ref 0 in
      List.iter (fun src ->
          let target = destination dst src in
          if force && exists target then (try Unix.unlink target with _ -> ());
          match (if symbolic then Unix.symlink src target else Unix.link src target) with
          | () -> ()
          | exception e -> warn "%s" (sys_message e); status := 1) sources;
      !status

(* ---------- touch ---------- *)

let touch _argv opts operands =
  let no_create = Posix.Getopt.has opts "c" in
  let time =
    match Posix.Getopt.arg opts "r" with
    | Some ref_file ->
        (match Unix.stat ref_file with
         | s -> Some s.Unix.st_mtime
         | exception e -> warn "%s" (sys_message e); None)
    | None ->
        (match Posix.Getopt.arg opts "t" with
         | Some spec ->
             (* [[CC]YY]MMDDhhmm[.SS] *)
             let (body, seconds) =
               match String.index_opt spec '.' with
               | Some k ->
                   (String.sub spec 0 k,
                    (match int_of_string_opt (String.sub spec (k + 1) (String.length spec - k - 1)) with
                     | Some v -> v | None -> 0))
               | None -> (spec, 0) in
             let n = String.length body in
             let take a =
               if a + 2 > n then 0
               else match int_of_string_opt (String.sub body a 2) with Some v -> v | None -> 0 in
             let (year, rest) =
               if n = 12 then ((match int_of_string_opt (String.sub body 0 4) with
                   | Some v -> v | None -> 1970), 4)
               else if n = 10 then
                 (let yy = take 0 in (if yy < 69 then 2000 + yy else 1900 + yy), 2)
               else ((Unix.localtime (Unix.time ())).Unix.tm_year + 1900, 0) in
             let tm = { Unix.tm_sec = seconds;
                        tm_min = take (rest + 6);
                        tm_hour = take (rest + 4);
                        tm_mday = take (rest + 2);
                        tm_mon = take rest - 1;
                        tm_year = year - 1900;
                        tm_wday = 0; tm_yday = 0; tm_isdst = false } in
             let (t, _) = Unix.mktime tm in
             Some t
         | None -> None) in
  let now = match time with Some t -> t | None -> Unix.gettimeofday () in
  let status = ref 0 in
  List.iter (fun path ->
      if not (exists path) then begin
        if not no_create then
          match open_out_gen [ Open_wronly; Open_creat; Open_append ] 0o666 path with
          | oc -> close_out oc; (try Unix.utimes path now now with _ -> ())
          | exception e -> warn "%s" (sys_message e); status := 1
      end else
        match Unix.utimes path now now with
        | () -> ()
        | exception e -> warn "%s" (sys_message e); status := 1)
    operands;
  !status

(* ---------- chmod ---------- *)

(* A mode is octal, or symbolic: who, then an operator, then the
   permissions, with several clauses separated by commas (XCU chmod).
   The who part is any of u, g, o and a; the operator is +, - or =; the
   permissions are any of r, w, x, X, s and t. *)
let apply_symbolic spec current is_dir_file =
  let result = ref current in
  List.iter (fun clause ->
      let n = String.length clause in
      let i = ref 0 in
      let who = ref 0 in
      while !i < n && (match clause.[!i] with 'u' | 'g' | 'o' | 'a' -> true | _ -> false) do
        (match clause.[!i] with
         | 'u' -> who := !who lor 0o4700
         | 'g' -> who := !who lor 0o2070
         | 'o' -> who := !who lor 0o0007
         | _ -> who := !who lor 0o7777);
        incr i
      done;
      let who = if !who = 0 then 0o7777 else !who in
      if !i < n then begin
        let op = clause.[!i] in
        incr i;
        let bits = ref 0 in
        while !i < n do
          (match clause.[!i] with
           | 'r' -> bits := !bits lor 0o444
           | 'w' -> bits := !bits lor 0o222
           | 'x' -> bits := !bits lor 0o111
           | 'X' -> if is_dir_file || current land 0o111 <> 0 then bits := !bits lor 0o111
           | 's' -> bits := !bits lor 0o6000
           | 't' -> bits := !bits lor 0o1000
           | _ -> ());
          incr i
        done;
        let bits = !bits land who in
        match op with
        | '+' -> result := !result lor bits
        | '-' -> result := !result land lnot bits
        | '=' -> result := (!result land lnot who) lor bits
        | _ -> ()
      end)
    (String.split_on_char ',' spec);
  !result

let chmod argv opts operands =
  ignore argv;
  let recursive = Posix.Getopt.has opts "R" in
  match operands with
  | [] | [ _ ] -> die 1 "usage: chmod [-R] mode file..."
  | spec :: files ->
      let status = ref 0 in
      let rec change path =
        match Unix.stat path with
        | exception e -> warn "%s" (sys_message e); status := 1
        | st ->
            let mode =
              match int_of_string_opt ("0o" ^ spec) with
              | Some m -> m
              | None -> apply_symbolic spec st.Unix.st_perm (st.Unix.st_kind = Unix.S_DIR) in
            (match Unix.chmod path mode with
             | () -> ()
             | exception e -> warn "%s" (sys_message e); status := 1);
            if recursive && st.Unix.st_kind = Unix.S_DIR then
              List.iter (fun e -> change (Filename.concat path e)) (entries path) in
      List.iter change files;
      !status

(* ---------- mktemp ---------- *)

let mktemp _argv opts operands =
  let want_dir = Posix.Getopt.has opts "d" in
  let dry = Posix.Getopt.has opts "u" in
  let quiet = Posix.Getopt.has opts "q" in
  let template = match operands with
    | t :: _ -> t
    | [] ->
        let dir = match Sys.getenv_opt "TMPDIR" with Some d when d <> "" -> d | _ -> "/tmp" in
        Filename.concat dir "tmp.XXXXXXXXXX" in
  (* the trailing run of X is replaced; the standard asks for at least six *)
  let n = String.length template in
  let tail = ref n in
  while !tail > 0 && template.[!tail - 1] = 'X' do decr tail done;
  let count = n - !tail in
  if count < 3 then (if not quiet then warn "%s: too few X's in template" template; 1)
  else begin
    let alphabet = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789" in
    Random.self_init ();
    let rec attempt tries =
      if tries = 0 then (if not quiet then warn "failed to create a unique name"; 1)
      else begin
        let b = Buffer.create n in
        Buffer.add_string b (String.sub template 0 !tail);
        for _ = 1 to count do
          Buffer.add_char b alphabet.[Random.int (String.length alphabet)]
        done;
        let name = Buffer.contents b in
        if dry then (emit_line name; flush_out (); 0)
        else if want_dir then
          match Unix.mkdir name 0o700 with
          | () -> emit_line name; flush_out (); 0
          | exception Unix.Unix_error (Unix.EEXIST, _, _) -> attempt (tries - 1)
          | exception e -> if not quiet then warn "%s" (sys_message e); 1
        else
          match Unix.openfile name [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_EXCL ] 0o600 with
          | fd -> Unix.close fd; emit_line name; flush_out (); 0
          | exception Unix.Unix_error (Unix.EEXIST, _, _) -> attempt (tries - 1)
          | exception e -> if not quiet then warn "%s" (sys_message e); 1
      end in
    attempt 100
  end
