(* Driving a build: set up the database, read the makefile, pick the
   goals, build them (GNU make manual, chapter 9 for the command line). *)

let default_variables db =
  let d n v = Value.set db ~origin:Value.Default n v in
  d "CC" "cc"; d "CXX" "g++"; d "AR" "ar"; d "AS" "as"; d "CPP" "$(CC) -E";
  d "MAKE" (Filename.quote Sys.executable_name);
  d "SHELL" "/bin/sh";
  d ".RECIPEPREFIX" "";
  (* import the environment (origin Environment, so makefiles override) *)
  Array.iter (fun kv ->
      match String.index_opt kv '=' with
      | Some i -> Value.set db ~origin:Value.Environment (String.sub kv 0 i) (String.sub kv (i + 1) (String.length kv - i - 1))
      | None -> ()) (Unix.environment ())

type options = {
  mutable makefile : string option;
  mutable directory : string option;
  mutable goals : string list;
  mutable overrides : (string * string) list;   (* VAR=value on the command line *)
  mutable keep_going : bool;
  mutable dry_run : bool;
  mutable silent : bool;
  mutable question : bool;
  mutable jobs : int;
}

let find_makefile () =
  List.find_opt Sys.file_exists [ "GNUmakefile"; "makefile"; "Makefile" ]

(* ---------- recursion (GNU make manual 5.7) ---------- *)

(* MAKEFLAGS carries the options down to a sub-make and back up from the
   command line: the single letters run together without a dash, then
   any option that has an argument, then the variable assignments.  A
   sub-make reads it before its own arguments, so its own command line
   still wins. *)
let makeflags o =
  let letters = Buffer.create 8 in
  if o.keep_going then Buffer.add_char letters 'k';
  if o.dry_run then Buffer.add_char letters 'n';
  if o.silent then Buffer.add_char letters 's';
  if o.question then Buffer.add_char letters 'q';
  let pieces =
    (if Buffer.length letters > 0 then [ Buffer.contents letters ] else [])
    @ (if o.jobs > 1 then [ Printf.sprintf "-j%d" o.jobs ] else [])
    @ List.map (fun (n, v) -> n ^ "=" ^ v) o.overrides in
  String.concat " " pieces

(* the words of an inherited MAKEFLAGS, as command-line arguments *)
let flags_from_environment () =
  match Sys.getenv_opt "MAKEFLAGS" with
  | None | Some "" -> []
  | Some text ->
      let words = List.filter (fun w -> w <> "")
          (String.split_on_char ' ' (String.concat " " (String.split_on_char '\t' text))) in
      List.map (fun w ->
          if w = "" then w
          else if w.[0] = '-' || String.contains w '=' then w
          else "-" ^ w)                    (* the run-together letters *)
        words

let level () =
  match int_of_string_opt (try Sys.getenv "MAKELEVEL" with Not_found -> "0") with
  | Some n -> n
  | None -> 0

let run argv =
  let o = { makefile = None; directory = None; goals = []; overrides = []; keep_going = false;
            dry_run = false; silent = false; question = false; jobs = 1 } in
  (* The command line (chapter 9).  Several single-letter options may be
     run together in one argument, which is also the form MAKEFLAGS uses
     to pass them to a sub-make, so a bundle is split here. *)
  let rec args = function
    | [] -> ()
    | ("--file" | "--makefile") :: f :: rest -> o.makefile <- Some f; args rest
    | ("--keep-going") :: rest -> o.keep_going <- true; args rest
    | ("--dry-run" | "--just-print" | "--recon") :: rest -> o.dry_run <- true; args rest
    | ("--silent" | "--quiet") :: rest -> o.silent <- true; args rest
    | ("--question") :: rest -> o.question <- true; args rest
    | ("--directory") :: d :: rest -> o.directory <- Some d; args rest
    | a :: rest when String.length a > 1 && a.[0] = '-' && a.[1] <> '-' ->
        (* one bundle: each letter in turn, and the first that wants an
           argument takes the rest of the word or the next one *)
        let n = String.length a in
        let rest = ref rest in
        let i = ref 1 in
        let stop = ref false in
        while not !stop && !i < n do
          let c = a.[!i] in
          let argument () =
            if !i + 1 < n then (let v = String.sub a (!i + 1) (n - !i - 1) in stop := true; Some v)
            else match !rest with
              | v :: tl -> rest := tl; stop := true; Some v
              | [] -> stop := true; None in
          (match c with
           | 'f' -> (match argument () with Some v -> o.makefile <- Some v | None -> ())
           | 'C' -> (match argument () with Some v -> o.directory <- Some v | None -> ())
           | 'j' ->
               (* the count may be joined, separate or absent *)
               if !i + 1 < n && (let d = a.[!i + 1] in d >= '0' && d <= '9') then
                 (match argument () with
                  | Some v -> o.jobs <- (match int_of_string_opt v with Some k -> k | None -> 1)
                  | None -> ())
               else begin
                 (match !rest with
                  | v :: tl when int_of_string_opt v <> None ->
                      rest := tl;
                      o.jobs <- (match int_of_string_opt v with Some k -> k | None -> 1)
                  | _ -> o.jobs <- 0);           (* -j with no count: no limit *)
                 ()
               end
           | 'k' -> o.keep_going <- true
           | 'n' -> o.dry_run <- true
           | 's' -> o.silent <- true
           | 'q' -> o.question <- true
           | 'I' | 'o' | 'W' -> ignore (argument ())
           | 'r' | 'R' | 'w' | 'B' | 'i' | 'p' | 'd' | 'e' | 'L' | 't' -> ()
           | _ -> ());
          incr i
        done;
        args !rest
    | a :: rest when String.contains a '=' ->
        let i = String.index a '=' in
        o.overrides <- o.overrides @ [ String.sub a 0 i, String.sub a (i + 1) (String.length a - i - 1) ];
        args rest
    | a :: rest when String.length a > 0 && a.[0] = '-' -> args rest   (* ignore unknown flags *)
    | a :: rest -> o.goals <- o.goals @ [ Func.normalise a ]; args rest in
  (* the inherited flags first, so that this make's own arguments win *)
  args (flags_from_environment ());
  args (List.tl (Array.to_list argv));
  let name = Filename.basename argv.(0) in
  let depth = level () in
  (* GNU make names itself with the recursion depth and says which
     directory it is working in, which is what makes a recursive build's
     log readable (5.7.4) *)
  let me = if depth = 0 then name else Printf.sprintf "%s[%d]" name depth in
  let announce verb =
    if (depth > 0 || o.directory <> None) && not o.silent then begin
      Printf.printf "%s: %s directory '%s'\n" me verb (Sys.getcwd ());
      flush stdout
    end in
  (match o.directory with
   | Some d ->
       (match Sys.chdir d with
        | () -> ()
        | exception _ ->
            Printf.eprintf "%s: *** %s: No such file or directory.  Stop.\n" me d;
            exit 2)
   | None -> ());
  let db = Value.create () in
  default_variables db;
  (* A variable given on the command line is recursively expanded, like
     one written with '=': the build passes
     OCAMLRUN='$(ROOTDIR)/boot/ocamlrun' down to a sub-make and expects
     the sub-make to expand it there (9.5). *)
  List.iter (fun (n, v) ->
      Value.set db ~origin:Value.Command_line ~flavour:Value.Recursive n v) o.overrides;
  (* MAKEFLAGS/goal export for recursion *)
  (* A sub-make sees one more level and the flags in force; both go into
     the environment, since the recipe that starts it is a child. *)
  Value.set db ~origin:Value.Command_line "MAKELEVEL" (string_of_int depth);
  (try Unix.putenv "MAKELEVEL" (string_of_int (depth + 1)) with _ -> ());
  let flags = makeflags o in
  Value.set db ~origin:Value.Command_line "MAKEFLAGS" flags;
  (try Unix.putenv "MAKEFLAGS" flags with _ -> ());
  let rules = Rule.create () in
  let st = { Eval.db; rules; current = None; include_dirs = []; default_goal = None; static = None } in
  (* $(eval TEXT) evaluates into the same databases, so rules and the
     default goal it defines are visible to the rest of the build *)
  Expand.eval_hook := (fun _db text -> Eval.eval_text st text);
  announce "Entering";
  let mf = match o.makefile with Some f -> Some f | None -> find_makefile () in
  (match mf with
   | Some f ->
       (match In_channel.with_open_bin f In_channel.input_all with
        | text -> Eval.eval_text st text
        | exception Sys_error msg ->
            Printf.eprintf "%s: %s\n" me msg; exit 2)
   | None ->
       Printf.eprintf "%s: *** No targets specified and no makefile found.  Stop.\n" me;
       exit 2);
  let goals = if o.goals <> [] then o.goals
    else match st.default_goal with Some g -> [ g ] | None -> (match rules.Rule.explicit with r :: _ -> [ List.hd r.targets ] | [] -> []) in
  let ok = Build.build ~db ~rules ~keep_going:o.keep_going ~dry_run:o.dry_run
      ~silent:o.silent ~question:o.question ~jobs:o.jobs ~name:me goals in
  announce "Leaving";
  if ok then 0 else if o.question then 1 else 2
