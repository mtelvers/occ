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

let run argv =
  let o = { makefile = None; directory = None; goals = []; overrides = []; keep_going = false;
            dry_run = false; silent = false; question = false; jobs = 1 } in
  let rec args = function
    | [] -> ()
    | ("-f" | "--file" | "--makefile") :: f :: rest -> o.makefile <- Some f; args rest
    | "-C" :: d :: rest -> o.directory <- Some d; args rest
    | ("-k" | "--keep-going") :: rest -> o.keep_going <- true; args rest
    | ("-n" | "--dry-run" | "--just-print") :: rest -> o.dry_run <- true; args rest
    | ("-s" | "--silent" | "--quiet") :: rest -> o.silent <- true; args rest
    | ("-q" | "--question") :: rest -> o.question <- true; args rest
    | "-j" :: rest -> o.jobs <- 1; (match rest with n :: r when int_of_string_opt n <> None -> args r | _ -> args rest)
    | a :: rest when String.length a > 2 && String.sub a 0 2 = "-j" -> args rest
    | a :: rest when String.length a > 2 && String.sub a 0 2 = "-C" -> o.directory <- Some (String.sub a 2 (String.length a - 2)); args rest
    | a :: rest when String.length a > 2 && String.sub a 0 2 = "-f" -> o.makefile <- Some (String.sub a 2 (String.length a - 2)); args rest
    | a :: rest when String.contains a '=' ->
        let i = String.index a '=' in o.overrides <- o.overrides @ [ String.sub a 0 i, String.sub a (i + 1) (String.length a - i - 1) ]; args rest
    | a :: rest when String.length a > 0 && a.[0] = '-' -> args rest   (* ignore unknown flags *)
    | a :: rest -> o.goals <- o.goals @ [ a ]; args rest in
  args (List.tl (Array.to_list argv));
  Option.iter Sys.chdir o.directory;
  let db = Value.create () in
  default_variables db;
  List.iter (fun (n, v) -> Value.set db ~origin:Value.Command_line ~flavour:Value.Simple n v) o.overrides;
  (* MAKEFLAGS/goal export for recursion *)
  Value.set db ~origin:Value.Command_line "MAKELEVEL" (string_of_int (1 + (match int_of_string_opt (try Sys.getenv "MAKELEVEL" with Not_found -> "0") with Some n -> n | None -> 0)));
  let rules = Rule.create () in
  let st = { Eval.db; rules; current = None; include_dirs = []; default_goal = None; static = None } in
  (* $(eval TEXT) evaluates into the same databases, so rules and the
     default goal it defines are visible to the rest of the build *)
  Expand.eval_hook := (fun _db text -> Eval.eval_text st text);
  let mf = match o.makefile with Some f -> Some f | None -> find_makefile () in
  (match mf with
   | Some f -> Eval.eval_text st (In_channel.with_open_bin f In_channel.input_all)
   | None -> prerr_endline "occmake: no makefile found"; exit 2);
  let goals = if o.goals <> [] then o.goals
    else match st.default_goal with Some g -> [ g ] | None -> (match rules.Rule.explicit with r :: _ -> [ List.hd r.targets ] | [] -> []) in
  let ok = Build.build ~db ~rules ~keep_going:o.keep_going ~dry_run:o.dry_run ~silent:o.silent ~question:o.question goals in
  if ok then 0 else if o.question then 1 else 2
