(* Whole-program test runner.

   For each programs/NAME.c: compile with the compiler under test, run the
   result, and compare the exit status with the "// expect: N" line in the
   source.  A "// stdout: text" line, if present, must match the output
   exactly.  Usage: run.exe <occ> <directory>. *)

let read_file f = In_channel.with_open_bin f In_channel.input_all

let directive src key =
  let prefix = "// " ^ key ^ ": " in
  List.find_map
    (fun line ->
       if String.starts_with ~prefix line
       then Some (String.sub line (String.length prefix) (String.length line - String.length prefix))
       else None)
    (String.split_on_char '\n' src)

let native_stages =
  match Sys.getenv_opt "OCC_NATIVE" with
  | None | Some "" -> [] | Some s -> String.split_on_char ',' s

type outcome = Pass | Skip of string | Fail of string

let run_one occ dir name =
  let src_file = Filename.concat dir name in
  let src = read_file src_file in
  (* "// requires: cc" skips a test until that stage is native: the test
     exists to pin down behaviour gcc cannot stand in for. *)
  match directive src "requires" with
  | Some stage when not (List.mem stage native_stages) -> Skip ("needs native " ^ stage)
  | _ ->
  let expect = match directive src "expect" with Some n -> int_of_string n | None -> 0 in
  (* Build the executable beside the tests: /tmp may be mounted noexec. *)
  let exe = Filename.temp_file ~temp_dir:(Sys.getcwd ()) "occtest" ".exe" in
  let out = Filename.temp_file "occtest" ".out" in
  (* removed at exit as well, in case the run is interrupted *)
  at_exit (fun () -> List.iter (fun f -> try Sys.remove f with Sys_error _ -> ()) [ exe; out ]);
  let compile = Filename.quote_command occ [ src_file; "-o"; exe ] in
  let result =
    if Sys.command compile <> 0 then Fail "compile failed"
    else
      let status = Sys.command (Filename.quote_command exe [] ~stdout:out) in
      let stdout = read_file out in
      if status <> expect then Fail (Printf.sprintf "exit %d, expected %d" status expect)
      else match directive src "stdout" with
        | Some s when String.trim stdout <> s -> Fail (Printf.sprintf "stdout %S, expected %S" stdout s)
        | _ -> Pass in
  List.iter (fun f -> try Sys.remove f with Sys_error _ -> ()) [ exe; out ];
  result

let () =
  let occ = Sys.argv.(1) and dir = Sys.argv.(2) in
  let occ = if Filename.is_relative occ then Filename.concat (Sys.getcwd ()) occ else occ in
  let tests = Sys.readdir dir |> Array.to_list |> List.filter (fun f -> Filename.check_suffix f ".c") |> List.sort compare in
  let failures = List.filter_map (fun name ->
      match run_one occ dir name with
      | Pass -> Printf.printf "ok    %s\n" name; None
      | Skip why -> Printf.printf "skip  %s (%s)\n" name why; None
      | Fail msg -> Printf.printf "FAIL  %s: %s\n" name msg; Some name) tests in
  Printf.printf "%d tests, %d failures\n" (List.length tests) (List.length failures);
  if failures <> [] then exit 1
