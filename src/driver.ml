type stage = Preprocess | Compile | Assemble | Link
type mode = Native | Delegate

(* Every stage is native by default: preprocessing, compilation, assembly
   and (static) linking; shared objects are still gcc's job.  OCC_NATIVE
   overrides the set ("none" delegates everything, for use as a pure
   wrapper). *)
let native_stages =
  match Sys.getenv_opt "OCC_NATIVE" with
  | None | Some "" -> [ "pp"; "cc"; "as"; "ld" ]
  | Some "none" -> []
  | Some s -> String.split_on_char ',' s

let mode stage =
  let key = match stage with
    | Preprocess -> "pp" | Compile -> "cc" | Assemble -> "as" | Link -> "ld" in
  if List.mem key native_stages then Native else Delegate

let delegate_cc = Option.value (Sys.getenv_opt "OCC_CC") ~default:"gcc"

(* The include directory holding our stdarg.h, stdatomic.h and friends
   (C11 7.16, 7.17, 7.19).  Found relative to the executable so a build
   tree works without installation. *)
let include_dir =
  let exe = Sys.executable_name in
  let rec up d n = if n = 0 then d else up (Filename.dirname d) (n - 1) in
  let candidates = [
    Filename.concat (up exe 3) "include";   (* _build/default/bin/main.exe *)
    Filename.concat (up exe 4) "include";   (* _build/default/bin/.main.eobjs/... *)
    Filename.concat (up exe 2) "share/occ/include";
  ] in
  List.find_opt (fun d -> Sys.file_exists (Filename.concat d "stdatomic.h")) candidates

(* ---- Command line ------------------------------------------------------- *)

type input_kind = C | Preprocessed | Asm | Asm_cpp | Object | Other

let kind_of_file f =
  match Filename.extension f with
  | ".c" -> C | ".i" -> Preprocessed | ".s" -> Asm | ".S" -> Asm_cpp
  | ".o" | ".a" | ".so" -> Object | _ -> Other

type options = {
  mutable stop_after : stage;
  mutable output : string option;
  mutable inputs : string list;         (* files, in order *)
  mutable link_args : string list;      (* -l, -L, -Wl,..., in order with inputs *)
  mutable cpp : Preprocess.config;
  mutable passthrough : string list;    (* -f, -W, -m, -O, -g, -std, -pthread ... *)
  mutable verbose : bool;
  mutable dump : string option;         (* --dump=tokens|ast|typed|ir|asm *)
  mutable pic : bool;                    (* -fPIC / -fpic: position-independent code *)
  mutable debug : bool;                  (* -g: DWARF line tables and symbols *)
  mutable deps : bool;                   (* -MD / -MMD: write a make dependency file *)
  mutable deps_file : string option;     (* -MF *)
  mutable deps_target : string option;   (* -MT *)
}

let usage = "usage: occ [gcc-compatible options] files...\n\
             environment: OCC_NATIVE=pp,cc,as,ld selects native stages (default all; \"none\" delegates all); OCC_CC=gcc\n"

let parse_args argv =
  let o = {
    stop_after = Link; output = None; inputs = []; link_args = [];
    cpp = { Preprocess.include_dirs = []; system_dirs = []; defines = []; undefines = []; includes = []; line_markers = true; assembler = false };
    passthrough = []; verbose = false; dump = None; pic = false; debug = false; deps = false; deps_file = None; deps_target = None } in
  let n = Array.length argv in
  let i = ref 1 in
  let next flag = incr i; if !i >= n then (prerr_string ("occ: missing argument to " ^ flag ^ "\n"); exit 1); argv.(!i) in
  (* Options that take a separate argument and go straight to gcc. *)
  let with_arg_passthrough = [ "-MQ"; "-x"; "-Xlinker"; "-Xassembler"; "-Xpreprocessor"; "-idirafter"; "-iquote" ] in
  while !i < n do
    let a = argv.(!i) in
    let split prefix = String.sub a (String.length prefix) (String.length a - String.length prefix) in
    (match a with
     | "-E" -> o.stop_after <- Preprocess
     | "-S" -> o.stop_after <- Compile
     | "-c" -> o.stop_after <- Assemble
     | "-o" -> o.output <- Some (next a)
     | "-v" -> o.verbose <- true
     | "-fPIC" | "-fpic" | "-fPIE" | "-fpie" -> o.pic <- true; o.passthrough <- o.passthrough @ [ a ]
     | "-MD" | "-MMD" -> o.deps <- true; o.passthrough <- o.passthrough @ [ a ]
     | "-g" | "-g1" | "-g2" | "-g3" | "-ggdb" | "-gdwarf-4" | "-gdwarf-5" -> o.debug <- true; o.passthrough <- o.passthrough @ [ a ]
     | "-g0" -> o.debug <- false; o.passthrough <- o.passthrough @ [ a ]
     | "-MF" -> let f = next a in o.deps_file <- Some f; o.passthrough <- o.passthrough @ [ a; f ]
     | "-MT" -> let t = next a in o.deps_target <- Some t; o.passthrough <- o.passthrough @ [ a; t ]
     | _ when String.length a > 7 && String.sub a 0 7 = "--dump=" -> o.dump <- Some (split "--dump="); o.stop_after <- Assemble
     | "--version" -> print_string "occ 0.1 (C11, x86-64 Linux; native preprocess, compile, assemble; gcc links)\n"; exit 0
     | "-I" -> o.cpp <- { o.cpp with include_dirs = o.cpp.include_dirs @ [ next a ] }
     | "-isystem" -> o.cpp <- { o.cpp with system_dirs = o.cpp.system_dirs @ [ next a ] }
     | "-D" -> let d = next a in o.cpp <- { o.cpp with defines = o.cpp.defines @ [ (match String.index_opt d '=' with None -> d, None | Some k -> String.sub d 0 k, Some (String.sub d (k+1) (String.length d - k - 1))) ] }
     | "-U" -> o.cpp <- { o.cpp with undefines = o.cpp.undefines @ [ next a ] }
     | "-include" -> let f = next a in o.cpp <- { o.cpp with includes = o.cpp.includes @ [ f ] }; o.passthrough <- o.passthrough @ [ a; f ]
     | "-nostdinc" -> o.passthrough <- o.passthrough @ [ a ]
     | "-P" -> o.cpp <- { o.cpp with line_markers = false }; o.passthrough <- o.passthrough @ [ a ]
     | "-l" | "-L" -> o.link_args <- o.link_args @ [ a; next a ]
     | _ when List.mem a with_arg_passthrough -> o.passthrough <- o.passthrough @ [ a; next a ]
     | _ when String.length a > 2 && String.sub a 0 2 = "-I" -> o.cpp <- { o.cpp with include_dirs = o.cpp.include_dirs @ [ split "-I" ] }
     | _ when String.length a > 2 && String.sub a 0 2 = "-D" -> let d = split "-D" in o.cpp <- { o.cpp with defines = o.cpp.defines @ [ (match String.index_opt d '=' with None -> d, None | Some k -> String.sub d 0 k, Some (String.sub d (k+1) (String.length d - k - 1))) ] }
     | _ when String.length a > 2 && String.sub a 0 2 = "-U" -> o.cpp <- { o.cpp with undefines = o.cpp.undefines @ [ split "-U" ] }
     | _ when String.length a > 2 && (String.sub a 0 2 = "-l" || String.sub a 0 2 = "-L") -> o.link_args <- o.link_args @ [ a ]
     | _ when String.length a > 4 && String.sub a 0 4 = "-Wl," -> o.link_args <- o.link_args @ [ a ]
     | _ when a = "-shared" || a = "-static" || a = "-rdynamic" || a = "-pie" || a = "-no-pie" || a = "-nostdlib" || a = "-nostartfiles" -> o.link_args <- o.link_args @ [ a ]
     | _ when String.length a > 0 && a.[0] = '-' -> o.passthrough <- o.passthrough @ [ a ]
     | _ ->
         (match kind_of_file a with
          | Object | Other -> o.link_args <- o.link_args @ [ a ]
          | _ -> o.inputs <- o.inputs @ [ a ]));
    incr i
  done;
  o

(* ---- Running things ----------------------------------------------------- *)

let run o prog args =
  if o.verbose then prerr_endline (String.concat " " (prog :: args));
  match Sys.command (Filename.quote_command prog args) with
  | 0 -> ()
  | code -> exit code

(* Temporary files are removed however the process ends: after a
   successful run, after an error, after a --dump, or when a delegated
   command fails and its exit code is passed on. *)
let temp_files = ref []
let cleanup () = List.iter (fun f -> try Sys.remove f with Sys_error _ -> ()) !temp_files; temp_files := []
let () = at_exit cleanup
let temp suffix =
  let f = Filename.temp_file "occ" suffix in
  temp_files := f :: !temp_files; f

let cpp_flags (c : Preprocess.config) =
  List.concat_map (fun d -> [ "-I"; d ]) c.include_dirs
  @ List.concat_map (fun d -> [ "-isystem"; d ]) c.system_dirs
  @ List.map (fun (k, v) -> match v with None -> "-D" ^ k | Some v -> "-D" ^ k ^ "=" ^ v) c.defines
  @ List.map (fun u -> "-U" ^ u) c.undefines

(* Stage implementations.  Each takes an input path and an output path. *)

let preprocess o input output =
  match mode Preprocess with
  | Delegate ->
      (* The dialect of the preprocessed text is chosen by whoever compiles
         it.  When our compiler does, __GNUC__ is undefined so glibc takes
         its portable paths, and our headers replace gcc's; this is the
         same input tools/mkcorpus.sh produces.  When gcc compiles it too,
         it must see its own headers: glibc's non-GNU paths typedef names
         such as _Float32 that gcc's parser reserves. *)
      let dialect =
        match mode Compile, include_dir with
        | Native, Some d ->
            [ "-U__GNUC__"; "-U__SIZEOF_INT128__"; "-U_FORTIFY_SOURCE" ] @ Preprocess.predefined_extras @ [ "-nostdinc"; "-I"; d;
              "-isystem"; "/usr/include/x86_64-linux-gnu"; "-isystem"; "/usr/include" ]
        | Native, None -> failwith "cannot find include/ next to the executable"
        | Delegate, _ -> [] in
      run o delegate_cc ([ "-E" ] @ dialect @ cpp_flags o.cpp @ o.passthrough @ [ input; "-o"; output ])
  | Native ->
      (* our headers first, then the C library's; -nostdinc drops the defaults *)
      let defaults =
        if List.mem "-nostdinc" o.passthrough then []
        else match include_dir with
          | Some d -> [ d; "/usr/include/x86_64-linux-gnu"; "/usr/include" ]
          | None -> failwith "cannot find include/ next to the executable" in
      let extras = List.map (fun d ->
          let d = String.sub d 2 (String.length d - 2) in
          match String.index_opt d '=' with
          | None -> d, None
          | Some k -> String.sub d 0 k, Some (String.sub d (k + 1) (String.length d - k - 1))) Preprocess.predefined_extras in
      (* Like gcc on this platform, code is position-independent: -fPIC
         defines __PIC__, and the default (PIE-compatible) code defines
         __PIE__ as well; the runtime's amd64.S chooses GOT addressing by
         these. *)
      let pic = [ "__PIC__", Some "2"; "__pic__", Some "2" ] @ (if o.pic then [] else [ "__PIE__", Some "2"; "__pie__", Some "2" ]) in
      (* gcc's -pthread also defines _REENTRANT, which configure scripts test *)
      let pthread = if List.mem "-pthread" o.passthrough then [ "_REENTRANT", Some "1" ] else [] in
      let cfg = { o.cpp with Preprocess.system_dirs = o.cpp.system_dirs @ defaults; defines = extras @ pic @ pthread @ o.cpp.defines } in
      let text, included = Preprocess.run cfg input in
      Out_channel.with_open_bin output (fun oc -> output_string oc text);
      (* -MMD: a make rule listing the headers this unit depends on *)
      if o.deps then begin
        let target = match o.deps_target, o.output with
          | Some t, _ -> t
          | None, Some f -> f
          | None, None -> Filename.remove_extension (Filename.basename input) ^ ".o" in
        let file = match o.deps_file with Some f -> f | None -> Filename.remove_extension target ^ ".d" in
        Out_channel.with_open_bin file (fun oc ->
            Printf.fprintf oc "%s: %s%s\n" target input (String.concat "" (List.map (fun f -> " \\\n " ^ f) included)))
      end

let compile o ~src input output =
  match mode Compile with
  | Delegate -> run o delegate_cc ([ "-S"; "-fno-asynchronous-unwind-tables" ] @ o.passthrough @ [ "-x"; "cpp-output"; input; "-o"; output ])
  | Native ->
      let source = In_channel.with_open_bin input In_channel.input_all in
      let ppf = Format.std_formatter in
      let dump name f x = if o.dump = Some name then (f ppf x; Format.pp_print_flush ppf (); exit 0) else x in
      let asm =
        Lexer.tokenize ~file:input source
        |> dump "tokens" (fun ppf toks ->
               List.iter (fun { Token.tok; loc } -> Format.fprintf ppf "%a: %a@." Loc.pp loc Token.pp tok) toks)
        |> Parser.parse
        |> dump "ast" Syntax_print.translation_unit
        |> Elab.translation_unit
        |> dump "typed" (fun ppf ((_, tu) : Env.t * Typed.translation_unit) -> Typed_print.translation_unit ppf tu)
        |> (fun (env, tu) -> Lower.program ~source:src env tu)
        |> dump "ir" Ir_print.program
        |> Select.program ~pic:o.pic ~debug:o.debug (* register allocation happens inside *)
        |> dump "asm" Emit.program in
      Out_channel.with_open_bin output (fun oc ->
          let ppf = Format.formatter_of_out_channel oc in
          Emit.program ppf asm; Format.pp_print_flush ppf ())

let assemble o input output =
  match mode Assemble with
  | Delegate -> run o delegate_cc ([ "-c" ] @ o.passthrough @ [ input; "-o"; output ])
  | Native ->
      if o.verbose then Printf.eprintf "occas %s -o %s\n" input output;
      Assemble.files [ input ] output

(* Where the C runtime's start files and static libraries live.  The
   native linker is static, so it links crt1.o, crti.o, crtbeginT.o, the
   objects, then libgcc, libgcc_eh and libc, and crtend.o, crtn.o, the way
   gcc -static does.  Shared objects are still gcc's job. *)
let system_lib_dirs () =
  let gcc_dirs =
    let root = "/usr/lib/gcc/x86_64-linux-gnu" in
    if Sys.file_exists root && Sys.is_directory root then
      List.map (Filename.concat root) (List.sort (fun a b -> compare (int_of_string_opt b) (int_of_string_opt a)) (Array.to_list (Sys.readdir root)))
    else [] in
  gcc_dirs @ [ "/usr/lib/x86_64-linux-gnu"; "/lib/x86_64-linux-gnu"; "/usr/lib64"; "/usr/lib" ]

let find_file dirs name =
  match List.find_opt (fun d -> Sys.file_exists (Filename.concat d name)) dirs with
  | Some d -> Filename.concat d name
  | None -> failwith ("cannot find " ^ name)

let link o objects output =
  match mode Link with
  | Delegate -> run o delegate_cc (o.passthrough @ objects @ o.link_args @ [ "-o"; output ])
  | Native when List.mem "-shared" o.passthrough -> run o delegate_cc (o.passthrough @ objects @ o.link_args @ [ "-o"; output ])
  | Native ->
      let user_dirs = List.filter_map (fun a ->
          if String.length a > 2 && String.sub a 0 2 = "-L" then Some (String.sub a 2 (String.length a - 2)) else None) o.link_args in
      let search = user_dirs @ system_lib_dirs () in
      let items = List.filter_map (fun a ->
          if String.length a > 2 && String.sub a 0 2 = "-l" then Some (Link.Library (String.sub a 2 (String.length a - 2)))
          else if Filename.check_suffix a ".a" then Some (Link.Archive a)
          else if Filename.check_suffix a ".o" then Some (Link.Object a)
          else None) (objects @ o.link_args) in
      let crt name = Link.Object (find_file search name) in
      let items = [ crt "crt1.o"; crt "crti.o"; crt "crtbeginT.o" ] @ items
                  @ [ Link.Library "gcc"; Link.Library "gcc_eh"; Link.Library "c"; crt "crtend.o"; crt "crtn.o" ] in
      if o.verbose then prerr_endline ("occld -o " ^ output);
      Link.link ~output ~entry:"_start" ~search items

(* ---- Main --------------------------------------------------------------- *)

let default_output input stage =
  let base = Filename.remove_extension (Filename.basename input) in
  match stage with
  | Preprocess -> "-" | Compile -> base ^ ".s" | Assemble -> base ^ ".o" | Link -> "a.out"

let main argv =
  let o = parse_args argv in
  if o.inputs = [] && o.link_args = [] then (prerr_string usage; exit 1);
  let single = List.length o.inputs = 1 in
  let final_for input stage =
    match o.output with
    | Some f when single && stage = o.stop_after -> f
    | _ -> default_output input stage in
  let objects =
    List.filter_map (fun input ->
        let kind = kind_of_file input in
        (* Assembly with cpp directives needs both a native preprocessor and
           a native assembler; otherwise gcc does the whole job. *)
        if kind = Asm_cpp && (mode Preprocess = Delegate || mode Assemble = Delegate) then begin
          let out = if o.stop_after = Link then temp ".o" else final_for input Assemble in
          run o delegate_cc ([ "-c" ] @ cpp_flags o.cpp @ o.passthrough @ [ input; "-o"; out ]);
          if o.stop_after = Link then Some out else None
        end else begin
          let i_file =
            if kind = C || kind = Asm_cpp then begin
              let out = if o.stop_after = Preprocess then final_for input Preprocess else temp (if kind = C then ".i" else ".s") in
              let saved = o.cpp in
              (* as gcc does for .S: define __ASSEMBLER__ and leave out line markers *)
              if kind = Asm_cpp then o.cpp <- { o.cpp with line_markers = false; assembler = true; defines = ("__ASSEMBLER__", Some "1") :: o.cpp.defines };
              let result =
                if out = "-" then begin
                  let t = temp ".i" in preprocess o input t;
                  print_string (In_channel.with_open_bin t In_channel.input_all); None
                end else (preprocess o input out; Some out) in
              o.cpp <- saved;
              result
            end else if kind = Preprocessed then Some input else None in
          if o.stop_after = Preprocess then None else
          let s_file =
            match i_file with
            | Some i when kind = Asm_cpp -> Some i
            | Some i ->
                let out = if o.stop_after = Compile then final_for input Compile else temp ".s" in
                compile o ~src:input i out; Some out
            | None -> if kind = Asm then Some input else None in
          if o.stop_after = Compile then None else
          match s_file with
          | Some s ->
              let out = if o.stop_after = Assemble then final_for input Assemble else temp ".o" in
              assemble o s out;
              if o.stop_after = Assemble then None else Some out
          | None -> None
        end)
      o.inputs in
  if o.stop_after = Link then
    link o objects (Option.value o.output ~default:"a.out");
  cleanup ();
  0

let main argv =
  try main argv with
  | Diag.Error (loc, msg) -> Diag.report loc msg; cleanup (); 1
  | Failure msg -> prerr_endline ("occ: " ^ msg); cleanup (); 1
