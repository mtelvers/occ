(* The utilities occutils holds, with the options each accepts.

   Every entry gives the name a script calls, the option spec in the form
   Posix.Getopt reads (a letter, with ':' after it if it takes an
   argument), and the implementation, which is handed the whole argument
   vector, the options that were found and the operands that were left.

   A utility whose operands may themselves begin with '-' cannot let
   options be read from among them: `printf' and `echo' take their
   arguments exactly as given, so they are marked as taking none. *)

type run = string array -> Posix.Getopt.opt list -> string list -> int

type entry = {
  name : string;
  shorts : string;
  long : (string * bool) list;
  permute : bool;            (* read options that follow an operand *)
  run : run;
}

let e ?(long = []) ?(permute = true) name shorts run = { name; shorts; long; permute; run }

let utilities : entry list = [
  (* text *)
  e "cat" "uvetA" Textio.cat;
  e "tee" "ai" Textio.tee;
  (* the digits are the obsolete `head -5' form, which the utility itself
     turns into -n 5; they have to be in the spec or option parsing
     rejects them first *)
  e "head" "n:c:qv0123456789" Textio.head;
  e "tail" "n:c:qvf0123456789" Textio.tail;
  e "wc" "clmwL" Textio.wc;
  e "cut" "b:c:f:d:sn" Textio.cut;
  e "tr" "cCdst" Textio.tr;
  e "sort" ~long:[ "help", false ] "bcdfimnrusz:t:k:o:" Textio.sort;
  e "uniq" "cdiuf:s:w:" Textio.uniq;
  e "cmp" "ls" Textio.cmp;
  e "grep" ~long:[ "quiet", false; "silent", false; "color", true ]
    "EFGce:f:HhilnoqsvwxaP" Grep.main_opts;
  e "sed" ~long:[ "expression", true; "quiet", false; "silent", false;
                  "in-place", false; "regexp-extended", false; "separate", false ]
    "nEre:f:i:sz" Sed.main;
  e "awk" "F:v:f:" Awk.main;
  e "diff" ~long:[ "quiet", false; "brief", false; "unified", false;
                   "ignore-all-space", false; "exit-code", false;
                   "color", true; "new-file", false ]
    "uqbBwiEcNraU:" Diff.main;

  (* the file hierarchy *)
  e "rm" "firRv" Files.rm;
  e "cp" "afiprRLPdv" Files.cp;
  e "mv" "finv" Files.mv;
  e "mkdir" "pm:v" Files.mkdir;
  e "rmdir" "pv" Files.rmdir;
  e "ln" "sfnv" Files.ln;
  e "touch" "acmr:t:d:" Files.touch;
  e "chmod" "Rfv" Files.chmod;
  e "mktemp" "dqup:t" Files.mktemp;

  (* names and the machine *)
  e "basename" ~permute:false "as:z" Paths.basename;
  e "dirname" ~permute:false "z" Paths.dirname;
  e "realpath" "eqmszLP" Paths.realpath;
  e "pwd" "LP" Paths.pwd;
  e "env" ~permute:false "iu:0" Misc.env;
  e "which" "a" Misc.which;
  e "uname" "asnrvmpio" Misc.uname;
  e "hostname" "sfi" Misc.hostname;
  e "ls" "laAdiLR1FtrS" Misc.ls;
  e "expr" ~permute:false "" Misc.expr;
  e "sleep" "" Misc.sleep;
  e "find" ~permute:false "HLPdsx" Find.main;
  e "xargs" "0rtn:I:s:" Xargs.main;

  (* the ones the shell also has as built-ins *)
  e "echo" ~permute:false "" Paths.echo;
  e "printf" ~permute:false "" Paths.printf;
  e "test" ~permute:false "" Paths.test;
  e "[" ~permute:false "" Paths.test;
  e "true" ~permute:false "" Paths.true_;
  e "false" ~permute:false "" Paths.false_;
]

let find name = List.find_opt (fun u -> u.name = name) utilities

let names () = List.map (fun u -> u.name) utilities

(* Run one utility: parse its options, then hand it the pieces.  An
   unknown option is reported the way the other utilities report it, and
   is an error above 1 (XCU 1.4). *)
let run entry argv =
  Util.prog := entry.name;
  let spec = Posix.Getopt.spec ~long:entry.long entry.shorts in
  match Posix.Getopt.parse ~permute:entry.permute spec
          (Array.sub argv 1 (Array.length argv - 1)) with
  | (opts, operands) -> entry.run argv opts operands
  | exception Posix.Getopt.Error msg ->
      Printf.eprintf "%s: %s\n" entry.name msg;
      2
