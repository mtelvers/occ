(* Aliases (IEEE Std 1003.1-2017, XCU 2.3.1 and the alias utility).

   An alias is a name standing for some text.  Where a command name
   could appear, a word that is an alias name is replaced by that text,
   which is then lexed as though it had been written there.  The
   replacement is not repeated for a name already replaced at that
   position, so `alias ls="ls -l"' is not an infinite regress; and if
   the text ends in a blank, the word after it is checked for an alias
   too, which is what lets `alias e="echo "' expand its own argument.

   Substitution happens while parsing, so an alias defined by a line of
   a script applies to later lines and not to the rest of its own: the
   line was read before the alias existed.  That is the behaviour of the
   shell the build's scripts were written against.

   The table is one per shell.  A subshell is a fork and so gets a copy,
   which is what 2.12 asks for: it may define aliases without the
   parent seeing them. *)

let table : (string, string) Hashtbl.t = Hashtbl.create 8

(* 2.3.1: an alias name may hold underscores, digits, alphabetics and
   any of ! % , @ ; the shell need not accept others, and a name with a
   character that would end a word could never be recognised anyway. *)
let valid_name s =
  s <> ""
  && String.for_all (fun c ->
      Posix.Regex.is_alnum c || c = '_' || c = '!' || c = '%' || c = ',' || c = '@' || c = '.'
      || c = '-' || c = '+' || c = ':' || c = '?' || c = '[' || c = ']' || c = '^' || c = '~')
    s
  (* a name that is all digits would be an IO number, not a word *)
  && not (String.for_all Posix.Regex.is_digit s)

let define name value = Hashtbl.replace table name value
let remove name = Hashtbl.remove table name
let clear () = Hashtbl.reset table
let value name = Hashtbl.find_opt table name
let defined () = Hashtbl.length table > 0

(* by name, so that a listing does not depend on the hash table *)
let all () =
  let out = ref [] in
  Hashtbl.iter (fun k v -> out := (k, v) :: !out) table;
  List.sort (fun (a, _) (b, _) -> compare a b) !out

(* A value written as the shell would have to write it to mean the same
   text again, which is how a listing has to print it (XCU alias).  A
   run of ordinary characters goes in single quotes and a run of single
   quotes in double quotes, so that `echo \'a b\'' comes back as
   'echo '"'"'a b'"'"' -- the form the reference implementation prints. *)
let quote v =
  let b = Buffer.create (String.length v + 2) in
  (* which quote is open: single to begin with, so that an empty value
     prints as '' *)
  let single = ref true in
  Buffer.add_char b '\'';
  String.iter (fun c ->
      let want_single = c <> '\'' in
      if want_single <> !single then begin
        Buffer.add_char b (if !single then '\'' else '"');
        Buffer.add_char b (if want_single then '\'' else '"');
        single := want_single
      end;
      Buffer.add_char b c) v;
  Buffer.add_char b (if !single then '\'' else '"');
  Buffer.contents b
