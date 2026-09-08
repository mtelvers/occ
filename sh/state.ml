(* The shell's execution environment (IEEE Std 1003.1-2017, XCU 2.12).

   2.12 lists what a subshell inherits and what it may change without the
   parent seeing it: open files, the working directory, the variables,
   the traps, the options, the functions.  Everything here is that state,
   in one record, so that a subshell is a fork and nothing else has to be
   arranged.

   Control flow that crosses commands -- `break', `continue', `return'
   and `exit' -- is carried by exceptions, which unwind the redirections
   and the local variables that were saved on the way in. *)

type var = {
  mutable value : string option;      (* None: set but null is "" ; unset is absent *)
  mutable exported : bool;
  mutable readonly : bool;
}

type options = {
  mutable errexit : bool;             (* -e *)
  mutable nounset : bool;             (* -u *)
  mutable xtrace : bool;              (* -x *)
  mutable noglob : bool;              (* -f *)
  mutable verbose : bool;             (* -v *)
  mutable noexec : bool;              (* -n *)
  mutable noclobber : bool;           (* -C *)
  mutable allexport : bool;           (* -a *)
  mutable monitor : bool;             (* -m, accepted and without effect *)
}

type t = {
  vars : (string, var) Hashtbl.t;
  mutable params : string list;        (* the positional parameters *)
  mutable arg0 : string;               (* $0 *)
  mutable funcs : (string * Ast.command) list;
  mutable status : int;                (* $? *)
  opts : options;
  mutable traps : (string * string) list;
  (* one frame per function call, holding what `local' displaced *)
  mutable locals : (string * var option) list list;
  mutable last_bg : int;               (* $! *)
  mutable subshell : bool;
  mutable in_loop : int;
}

exception Return of int
exception Break of int
exception Continue of int
exception Exit_shell of int
exception Error of string             (* a shell error: bad substitution, syntax *)

let create () = {
  vars = Hashtbl.create 64;
  params = [];
  arg0 = "sh";
  funcs = [];
  status = 0;
  opts = { errexit = false; nounset = false; xtrace = false; noglob = false;
           verbose = false; noexec = false; noclobber = false; allexport = false;
           monitor = false };
  traps = [];
  locals = [];
  last_bg = 0;
  subshell = false;
  in_loop = 0;
}

(* ---------- variables ---------- *)

let find st name = Hashtbl.find_opt st.vars name

let get st name =
  match Hashtbl.find_opt st.vars name with
  | Some { value = Some v; _ } -> Some v
  | _ -> None

let get_or st name default = match get st name with Some v -> v | None -> default

let set st ?(export = false) name value =
  match Hashtbl.find_opt st.vars name with
  | Some v when v.readonly -> raise (Error (name ^ ": is read only"))
  | Some v -> v.value <- Some value; if export || st.opts.allexport then v.exported <- true
  | None ->
      Hashtbl.replace st.vars name
        { value = Some value; exported = export || st.opts.allexport; readonly = false }

let unset st name =
  match Hashtbl.find_opt st.vars name with
  | Some v when v.readonly -> raise (Error (name ^ ": is read only"))
  | _ -> Hashtbl.remove st.vars name

let export st name =
  match Hashtbl.find_opt st.vars name with
  | Some v -> v.exported <- true
  | None -> Hashtbl.replace st.vars name { value = None; exported = true; readonly = false }

let readonly st name =
  match Hashtbl.find_opt st.vars name with
  | Some v -> v.readonly <- true
  | None -> Hashtbl.replace st.vars name { value = None; exported = false; readonly = true }

(* the environment a command is started with (2.12): the exported
   variables that have a value *)
let environment st =
  let out = ref [] in
  Hashtbl.iter (fun name v ->
      match v.value with
      | Some value when v.exported -> out := (name ^ "=" ^ value) :: !out
      | _ -> ()) st.vars;
  Array.of_list (List.sort compare !out)

(* IFS decides field splitting; unset means the default (2.5.3) *)
let ifs st = get_or st "IFS" " \t\n"

(* ---------- the shell's own parameters ---------- *)

let positional st n =
  match List.nth_opt st.params (n - 1) with Some v -> Some v | None -> None

let import_environment st =
  Array.iter (fun kv ->
      match String.index_opt kv '=' with
      | Some i ->
          Hashtbl.replace st.vars (String.sub kv 0 i)
            { value = Some (String.sub kv (i + 1) (String.length kv - i - 1));
              exported = true; readonly = false }
      | None -> ()) (Unix.environment ())

(* ---------- signals and traps ---------- *)

(* the signals a script may name, by the names of XCU 1.4 kill and by
   number; 0 and EXIT are the exit trap *)
let signals = [
  "EXIT", 0; "HUP", 1; "INT", 2; "QUIT", 3; "ILL", 4; "TRAP", 5; "ABRT", 6;
  "BUS", 7; "FPE", 8; "KILL", 9; "USR1", 10; "SEGV", 11; "USR2", 12;
  "PIPE", 13; "ALRM", 14; "TERM", 15; "CHLD", 17; "CONT", 18; "STOP", 19;
  "TSTP", 20; "TTIN", 21; "TTOU", 22;
]

let signal_name s =
  let s = String.uppercase_ascii s in
  let s = if String.length s > 3 && String.sub s 0 3 = "SIG" then String.sub s 3 (String.length s - 3) else s in
  match int_of_string_opt s with
  | Some n -> (match List.find_opt (fun (_, k) -> k = n) signals with
      | Some (name, _) -> Some name
      | None -> None)
  | None -> if List.mem_assoc s signals then Some s else None

let signal_number name = List.assoc name signals

(* Signals arriving while a command runs are noted here and acted on
   between commands, as 2.11 requires: a trap runs after the command it
   interrupted, not in the middle of the shell's own work. *)
let pending : (string, unit) Hashtbl.t = Hashtbl.create 8

let trap_of st name = List.assoc_opt name st.traps

let set_trap st name action =
  st.traps <- (name, action) :: List.remove_assoc name st.traps

let clear_trap st name = st.traps <- List.remove_assoc name st.traps
