(* The make variable database (GNU make manual, chapters 6 and 8).

   A variable has a value and a flavour: "recursively expanded" (defined
   with =), whose value is expanded anew at each use, or "simply expanded"
   (defined with :=), whose value was expanded once at definition.  The
   origin (file, command line, environment, automatic, default) decides
   which definitions may override which (6.7, "The origin function"). *)

type flavour = Recursive | Simple

type origin =
  | Default        (* a built-in default like CC *)
  | Environment    (* from the environment *)
  | File           (* defined in a makefile *)
  | Command_line   (* VAR=value on the command line *)
  | Override       (* the override directive, or automatic *)
  | Automatic      (* $@, $<, ... set per recipe *)

type variable = {
  mutable value : string;
  mutable flavour : flavour;
  mutable origin : origin;
}

type t = {
  vars : (string, variable) Hashtbl.t;
  mutable pattern_vars : (string * (string * variable)) list;  (* target-pattern, (name, var); newest first *)
}

let create () = { vars = Hashtbl.create 256; pattern_vars = [] }

let find db name = Hashtbl.find_opt db.vars name

(* Command line beats a makefile beats the environment beats a default;
   override and automatic always win (6.7). *)
let rank = function
  | Default -> 0 | Environment -> 1 | File -> 2 | Command_line -> 3 | Override -> 4 | Automatic -> 5

let set db ?(flavour = Recursive) ?(origin = File) name value =
  match Hashtbl.find_opt db.vars name with
  | Some v when rank origin < rank v.origin -> ()   (* a weaker definition does not override *)
  | Some v -> v.value <- value; v.flavour <- flavour; v.origin <- origin
  | None -> Hashtbl.replace db.vars name { value; flavour; origin }

let append db name extra =
  match Hashtbl.find_opt db.vars name with
  | Some v -> v.value <- (if v.value = "" then extra else v.value ^ " " ^ extra)
  | None -> Hashtbl.replace db.vars name { value = extra; flavour = Recursive; origin = File }

let get db name = match Hashtbl.find_opt db.vars name with Some v -> v.value | None -> ""
