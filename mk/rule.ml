(* Rules and the rule database (GNU make manual, chapters 2, 4, 10).

   An explicit rule names its targets; a pattern rule (targets contain
   '%') is a template applied to any target whose name matches.  A recipe
   is the list of command lines run to update the targets.  Prerequisites
   are ordinary (their being newer forces a rebuild) or order-only (they
   must exist but their time is ignored, after a '|'). *)

type rule = {
  targets : string list;
  prereqs : string list;
  order_only : string list;
  recipe : string list;         (* command lines, unexpanded *)
  is_pattern : bool;
  is_double_colon : bool;
  phony : bool;                 (* named in .PHONY *)
}

(* a target-specific variable (6.11) or pattern-specific one (6.12):
   the target or pattern it applies to, the variable, the operator and
   the unexpanded value *)
type tsv = { pat : string; var : string; op : string; rhs : string }

type t = {
  mutable explicit : rule list;     (* in file order *)
  mutable patterns : rule list;     (* pattern rules, in file order *)
  mutable tsvs : tsv list;          (* in file order *)
  mutable second_expansion : bool;  (* .SECONDEXPANSION: re-expand prerequisites at build time *)
  phony : (string, unit) Hashtbl.t;
  by_target : (string, rule) Hashtbl.t;   (* first explicit rule defining each target *)
}

let create () = { explicit = []; patterns = []; tsvs = []; second_expansion = false; phony = Hashtbl.create 64; by_target = Hashtbl.create 256 }

let add db (r : rule) =
  if r.is_pattern then db.patterns <- db.patterns @ [ r ]
  else begin
    db.explicit <- db.explicit @ [ r ];
    List.iter (fun t -> if not (Hashtbl.mem db.by_target t) then Hashtbl.replace db.by_target t r) r.targets
  end

let mark_phony db name = Hashtbl.replace db.phony name ()
let is_phony db name = Hashtbl.mem db.phony name
