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
  (* where each recipe line was written, "file:line", as make names it
     when a recipe fails *)
  recipe_loc : string list;
  is_pattern : bool;
  is_double_colon : bool;
  phony : bool;                 (* named in .PHONY *)
  (* The stem, for the explicit rules a static pattern rule is expanded
     into (4.12): they are ordinary rules, but $* in the recipe still
     means the part the pattern matched. *)
  stem : string;
}

(* a target-specific variable (6.11) or pattern-specific one (6.12):
   the target or pattern it applies to, the variable, the operator and
   the unexpanded value *)
type tsv = { pat : string; var : string; op : string; rhs : string }

(* a static pattern rule while its recipe is being collected: the target
   pattern, the prerequisite patterns and the order-only ones, and the
   targets it names (4.12) *)
type static = {
  tpat : string;
  ppats : string list;
  opats : string list;
  stargets : string list;
}

type t = {
  mutable explicit : rule list;     (* in file order *)
  mutable patterns : rule list;     (* pattern rules, in file order *)
  mutable tsvs : tsv list;          (* in file order *)
  mutable second_expansion : bool;  (* .SECONDEXPANSION: re-expand prerequisites at build time *)
  (* `vpath pattern dirs' (4.5.2): where to look for a file whose name
     matches the pattern and which is not in the current directory *)
  mutable vpaths : (string * string list) list;
  phony : (string, unit) Hashtbl.t;
  (* .PRECIOUS and .SECONDARY name files make must not delete, either as
     an intermediate in a chain or when a recipe fails (10.4, 10.5.5) *)
  mutable precious : string list;
  mutable notparallel : bool;              (* .NOTPARALLEL: run one job at a time *)
  (* .DELETE_ON_ERROR: a target whose recipe failed is removed, since
     what is left of it is not the file the rule promised *)
  mutable delete_on_error : bool;
  (* The rules that make each target.  A target written with one colon
     has exactly one entry, its rules merged; a target written with two
     has one entry per rule, since 4.13 makes each of those independent
     and runs every one of them. *)
  by_target : (string, rule list) Hashtbl.t;
}

let create () = { explicit = []; patterns = []; tsvs = []; second_expansion = false;
                  vpaths = []; precious = []; notparallel = false;
                  delete_on_error = false;
                  phony = Hashtbl.create 64;
                  by_target = Hashtbl.create 256 }

(* A file may be named by several rules: the prerequisites add up, and at
   most one of the rules may carry a recipe (4.11).  The build's
   `runtime-all' is written that way, gathering its prerequisites over
   three rules, and a make that kept only the first would silently build
   less than it was asked to.  Double-colon rules are the exception:
   each is independent and has its own recipe. *)
let add db (r : rule) =
  if r.is_pattern then begin
    (* A pattern rule with the same target and prerequisite patterns as
       one already given replaces it, and one with no recipe cancels it
       and is not kept (10.5.6).  Makefile.common opens with a recipeless
       `%.o: %.c' to cancel make's own built-in rule, and a real one is
       given later; a make that kept the first would find no recipe. *)
    let same (x : rule) = x.targets = r.targets && x.prereqs = r.prereqs in
    db.patterns <- List.filter (fun x -> not (same x)) db.patterns;
    if r.recipe <> [] then db.patterns <- db.patterns @ [ r ]
  end
  else begin
    db.explicit <- db.explicit @ [ r ];
    List.iter (fun t ->
        let existing = match Hashtbl.find_opt db.by_target t with Some l -> l | None -> [] in
        if r.is_double_colon || List.exists (fun (x : rule) -> x.is_double_colon) existing
        then Hashtbl.replace db.by_target t (existing @ [ r ])
        else
          (* A file may be named by several one-colon rules: the
             prerequisites add up, and at most one of the rules may carry
             a recipe (4.11).  The build's `runtime-all' is written that
             way, gathering its prerequisites over three rules, and a make
             that kept only the first would silently build less than it
             was asked to.  The rule with the recipe contributes its
             prerequisites first, so that $< is the one it names. *)
          match existing with
          | [] -> Hashtbl.replace db.by_target t [ r ]
          | [ e ] ->
              if e.recipe <> [] && r.recipe <> [] then
                Printf.eprintf "warning: overriding recipe for target '%s'\n" t;
              let first, second = if r.recipe <> [] && e.recipe = [] then r, e else e, r in
              Hashtbl.replace db.by_target t
                [ { e with
                    prereqs = first.prereqs @ second.prereqs;
                    order_only = first.order_only @ second.order_only;
                    recipe = (if r.recipe <> [] then r.recipe else e.recipe);
                    stem = (if first.stem <> "" then first.stem else second.stem);
                    phony = e.phony || r.phony } ]
          | l -> Hashtbl.replace db.by_target t (l @ [ r ]))
      r.targets
  end

(* the rules that make [t], in the order they were written *)
let rules_for db t = match Hashtbl.find_opt db.by_target t with Some l -> l | None -> []

let has_rule db t = Hashtbl.mem db.by_target t

let mark_phony db name = Hashtbl.replace db.phony name ()
let is_phony db name = Hashtbl.mem db.phony name

let mark_precious db name = db.precious <- name :: db.precious

(* A name is precious if it was listed, or if a pattern that was listed
   matches it: the build writes `.PRECIOUS: $(DEPDIR)/%'. *)
let is_precious db name =
  List.exists (fun p ->
      if String.contains p '%' then Func.pattern_match p name <> None else p = name)
    db.precious
