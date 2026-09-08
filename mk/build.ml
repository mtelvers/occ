(* The build algorithm (GNU make manual, chapters 2 and 10).

   To update a goal, make first updates every prerequisite, then rebuilds
   the goal if it does not exist or is older than any prerequisite.  A
   target with no explicit recipe is matched against the pattern rules
   (implicit rules, 10.5); the first pattern whose prerequisites can
   themselves be made is used.  The recipe runs one line per shell, with
   the automatic variables (target, first and all prerequisites,
   the newer ones, and the stem) bound. *)

type t = {
  db : Value.t;
  rules : Rule.t;
  mutable building : (string, unit) Hashtbl.t;  (* cycle guard *)
  mutable done_ : (string, bool) Hashtbl.t;      (* target -> whether it was rebuilt *)
  keep_going : bool;
  dry_run : bool;
  silent : bool;
  question : bool;
  mutable failed : bool;
}

let mtime f = match Unix.stat f with s -> Some s.Unix.st_mtime | exception _ -> None
let exists f = Sys.file_exists f

(* how a target will be built: an explicit rule, a matched pattern rule
   (with its stem), or nothing *)
type how =
  | Explicit of Rule.rule
  | Implicit of Rule.rule * string   (* the pattern rule and the stem *)
  | Source                           (* a file with no rule; must already exist *)

let starts_with p s = String.length s >= String.length p && String.sub s 0 (String.length p) = p

(* find an applicable pattern rule whose prerequisites exist or can be made *)
let find_implicit b t =
  let candidates = List.filter_map (fun (r : Rule.rule) ->
      match List.find_map (fun tp -> Option.map (fun stem -> tp, stem) (Func.pattern_match tp t)) r.targets with
      | Some (_, stem) -> Some (r, stem)
      | None -> None) b.rules.Rule.patterns in
  (* prefer a rule all of whose prerequisites can be provided *)
  List.find_opt (fun ((r : Rule.rule), stem) ->
      List.for_all (fun p ->
          let p = Func.pattern_subst p stem in
          exists p || Hashtbl.mem b.rules.Rule.by_target p) r.prereqs) candidates


let how b t =
  match Hashtbl.find_opt b.rules.Rule.by_target t with
  | Some r when r.recipe <> [] -> Explicit r
  | Some r ->
      (* a rule with prerequisites but no recipe (as the .dep files give
         each object): take the recipe from a matching pattern rule and
         keep the explicit prerequisites (2.4, 4.14, 10.5.5) *)
      (match find_implicit b t with
       | Some (pr, stem) ->
           let stemmed = List.map (fun p -> Func.pattern_subst p stem) pr.prereqs in
           Implicit ({ pr with prereqs = r.prereqs @ stemmed; order_only = r.order_only @ pr.order_only }, stem)
       | None -> Explicit r)
  | None ->
      (match find_implicit b t with
       | Some (pr, stem) -> Implicit ({ pr with prereqs = List.map (fun p -> Func.pattern_subst p stem) pr.prereqs }, stem)
       | None -> Source)

(* Bind the target- and pattern-specific variables that apply to [t]
   (6.11, 6.12), returning a closure that restores the previous values.
   The bindings are applied in file order so a later one wins. *)
let apply_tsv b t =
  let matching = List.filter (fun (v : Rule.tsv) ->
      if String.contains v.pat '%' then Func.pattern_match v.pat t <> None else v.pat = t) b.rules.Rule.tsvs in
  let saved = List.filter_map (fun (v : Rule.tsv) ->
      let prev = Value.find b.db v.var in
      let snapshot = Option.map (fun (x : Value.variable) -> { x with Value.value = x.value }) prev in
      (match v.op with
       | "=" -> Value.set b.db ~origin:Value.Override ~flavour:Value.Recursive v.var v.rhs
       | ":=" | "::=" -> Value.set b.db ~origin:Value.Override ~flavour:Value.Simple v.var (Expand.expand { Expand.db = b.db; call_stack = []; expanding = Hashtbl.create 16 } v.rhs)
       | "+=" -> Value.append b.db v.var (match prev with Some { flavour = Value.Simple; _ } -> Expand.expand { Expand.db = b.db; call_stack = []; expanding = Hashtbl.create 16 } v.rhs | _ -> v.rhs)
       | "?=" -> if prev = None then Value.set b.db ~origin:Value.Override ~flavour:Value.Recursive v.var v.rhs
       | _ -> ());
      Some (v.var, snapshot)) matching in
  fun () ->
    List.iter (fun (name, snap) ->
        match snap with
        | Some v -> Hashtbl.replace b.db.Value.vars name v
        | None -> Hashtbl.remove b.db.Value.vars name) (List.rev saved)

(* set the automatic variables for a recipe (10.5.3) *)
let set_automatic b ~target ~prereqs ~newer ~stem =
  let set n v = Value.set b.db ~flavour:Value.Simple ~origin:Value.Automatic n v in
  set "@" target;
  set "<" (match prereqs with p :: _ -> p | [] -> "");
  set "^" (String.concat " " prereqs);
  set "+" (String.concat " " prereqs);
  set "?" (String.concat " " newer);
  set "*" stem

let run_recipe b ~target ~lines =
  let rec go = function
    | [] -> true
    | line :: rest ->
        let line = Expand.expand { Expand.db = b.db; call_stack = []; expanding = Hashtbl.create 16 } line in
        (* recipe-line prefixes: @ silent, - ignore errors, + always run *)
        let silent = ref b.silent and ignore_err = ref false and i = ref 0 in
        let n = String.length line in
        while !i < n && (line.[!i] = '@' || line.[!i] = '-' || line.[!i] = '+' || line.[!i] = ' ' || line.[!i] = '\t') do
          (if line.[!i] = '@' then silent := true else if line.[!i] = '-' then ignore_err := true);
          incr i
        done;
        let cmd = String.sub line !i (n - !i) in
        if cmd = "" then go rest
        else begin
          if not !silent && not b.dry_run || (b.dry_run) then print_endline cmd;
          if b.dry_run then go rest
          else begin
            let shell = Value.get b.db "SHELL" in
            let shell = if shell = "" then "/bin/sh" else shell in
            let code = Sys.command (Printf.sprintf "%s -c %s" (Filename.quote shell) (Filename.quote cmd)) in
            if code = 0 then go rest
            else if !ignore_err then go rest
            else begin
              Printf.eprintf "occmake: *** [%s] Error %d\n" target code;
              false
            end
          end
        end in
  go lines

(* update one target; returns true if it (or a prerequisite) was rebuilt *)
let rec update b (t : string) : bool =
  match Hashtbl.find_opt b.done_ t with
  | Some rebuilt -> rebuilt
  | None ->
      if Hashtbl.mem b.building t then (Printf.eprintf "occmake: circular dependency on %s\n" t; false)
      else begin
        Hashtbl.replace b.building t ();
        let result = update_uncached b t in
        Hashtbl.remove b.building t;
        Hashtbl.replace b.done_ t result;
        result
      end

and update_uncached b t =
  let phony = Rule.is_phony b.rules t in
  match how b t with
  | Source ->
      if exists t then false
      else begin
        Printf.eprintf "occmake: *** No rule to make target '%s'.  Stop.\n" t;
        b.failed <- true; false
      end
  | Explicit r | Implicit (r, _) as h ->
      let stem = match h with Implicit (_, s) -> s | _ -> "" in
      (* how already substituted the stem into a combined rule's prerequisites *)
      let prereqs = r.prereqs in
      let order = r.order_only in
      (* build prerequisites first *)
      let prereq_rebuilt = List.map (fun p -> let rb = update b p in (p, rb)) (prereqs @ order) in
      if b.failed && not b.keep_going then false
      else begin
        let target_time = mtime t in
        let newer = List.filter_map (fun p ->
            if List.mem p order then None
            else match target_time, mtime p with
              | _, None -> Some p                         (* a phony or freshly made prereq *)
              | None, _ -> Some p
              | Some tt, Some pt -> if pt > tt then Some p else None) prereqs in
        let any_prereq_rebuilt = List.exists (fun (p, rb) -> rb && not (List.mem p order)) prereq_rebuilt in
        let must = phony || not (exists t) || newer <> [] || any_prereq_rebuilt in
        if must && r.recipe <> [] then begin
          set_automatic b ~target:t ~prereqs ~newer:(if newer = [] then prereqs else newer) ~stem;
          let restore = apply_tsv b t in
          let ok = if b.question then false else run_recipe b ~target:t ~lines:r.recipe in
          restore ();
          if b.question then (b.failed <- true; true)
          else if ok then true
          else (b.failed <- true; false)
        end
        else must
      end

let build ~db ~rules ~keep_going ~dry_run ~silent ~question goals =
  let b = { db; rules; building = Hashtbl.create 64; done_ = Hashtbl.create 256;
            keep_going; dry_run; silent; question; failed = false } in
  List.iter (fun g -> ignore (update b g)) goals;
  not b.failed
