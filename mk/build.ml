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
  name : string;                                 (* how to name itself in errors *)
  (* files made only because a chain of pattern rules needed them (10.4) *)
  intermediates : (string, unit) Hashtbl.t;
  mentioned : (string, unit) Hashtbl.t;          (* names a rule writes out *)
  goals : (string, unit) Hashtbl.t;
  mutable failed : bool;
}

let mtime f = match Unix.stat f with s -> Some s.Unix.st_mtime | exception _ -> None
let exists f = Sys.file_exists f

(* ---------- the directory search (GNU make manual 4.5) ---------- *)

(* The directories to look in for [name]: those of a `vpath' whose
   pattern it matches, then those of VPATH.  Both accept colons or
   spaces as separators. *)
let search_dirs b name =
  let from_vpath =
    List.concat_map (fun (pat, dirs) ->
        if Func.pattern_match pat name <> None then dirs else [])
      b.rules.Rule.vpaths in
  let general =
    let raw = Value.get b.db "VPATH" in
    List.filter (fun d -> d <> "")
      (List.concat_map (String.split_on_char ':')
         (List.concat_map (fun s -> String.split_on_char ' ' s)
            (String.split_on_char '\t' raw))) in
  from_vpath @ general

(* Where [name] can be found: itself if it is there, else the first
   directory of the search path that holds it.  4.5.3: a file found this
   way is used as a prerequisite, but a target that has to be rebuilt is
   rebuilt in the current directory, so this is only ever consulted for
   files that already exist. *)
let locate b name =
  if exists name then Some name
  else if not (Filename.is_relative name) then None
  else
    List.find_map (fun d ->
        let p = Filename.concat d name in
        if exists p then Some p else None) (search_dirs b name)

(* how a target will be built: an explicit rule, a matched pattern rule
   (with its stem), or nothing *)
type how =
  | Explicit of Rule.rule
  | Implicit of Rule.rule * string   (* the pattern rule and the stem *)
  | Source                           (* a file with no rule; must already exist *)

let starts_with p s = String.length s >= String.length p && String.sub s 0 (String.length p) = p

(* The pattern rules whose target pattern matches [t], with the stem,
   ordered by how specific the match is: the shortest stem first (10.8).
   Without that ordering a general rule such as `%.o: %.c' would win over
   `dir/%.c.o: dir/%.c' for `dir/x.c.o', matching the stem `dir/x.c' and
   asking for a file called `dir/x.c.c'. *)
let matching_patterns b t =
  let candidates = List.filter_map (fun (r : Rule.rule) ->
      match List.find_map (fun tp -> Option.map (fun stem -> tp, stem) (Func.pattern_match tp t)) r.targets with
      | Some (_, stem) -> Some (r, stem)
      | None -> None) b.rules.Rule.patterns in
  List.stable_sort (fun (_, a) (_, z) -> compare (String.length a) (String.length z)) candidates

(* Can this name be provided?  It is there, or the search path has it, or
   a rule names it, or -- and this is rule chaining (10.4) -- a pattern
   rule applies whose own prerequisites can in turn be provided.  That is
   what makes `x.t' from `x.c.o' from `x.c', none of which exists yet.
   The depth is bounded: without a bound a chain of pattern rules could
   be followed for ever. *)
let rec provided b depth t =
  exists t
  || Hashtbl.mem b.rules.Rule.by_target t
  || Rule.is_phony b.rules t
  || locate b t <> None
  || (depth > 0
      && List.exists (fun ((r : Rule.rule), stem) ->
          List.for_all (fun p -> provided b (depth - 1) (Func.pattern_subst p stem)) r.prereqs)
        (matching_patterns b t))

(* find an applicable pattern rule whose prerequisites can be provided *)
let find_implicit b t =
  let candidates = matching_patterns b t in
  (* First a rule whose prerequisites are all there, and only then one
     that needs a chain, which is the order 10.8 asks for. *)
  match List.find_opt (fun ((r : Rule.rule), stem) ->
      List.for_all (fun p -> provided b 0 (Func.pattern_subst p stem)) r.prereqs) candidates with
  | Some hit -> Some hit
  | None ->
      List.find_opt (fun ((r : Rule.rule), stem) ->
          List.for_all (fun p -> provided b 3 (Func.pattern_subst p stem)) r.prereqs) candidates


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
           (* the pattern rule brings the recipe, so its own prerequisite
              is the first one and $< names it *)
           Implicit ({ pr with prereqs = stemmed @ r.prereqs;
                               order_only = pr.order_only @ r.order_only }, stem)
       | None -> Explicit r)
  | None ->
      (match find_implicit b t with
       | Some (pr, stem) -> Implicit ({ pr with prereqs = List.map (fun p -> Func.pattern_subst p stem) pr.prereqs }, stem)
       | None -> Source)

(* Bind the target- and pattern-specific variables that apply to [t]
   (6.11, 6.12), returning a closure that restores the previous values.
   The bindings are applied in file order so a later one wins.

   They are applied on the way *into* a target, before its prerequisites
   are built, because 6.11 has them apply to the target and to every
   prerequisite reached through it: the build relies on that to give
   runtime/sak the runtime's own preprocessor flags, which are set on the
   object files that need it and reach sak through them. *)
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

(* the directory and file halves of an automatic variable (10.5.3) *)
let dirpart s = match String.rindex_opt s '/' with Some i -> String.sub s 0 i | None -> "."
let filepart s = match String.rindex_opt s '/' with Some i -> String.sub s (i + 1) (String.length s - i - 1) | None -> s
let each f s = String.concat " " (List.map f (String.split_on_char ' ' s))

(* set the automatic variables for a recipe (10.5.3), including the D/F
   directory and file variants *)
let set_automatic b ~target ~prereqs ~newer ~stem =
  let set n v = Value.set b.db ~flavour:Value.Simple ~origin:Value.Automatic n v in
  let first = match prereqs with p :: _ -> p | [] -> "" in
  let all = String.concat " " prereqs in
  set "@" target; set "@D" (dirpart target); set "@F" (filepart target);
  set "<" first; set "<D" (dirpart first); set "<F" (filepart first);
  set "^" all; set "^D" (each dirpart all); set "^F" (each filepart all);
  set "+" all;
  set "?" (String.concat " " newer);
  set "*" stem; set "*D" (dirpart stem); set "*F" (filepart stem)

(* Does the line start a sub-make?  Such a line is run even under -n, so
   that a dry run shows the whole recursive plan (9.3); -n reaches the
   sub-make through MAKEFLAGS instead. *)
let mentions_make raw =
  let has needle =
    let n = String.length needle and m = String.length raw in
    let rec go i = i + n <= m && (String.sub raw i n = needle || go (i + 1)) in
    go 0 in
  has "$(MAKE)" || has "${MAKE}"

let run_recipe b ~target ~lines =
  let rec go = function
    | [] -> true
    | raw :: rest ->
        let recursive = mentions_make raw in
        let line = Expand.expand { Expand.db = b.db; call_stack = []; expanding = Hashtbl.create 16 } raw in
        (* recipe-line prefixes: @ silent, - ignore errors, + always run *)
        let silent = ref b.silent and ignore_err = ref false and always = ref recursive and i = ref 0 in
        let n = String.length line in
        while !i < n && (line.[!i] = '@' || line.[!i] = '-' || line.[!i] = '+' || line.[!i] = ' ' || line.[!i] = '\t') do
          (if line.[!i] = '@' then silent := true
           else if line.[!i] = '-' then ignore_err := true
           else if line.[!i] = '+' then always := true);
          incr i
        done;
        let cmd = String.sub line !i (n - !i) in
        if cmd = "" then go rest
        else begin
          if not !silent && not b.dry_run || (b.dry_run) then print_endline cmd;
          if b.dry_run && not !always then go rest
          else begin
            let shell = Value.get b.db "SHELL" in
            let shell = if shell = "" then "/bin/sh" else shell in
            let code = Sys.command (Printf.sprintf "%s -c %s" (Filename.quote shell) (Filename.quote cmd)) in
            if code = 0 then go rest
            else if !ignore_err then go rest
            else begin
              Printf.eprintf "%s: *** [%s] Error %d\n" b.name target code;
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
      if Hashtbl.mem b.building t then
        (Printf.eprintf "%s: Circular %s <- %s dependency dropped.\n" b.name t t; false)
      else begin
        Hashtbl.replace b.building t ();
        let result = update_uncached b t in
        Hashtbl.remove b.building t;
        Hashtbl.replace b.done_ t result;
        result
      end

and update_uncached b t =
  let restore = apply_tsv b t in
  Fun.protect ~finally:restore (fun () -> update_body b t)

and update_body b t =
  let phony = Rule.is_phony b.rules t in
  (* OCCMAKE_DEBUG names, for each target, how it was chosen and with
     which prerequisites; the fastest way to see why a build differs *)
  let debug = Sys.getenv_opt "OCCMAKE_DEBUG" <> None in
  match how b t with
  | Source ->
      if exists t then false
      else begin
        Printf.eprintf "%s: *** No rule to make target '%s'.  Stop.\n" b.name t;
        b.failed <- true; false
      end
  | Explicit r | Implicit (r, _) as h ->
      let stem = match h with Implicit (_, s) -> s | _ -> "" in
      (* A file that only a pattern rule knows about, that no rule names
         and that was not asked for, exists only as a link in a chain and
         is removed once the build is done (10.4). *)
      if debug then
        Printf.eprintf "occmake: %s: %s prereqs=[%s] order=[%s] recipe=%d\n" t
          (match h with Implicit (_, s) -> "implicit stem=" ^ s | _ -> "explicit")
          (String.concat " " r.prereqs) (String.concat " " r.order_only)
          (List.length r.recipe);
      (match h with
       | Implicit _ when not (Hashtbl.mem b.mentioned t) && not (Hashtbl.mem b.goals t)
                         && not (Rule.is_precious b.rules t) && not (exists t) ->
           Hashtbl.replace b.intermediates t ()
       | _ -> ());
      (* how already substituted the stem into a combined rule's prerequisites *)
      let prereqs = r.prereqs in
      let order = r.order_only in
      (* .SECONDEXPANSION: prerequisites are expanded again now, with the
         target's automatic variables available (e.g. .dep/$(@D)) *)
      let prereqs, order =
        if b.rules.Rule.second_expansion then begin
          set_automatic b ~target:t ~prereqs ~newer:[] ~stem;
          let reexp lst = List.concat_map (fun p ->
              Expand.words (Expand.expand { Expand.db = b.db; call_stack = []; expanding = Hashtbl.create 4 } p)) lst in
          reexp prereqs, reexp order
        end else prereqs, order in
      (* A prerequisite that is not in the current directory and has no
         rule of its own may be somewhere on the search path; if it is,
         that is the name the recipe and the automatic variables see
         (4.5.3). *)
      let resolve p =
        if exists p || Hashtbl.mem b.rules.Rule.by_target p
           || Rule.is_phony b.rules p then p
        else match locate b p with Some found -> found | None -> p in
      let prereqs = List.map resolve prereqs in
      let order = List.map resolve order in
      (* build prerequisites first *)
      let prereq_rebuilt = List.map (fun p -> let rb = update b p in (p, rb)) (prereqs @ order) in
      if b.failed && not b.keep_going then false
      else begin
        let target_time =
          match mtime t with
          | Some _ as time -> time
          | None -> (match locate b t with Some found -> mtime found | None -> None) in
        let newer = List.filter_map (fun p ->
            if List.mem p order then None
            else match target_time, mtime p with
              | _, None -> Some p                         (* a phony or freshly made prereq *)
              | None, _ -> Some p
              | Some tt, Some pt -> if pt > tt then Some p else None) prereqs in
        let any_prereq_rebuilt = List.exists (fun (p, rb) -> rb && not (List.mem p order)) prereq_rebuilt in
        (* A target the search path can find counts as existing, and if
           nothing is newer it is up to date where it was found (4.5.3). *)
        let must = phony || locate b t = None || newer <> [] || any_prereq_rebuilt in
        if must && r.recipe <> [] then begin
          if debug then
            Printf.eprintf "occmake: %s: running with prereqs=[%s]\n  recipe: %s\n" t
              (String.concat " " prereqs) (String.concat "\n  recipe: " r.recipe);
          set_automatic b ~target:t ~prereqs ~newer:(if newer = [] then prereqs else newer) ~stem;
          if debug then
            Printf.eprintf "occmake: %s: automatics @=[%s] <=[%s] ^=[%s]\n" t
              (Value.get b.db "@") (Value.get b.db "<") (Value.get b.db "^");
          let ok = if b.question then false else run_recipe b ~target:t ~lines:r.recipe in
          if b.question then (b.failed <- true; true)
          else if ok then true
          else (b.failed <- true; false)
        end
        else must
      end

let build ~db ~rules ~keep_going ~dry_run ~silent ~question ~name goals =
  let mentioned = Hashtbl.create 512 in
  List.iter (fun (r : Rule.rule) ->
      List.iter (fun p -> Hashtbl.replace mentioned p ()) (r.prereqs @ r.order_only))
    rules.Rule.explicit;
  let goal_set = Hashtbl.create 8 in
  List.iter (fun g -> Hashtbl.replace goal_set g ()) goals;
  let b = { db; rules; building = Hashtbl.create 64; done_ = Hashtbl.create 256;
            keep_going; dry_run; silent; question; name;
            intermediates = Hashtbl.create 64; mentioned; goals = goal_set;
            failed = false } in
  List.iter (fun g -> ignore (update b g)) goals;
  (* The intermediates go last, in one report, as make does.  Their order
     here is settled rather than the reference's, which is its own
     internal one. *)
  let leftovers =
    List.sort compare
      (Hashtbl.fold (fun f () acc -> if exists f || dry_run then f :: acc else acc)
         b.intermediates []) in
  if leftovers <> [] && not b.failed then begin
    print_endline ("rm " ^ String.concat " " leftovers);
    if not dry_run then List.iter (fun f -> try Sys.remove f with _ -> ()) leftovers
  end;
  not b.failed
