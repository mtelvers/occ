(* Reading and evaluating a makefile into the variable and rule databases
   (GNU make manual, chapters 3 to 5, 7).

   A makefile is processed line by line.  Logical lines join physical
   lines ending in backslash (3.1.1).  A line beginning with a tab is a
   recipe line for the rule in progress.  Otherwise the line is a variable
   assignment, a directive (conditionals, include, define, export, vpath,
   .PHONY through a rule), or a rule.  Conditionals and include are
   resolved as the file is read, so a variable's value can steer them. *)

type state = {
  db : Value.t;
  rules : Rule.t;
  mutable current : Rule.rule option;   (* the rule whose recipe is being collected *)
  mutable include_dirs : string list;
  mutable default_goal : string option;
  mutable static : (string * string list * string list) option;  (* static pattern: tpat, ppats, targets *)
}

let ctx st = { Expand.db = st.db; call_stack = []; expanding = Hashtbl.create 16 }
let expand st s = Expand.expand (ctx st) s

(* ---- Physical to logical lines ------------------------------------------------- *)

(* Split into logical lines, joining backslash-newline.  A backslash-newline
   inside a recipe (tab line) keeps the backslash and newline for the shell;
   elsewhere it becomes a single space (3.1.1). *)
let logical_lines text =
  let lines = String.split_on_char '\n' text in
  let out = ref [] and buf = Buffer.create 128 and joining = ref false in
  List.iter (fun line ->
      let line = if String.length line > 0 && line.[String.length line - 1] = '\r' then String.sub line 0 (String.length line - 1) else line in
      let cont = String.length line > 0 && line.[String.length line - 1] = '\\'
                 && not (String.length line >= 2 && line.[String.length line - 2] = '\\') in
      let body = if cont then String.sub line 0 (String.length line - 1) else line in
      if !joining then begin
        (* continuation: collapse leading whitespace to one space unless recipe *)
        let trimmed = String.trim body in
        Buffer.add_char buf ' '; Buffer.add_string buf trimmed
      end else Buffer.add_string buf body;
      if cont then joining := true
      else begin out := Buffer.contents buf :: !out; Buffer.clear buf; joining := false end)
    lines;
  if Buffer.length buf > 0 then out := Buffer.contents buf :: !out;
  List.rev !out

(* strip an unescaped '#' comment (3.1) *)
let strip_comment line =
  let n = String.length line in
  let rec go i = if i >= n then line
    else if line.[i] = '#' && (i = 0 || line.[i - 1] <> '\\') then String.sub line 0 i
    else go (i + 1) in
  go 0

(* ---- Assignments and rules ----------------------------------------------------- *)

let starts_with p s = String.length s >= String.length p && String.sub s 0 (String.length p) = p

(* find the assignment operator (=, :=, ::=, ?=, +=) at top level, before
   any ':' that would make it a rule *)
let assignment_op line =
  let n = String.length line in
  let depth = ref 0 in
  let rec go i =
    if i >= n then None
    else match line.[i] with
      | '(' | '{' -> incr depth; go (i + 1)
      | ')' | '}' -> decr depth; go (i + 1)
      | '=' when !depth = 0 ->
          let op_start = if i > 0 && (line.[i-1] = ':' || line.[i-1] = '?' || line.[i-1] = '+') then
              (if i > 1 && line.[i-1] = ':' && line.[i-2] = ':' then i - 2 else i - 1) else i in
          Some (op_start, i + 1)
      | ':' when !depth = 0 && i + 1 < n && line.[i+1] <> '=' -> None   (* a rule *)
      | _ -> go (i + 1) in
  go 0

let finish_recipe st =
  (match st.current, st.static with
   | Some r, Some (tpat, ppats, targets) ->
       (* a static pattern rule: the collected recipe applies to each named
          target, with prerequisites derived from its stem (4.12) *)
       List.iter (fun t ->
           match Func.pattern_match tpat t with
           | Some stem ->
               let prereqs = List.map (fun p -> Func.pattern_subst p stem) ppats in
               Rule.add st.rules { r with Rule.targets = [ t ]; prereqs; is_pattern = false }
           | None -> ()) targets
   | Some r, None -> Rule.add st.rules r
   | None, _ -> ());
  st.current <- None;
  st.static <- None

let rec eval_text st text =
  let lines = logical_lines text in
  eval_lines st lines

and eval_lines st lines =
  (* a small conditional stack: whether each open conditional is active *)
  let active = ref [] in
  let live () = List.for_all (fun (b, _) -> b) !active in
  let rec loop = function
    | [] -> ()
    | raw :: rest ->
        let is_recipe = String.length raw > 0 && raw.[0] = '\t' in
        if is_recipe && st.current <> None && live () then begin
          (match st.current with Some r -> st.current <- Some { r with recipe = r.recipe @ [ String.sub raw 1 (String.length raw - 1) ] } | None -> ());
          loop rest
        end else begin
          let line = String.trim (strip_comment raw) in
          if line = "" then (loop rest)
          else begin
            let first = match String.index_opt line ' ' with Some i -> String.sub line 0 i | None -> line in
            match first with
            | "ifeq" | "ifneq" | "ifdef" | "ifndef" ->
                let cond = if live () then eval_cond st first (String.sub line (String.length first) (String.length line - String.length first)) else false in
                active := (cond, live ()) :: !active; loop rest
            | "else" ->
                (match !active with
                 | (taken, parent) :: tl ->
                     let rest_line = String.trim (String.sub line 4 (String.length line - 4)) in
                     if rest_line = "" then active := (parent && not taken, parent) :: tl
                     else begin (* else ifeq ... *)
                       let f2 = match String.index_opt rest_line ' ' with Some i -> String.sub rest_line 0 i | None -> rest_line in
                       let cond = if parent && not taken then eval_cond st f2 (String.sub rest_line (String.length f2) (String.length rest_line - String.length f2)) else false in
                       active := (cond, parent) :: tl
                     end
                 | [] -> ());
                loop rest
            | "endif" -> (match !active with _ :: tl -> active := tl | [] -> ()); loop rest
            | _ when not (live ()) -> loop rest
            | "include" | "-include" | "sinclude" ->
                let files = Expand.words (expand st (String.sub line (String.length first) (String.length line - String.length first))) in
                List.iter (fun f -> include_file st ~optional:(first <> "include") f) files;
                loop rest
            | "define" -> loop (collect_define st line rest)
            | "export" ->
                let rest_line = String.trim (String.sub line 6 (String.length line - 6)) in
                (if rest_line <> "" && assignment_op rest_line <> None then do_assignment st rest_line
                 else List.iter (fun n -> try Unix.putenv n (Value.get st.db n) with _ -> ()) (Expand.words (expand st rest_line)));
                loop rest
            | "unexport" -> loop rest
            | "override" -> do_assignment ~origin:Value.Override st (String.trim (String.sub line 8 (String.length line - 8))); loop rest
            | "vpath" -> loop rest
            | _ ->
                (match assignment_op line with
                 | Some _ -> do_assignment st line
                 | None ->
                     (* a rule if there is a top-level colon; otherwise a bare
                        expression evaluated for its side effects, such as
                        $(eval ...), $(info ...) or a $(foreach ...) of them *)
                     if top_colon line <> None then parse_rule st line
                     else ignore (expand st line));
                loop rest
          end
        end in
  loop lines;
  finish_recipe st

and eval_cond st kind arg =
  let arg = String.trim arg in
  match kind with
  | "ifdef" -> Value.get st.db (String.trim (expand st arg)) <> ""
  | "ifndef" -> Value.get st.db (String.trim (expand st arg)) = ""
  | _ ->
      let a, b = parse_two arg in
      let a = String.trim (expand st a) and b = String.trim (expand st b) in
      if kind = "ifeq" then a = b else a <> b

(* "(a,b)" or "\"a\" \"b\"" or "'a' 'b'" *)
and parse_two arg =
  if starts_with "(" arg then
    let inner = String.sub arg 1 (String.length arg - 2) in
    (match Expand.split_args ~max:2 inner with [ a; b ] -> a, b | [ a ] -> a, "" | _ -> "", "")
  else
    (* quoted forms *)
    let q = arg.[0] in
    (match String.index_from_opt arg 1 q with
     | Some e ->
         let a = String.sub arg 1 (e - 1) in
         let rest = String.trim (String.sub arg (e + 1) (String.length arg - e - 1)) in
         let b = if String.length rest >= 2 then String.sub rest 1 (String.length rest - 2) else "" in
         a, b
     | None -> arg, "")

and do_assignment ?origin st line =
  match assignment_op line with
  | None -> ()
  | Some (op_start, val_start) ->
      let name = String.trim (String.sub line 0 op_start) in
      let op = String.sub line op_start (val_start - op_start) in
      let rhs = if val_start <= String.length line then String.trim (String.sub line val_start (String.length line - val_start)) else "" in
      let name = expand st name in
      (match op with
       | "=" -> Value.set st.db ?origin ~flavour:Value.Recursive name rhs
       | ":=" | "::=" -> Value.set st.db ?origin ~flavour:Value.Simple name (expand st rhs)
       | "?=" -> if Value.find st.db name = None then Value.set st.db ?origin ~flavour:Value.Recursive name rhs
       | "+=" ->
           (match Value.find st.db name with
            | Some { flavour = Value.Simple; _ } -> Value.append st.db name (expand st rhs)
            | _ -> Value.append st.db name rhs)
       | _ -> ())

and parse_rule st line = try parse_rule_body st line with Exit -> ()
and parse_rule_body st line =
  (* split targets ':' prereqs (':=' already excluded), a target-specific
     variable, or a static pattern rule *)
  let colon = top_colon line in
  match colon with
  | None -> ()
  | Some i ->
      let targets_s = String.sub line 0 i in
      let rhs = String.sub line (i + 1) (String.length line - i - 1) in
      (* an inline recipe after a ';' on the rule line (5.1) *)
      let rhs, inline_recipe =
        let depth = ref 0 and cut = ref (-1) and k = ref 0 in
        String.iter (fun c ->
            (match c with '(' | '{' -> incr depth | ')' | '}' -> decr depth
             | ';' when !depth = 0 && !cut < 0 -> cut := !k | _ -> ());
            incr k) rhs;
        if !cut >= 0 then String.sub rhs 0 !cut, Some (String.trim (String.sub rhs (!cut + 1) (String.length rhs - !cut - 1)))
        else rhs, None in
      let is_double = String.length rhs > 0 && rhs.[0] = ':' in
      let rhs = if is_double then String.sub rhs 1 (String.length rhs - 1) else rhs in
      let targets = Expand.words (expand st targets_s) in
      (* a target-specific variable: "targets: VAR OP value" (6.11) *)
      (match assignment_op rhs with
       | Some _ when top_colon rhs = None && not is_double ->
           (match assignment_op rhs with
            | Some (op_start, val_start) ->
                let var = String.trim (String.sub rhs 0 op_start) in
                (* a real prerequisite list has no bare assignment; a single
                   identifier before the operator marks a variable *)
                if var <> "" && not (String.contains var ' ') && not (String.contains var '/') && not (String.contains var '.') then begin
                  let op = String.sub rhs op_start (val_start - op_start) in
                  let value = if val_start <= String.length rhs then String.trim (String.sub rhs val_start (String.length rhs - val_start)) else "" in
                  finish_recipe st;
                  List.iter (fun t -> st.rules.Rule.tsvs <- st.rules.Rule.tsvs @ [ { Rule.pat = t; var; op; rhs = value } ]) targets;
                  raise Exit
                end
            | None -> ())
       | _ -> ());
      (* static pattern rule: "targets: tpat: ppat" *)
      let prereqs_s, order_s, static =
        (match top_colon rhs with
         | Some j -> "", "", Some (String.sub rhs 0 j, String.sub rhs (j + 1) (String.length rhs - j - 1))
         | None ->
             (match String.index_opt rhs '|' with
              | Some j -> String.sub rhs 0 j, String.sub rhs (j + 1) (String.length rhs - j - 1), None
              | None -> rhs, "", None)) in
      let make_rule targets prereqs order is_pattern =
        finish_recipe st;
        let r = { Rule.targets; prereqs; order_only = order; recipe = (match inline_recipe with Some c -> [ c ] | None -> []); is_pattern;
                  is_double_colon = is_double; phony = false } in
        st.current <- Some r;
        if st.default_goal = None && not is_pattern then
          (match List.find_opt (fun t -> not (starts_with "." t)) targets with Some g -> st.default_goal <- Some g | None -> ()) in
      (match static with
       | Some (tpat, ppat) ->
           let tpat = String.trim (expand st tpat) in
           let ppats = Expand.words (expand st ppat) in
           finish_recipe st;
           (* the shared recipe is collected next and applied per target in finish_recipe *)
           st.current <- Some { Rule.targets; prereqs = []; order_only = []; recipe = []; is_pattern = false; is_double_colon = is_double; phony = false };
           st.static <- Some (tpat, ppats, targets)
       | None ->
           let prereqs = Expand.words (expand st prereqs_s) and order = Expand.words (expand st order_s) in
           let is_pattern = List.exists (fun t -> String.contains t '%') targets in
           if List.mem ".PHONY" targets then List.iter (Rule.mark_phony st.rules) prereqs;
           if List.mem ".SECONDEXPANSION" targets then st.rules.Rule.second_expansion <- true;
           make_rule targets prereqs order is_pattern)

and top_colon line =
  let n = String.length line in
  let depth = ref 0 in
  let rec go i =
    if i >= n then None
    else match line.[i] with
      | '(' | '{' -> incr depth; go (i + 1)
      | ')' | '}' -> decr depth; go (i + 1)
      | ':' when !depth = 0 && not (i + 1 < n && line.[i+1] = '=') && not (i > 0 && line.[i-1] = ':') -> Some i
      | _ -> go (i + 1) in
  go 0

and collect_define st line rest =
  (* define NAME [op]\n ... \nendef *)
  let header = String.trim (String.sub line 6 (String.length line - 6)) in
  let name, flavour = match assignment_op header with
    | Some (s, v) -> String.trim (String.sub header 0 s), (if String.sub header s (v - s) = ":=" then Value.Simple else Value.Recursive)
    | None -> header, Value.Recursive in
  let body = Buffer.create 128 in
  let rec take = function
    | [] -> []
    | l :: tl when (let t = String.trim (strip_comment l) in t = "endef") -> tl
    | l :: tl -> if Buffer.length body > 0 then Buffer.add_char body '\n'; Buffer.add_string body l; take tl in
  let rest = take rest in
  Value.set st.db ~flavour (expand st name) (if flavour = Value.Simple then expand st (Buffer.contents body) else Buffer.contents body);
  rest

and include_file st ~optional f =
  let path = if Sys.file_exists f then Some f
    else List.find_map (fun d -> let p = Filename.concat d f in if Sys.file_exists p then Some p else None) st.include_dirs in
  match path with
  | Some p -> eval_text st (In_channel.with_open_bin p In_channel.input_all)
  | None -> if not optional then raise (Expand.Make_error (Printf.sprintf "%s: no such file" f))
