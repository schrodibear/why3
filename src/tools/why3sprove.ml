(********************************************************************)
(*                                                                  *)
(*  The Why3 Verification Platform   /   The Why3 Development Team  *)
(*  Copyright 2010-2017   --   INRIA - CNRS - Paris-Sud University  *)
(*                                                                  *)
(*  This software is distributed under the terms of the GNU Lesser  *)
(*  General Public License version 2.1, with the special exception  *)
(*  on linking described in file LICENSE.                           *)
(*                                                                  *)
(********************************************************************)

open Format
open Why3
open Wstdlib
open Whyconf

let usage_msg = sprintf
  "Usage: %s [options] -S <strategy> [file|-]"
  (Filename.basename Sys.argv.(0))

let debug = Debug.register_info_flag
    ~desc:"of@ the@ progression@ of@ a@ strategy@ status@"
    "sprove"

let () = Debug.set_flag debug

let opt_strategy        = ref None
let opt_clean           = ref false

let opt_file_filter     = ref []
let opt_theory_filter   = ref []
let opt_goal_filter     = ref []

let opt_file_select     = ref []
let opt_theory_select   = ref []
let opt_goal_select     = ref []


let files = Queue.create ()

let option_list = [
  ("-S",
   Arg.String (fun s -> opt_strategy := Some s),
   "<strategy> use strategy to prove goals. By default, the strategy will be scheduled on all goals. You can limit scheduling with select and filter options. Select and filter can't be applied on the same level simultaneously.");
  ("--strategy",
   Arg.String (fun s -> opt_strategy := Some s),
   " same as -S");
  ("--clean",
   Arg.Set opt_clean,
   " remove failed proof attempts before exit");
  ("--file-filter",
   Arg.String (fun s -> opt_file_filter := s :: !opt_file_filter),
   "<string> don't run strategy on files with id string (case insensitive)");
  ("--theory-filter",
   Arg.String (fun s -> opt_theory_filter := s :: !opt_theory_filter),
   "<string> don't run strategy on theories with id string (case insensitive)");
  ("--goal-filter",
   Arg.String (fun s -> opt_goal_filter := s :: !opt_goal_filter),
   "<string> don't run strategy on goals with id string (case insensitive)");
  ("--file-select",
   Arg.String (fun s -> opt_file_select := s :: !opt_file_select),
   "<string> run strategy on files with id string (case insensitive)");
  ("--theory-select",
   Arg.String (fun s -> opt_theory_select := s :: !opt_theory_select),
   "<string> run strategy on theories with id string (case insensitive)");
  ("--goal-select",
   Arg.String (fun s -> opt_goal_select := s :: !opt_goal_select),
   "<string> run strategy on goals with id string (case insensitive)");
  Termcode.arg_extra_expl_prefix; ]

let config, base_config, env =
  Whyconf.Args.initialize option_list (fun f -> Queue.add f files) usage_msg

let () =
  if (!opt_file_filter != [] && !opt_file_select != []) ||
     (!opt_theory_filter != [] && !opt_theory_select != []) ||
     (!opt_goal_filter != [] && !opt_goal_select != []) ||
     Queue.is_empty files
  then Whyconf.Args.exit_with_usage option_list usage_msg


let project_dir =
  let fname = Queue.pop files in
  (* The remaining files in [files] are going to be open *)
  if Sys.file_exists fname then
    begin
      if Sys.is_directory fname then
        begin
          Debug.dprintf debug
            "[SPROVE] found directory '%s' for the project@." fname;
          fname
        end
      else
        if Queue.is_empty files then (* that was the only file *) begin
          Debug.dprintf debug "[SPROVE] found regular file '%s'@." fname;
          let d =
            try Filename.chop_extension fname
            with Invalid_argument _ -> fname
          in
          Debug.dprintf debug
            "[SPROVE] using '%s' as directory for the project@." d;
          Queue.push fname files; (* we need to open [fname] *)
          d
        end
        else begin
          (* The first argument is not a directory and it's not the
              only file *)
          Format.eprintf
            "[Error] @[When@ more@ than@ one@ file@ is@ given@ on@ the@ \
             command@ line@ the@ first@ one@ must@ be@ the@ directory@ \
             of@ the@ session.@]@.";
          Arg.usage option_list usage_msg; exit 1
        end
    end
  else
    fname

module S = Session

let load_strategies env config =
  Mstr.fold_left
    (fun acc _ st ->
      let name = st.Whyconf.strategy_name in
      try
        let code = Strategy_parser.parse env (st.Whyconf.strategy_code) in
        Format.eprintf "[SPROVE] Strategy '%s' loaded.@." name;
        (name, code) :: acc
      with Strategy_parser.SyntaxError msg ->
        Format.eprintf "Warning Loading strategy '%s' failed: %s@." name msg;
        acc)
    []
    (Whyconf.get_strategies config)

let file_filter_regexp   = List.map (fun s -> Str.regexp_string_case_fold s ) !opt_file_filter
let theory_filter_regexp = List.map (fun s -> Str.regexp_string_case_fold s ) !opt_theory_filter
let goal_filter_regexp   = List.map (fun s -> Str.regexp_string_case_fold s ) !opt_goal_filter

let file_select_regexp   = List.map (fun s -> Str.regexp_string_case_fold s ) !opt_file_select
let theory_select_regexp = List.map (fun s -> Str.regexp_string_case_fold s ) !opt_theory_select
let goal_select_regexp   = List.map (fun s -> Str.regexp_string_case_fold s ) !opt_goal_select

let filter_allow invert filter_regexp s =
  let check re =
    try ignore (Str.search_forward re s 0); true
    with Not_found -> false
  in
  List.for_all (fun r -> invert (check r)) filter_regexp

let file_allow, theory_allow, goal_allow =
  let choose_filter = (function
    | ([], []) -> (fun _ -> true)
    | (fl, []) -> filter_allow not fl
    | ([], sl) -> filter_allow (fun x -> x) sl
    | _        -> assert false) in
  choose_filter (file_filter_regexp,   file_select_regexp),
  choose_filter (theory_filter_regexp, theory_select_regexp),
  choose_filter (goal_filter_regexp,   goal_select_regexp)

let session_all_subgoals session =
  let res = ref [] in
  S.iter_session
    (fun f ->
       if file_allow (Filename.basename f.S.file_name)
       then
         S.iter_file
           (fun t ->
             if theory_allow t.S.theory_name.Ident.id_string
             then
               res := S.fold_all_sub_goals_of_theory (fun acc a ->
                   let goal_name = (S.goal_name a).Ident.id_string in
                   if goal_allow goal_name
                   then
                     begin
                       printf "sheduling strategy on goal: %s (%s)@." goal_name (S.goal_user_name a);
                       List.cons (S.Goal a) acc
                     end
                   else
                     acc
                 ) !res t) f) session;
  !res

let prover_answer_tostr a = match a with
  | Call_provers.Valid             -> "Valid"
  | Call_provers.Invalid           -> "Invalid"
  | Call_provers.Timeout           -> "Timeout"
  | Call_provers.OutOfMemory       -> "OutOfMemory"
  | Call_provers.StepLimitExceeded -> "StepLimitExceeded"
  | Call_provers.Unknown _         -> "Unknown"
  | Call_provers.Failure _         -> "Failure"
  | Call_provers.HighFailure       -> "HighFailure"

let print_proof_state st lt lm = match st with
  | S.Done { Call_provers.pr_time = time; Call_provers.pr_steps = steps; Call_provers.pr_answer = answer; } ->
     let s =
       Format.sprintf "%s %.2f [%d.0]" (prover_answer_tostr answer) time lt
     in
     if steps >= 0 then
       Format.sprintf "%s (steps: %d)" s steps
     else
       s
  | S.Unedited              -> "(not yet edited)"
  | S.JustEdited            -> "(edited)"
  | S.InternalFailure _     -> "(internal failure)"
  | S.Interrupted           -> "(interrupted)"
  | S.Scheduled | S.Running ->
      Format.sprintf "sheduled/running with [limit=%d sec., %d M]" lt lm


let report = ref (fun () -> (assert false : unit))
let ref_save_session = ref (fun () -> ())
let session_init = ref false
let session_proof_attempts_cnt = ref 0

module O = struct
  type key = int

  let create ?parent () =
    match parent with
    | None -> 0
    | Some n -> n+1

  let remove _row = ()

  let reset () = ()

  let init = fun _row _any -> ()

  let notify i =
    begin
      (match i with
      | S.Goal g ->
        printf "Goal '%s' proved: %b@." (S.goal_name g).Ident.id_string (Opt.inhabited (S.goal_verified g))
      | S.Theory th ->
        printf "Theory '%s' verified: %b@." th.S.theory_name.Ident.id_string (Opt.inhabited th.S.theory_verified)
      | S.File file ->
        printf "File '%s' verified: %b@." (Filename.basename file.S.file_name)
          (Opt.inhabited file.S.file_verified)
      | S.Proof_attempt a ->
        begin
          let p = a.S.proof_prover in
          let st = a.S.proof_state in
          if not !session_init then
            (match st with
             | S.Done _
             | S.InternalFailure _
             | S.JustEdited -> incr session_proof_attempts_cnt
             | _ -> ());
          if st <> S.Scheduled && st <> S.Running
          then
            printf "Proof with '%s,%s,%s' gives %s@."
              p.prover_name p.prover_version p.prover_altern
              (print_proof_state st
                 (a.S.proof_limit.Call_provers.limit_time)
                 (a.S.proof_limit.Call_provers.limit_mem))
        end
      | S.Transf tr ->
        printf "Transformation '%s' proved: %b@."
          tr.S.transf_name (Opt.inhabited tr.S.transf_verified)
      | S.Metas m ->
        printf "Metas proved: %b@."
          (Opt.inhabited m.S.metas_verified));
      let session_autosave = Whyconf.autosave @@ get_main config in
      if session_autosave > 0 && !session_proof_attempts_cnt == session_autosave
      then
        begin
          session_proof_attempts_cnt := 0;
          !ref_save_session ();
        end
    end


  let uninstalled_prover _eS unknown =
    try
      Whyconf.get_prover_upgrade_policy config unknown
    with Not_found -> Whyconf.CPU_keep

  module Scheduler = Session_scheduler.Base_scheduler(struct end)

  include Scheduler
  let notify_timer_state tw spt rpt =
    if tw = 0 && spt = 0 && rpt = 0
    then
      begin
        !report ();
        exit 0
      end
end

module M = Session_scheduler.Make(O)

let clean session =
  printf "Cleaning session...\n";
  let rec handle_any a =
    let any_success = ref false in
    let update r = if r then any_success := true in
    let unless_successful f ~perform =
      any_success := false;
      f ();
      if not @@ !any_success then perform ();
      !any_success
    in
    let rec_ a = update @@ handle_any a in
    let rec_pa pa = rec_ @@ S.Proof_attempt pa in
    let rec_tr tr = rec_ @@ S.Transf tr in
    let rec_me me = rec_ @@ S.Metas me in
    let rec_gl gl = rec_ @@ S.Goal gl in
    let rec_th th = rec_ @@ S.Theory th in
    match a with
    | S.Goal g when S.goal_verified g = None ->
      unless_successful (fun () -> S.iter_goal rec_pa rec_tr rec_me g) ~perform:(ignore)
    | S.Theory th when th.S.theory_verified = None ->
      unless_successful (fun () -> S.iter_theory rec_gl th) ~perform:(ignore)
    | S.File f when f.S.file_verified = None ->
      unless_successful (fun () -> S.iter_file rec_th f) ~perform:(ignore)
    | S.Proof_attempt pa ->
      let maybe_success =
        match[@warning "-33"] pa.S.proof_state with
        | S.Interrupted -> false
        | S.InternalFailure _ -> false
        | S.Done Call_provers.{ pr_answer = Valid | Invalid } -> true
        | S.Done _ -> false
        | _ -> true
      in
      if not maybe_success then M.remove_proof_attempt pa;
      maybe_success
    | S.Transf tr when tr.S.transf_verified = None ->
      unless_successful (fun () -> S.iter_transf rec_gl tr) ~perform:(fun () -> M.remove_transformation tr)
    | S.Metas m when m.S.metas_verified = None ->
      unless_successful (fun () -> S.iter_metas rec_gl m) ~perform:(fun () -> M.remove_metas m)
    | _ -> true
  in
  S.iter_session
    (fun r ->
       let any = S.File r in
       M.clean any;
       ignore @@ handle_any any)
  session

let goal_statistics (goals,n,m) g =
  if Opt.inhabited (S.goal_verified g) then (goals,n+1,m+1) else (g::goals,n,m+1)

let theory_statistics (ths,n,m) th =
  let goals,n1,m1 =
    List.fold_left goal_statistics ([],0,0) th.S.theory_goals in
  ((th,goals,n1,m1)::ths,n+n1,m+m1)

let file_statistics _ f (files,n,m) =
  let ths,n1,m1 =
    List.fold_left theory_statistics ([],0,0) f.S.file_theories in
  ((f,ths,n1,m1)::files,n+n1,m+m1)

let print_statistics files =
  let print_goal g =
      printf "         +--goal %s not proved@." (S.goal_name g).Ident.id_string
  in
  let print_theory (th,goals,n,m) =
    if n<m then begin
      printf "      +--theory %s: %d/%d@."
        th.S.theory_name.Ident.id_string n m;
      List.iter print_goal (List.rev goals)
    end
  in
  let print_file (f,ths,n,m) =
    if n<m then begin
      printf "   +--file %s: %d/%d@." f.S.file_name n m;
      List.iter print_theory (List.rev ths)
    end
  in
  List.iter print_file (List.rev files)

let save_session config session =
  begin
    Debug.dprintf debug "Saving session...@?";
    S.save_session config session;
    Debug.dprintf debug " done@.";
  end

let register_save_session config session =
  ref_save_session := (fun () ->
      save_session config session;
  )

let register_report env_session = report := (fun () ->
    let session = env_session.S.session in
    if !opt_clean then clean session;
    Debug.dprintf debug "@.";
    let files,n,m =
      S.PHstr.fold file_statistics
        session.S.session_files ([],0,0)
    in
    printf " %d/%d " n m;
    if n<m then print_statistics files;
    save_session config session
  )

let open_file env_session f =
  let f = Sysutil.relativize_filename project_dir f in
  Debug.dprintf debug "[SPROVE session] Adding file '%s'@." f;
  if S.PHstr.mem (env_session).S.session.S.session_files f then
    Debug.dprintf debug "[SPROVE] file %s already in database@." f
  else
    try
      Debug.dprintf debug "[SPROVE] adding file %s in database@." f;
      ignore (M.add_file env_session f)
    with e ->
      eprintf "@[Error while reading file@ '%s':@ %a@]@." f
        Exn_printer.exn_printer e;
      exit 1


let () =
  if not (Sys.file_exists project_dir) then
    begin
      Debug.dprintf debug "[SPROVE] '%s' does not exist. \
        Creating directory of that name for the project@." project_dir;
      Unix.mkdir project_dir 0o777
    end

let sched, env_session =
  try
    session_init := true;
    Debug.dprintf debug "@[<hov 2>[SPROVE session] Opening session...@\n";
    let session,use_shapes =
      if Sys.file_exists project_dir then
        S.read_session project_dir
      else
        S.create_session project_dir, false
    in
    let env_session,(_:bool),(_:bool) =
      M.update_session ~allow_obsolete:true ~release:false ~use_shapes
        session env config
    in
    Debug.dprintf debug "@]@\n[SPROVE session] Opening session: update done@.  @[<hov 2>";
    let sched = M.init (Whyconf.running_provers_max (Whyconf.get_main config))
    in
    Debug.dprintf debug "@]@\n[SPROVE session] Opening session: done@.";
    session_init := false;
    sched, env_session
  with e when not (Debug.test_flag Debug.stack_trace) ->
    eprintf "@[Error while opening session:@ %a@.@]"
      Exn_printer.exn_printer e;
    exit 1

let strategy =
  try
    List.find (fun s -> Some (fst s) = (!opt_strategy)) (load_strategies env_session config)
  with Not_found ->
    Whyconf.Args.exit_with_usage option_list usage_msg

let () = try
  register_report env_session;
  register_save_session config env_session.S.session;
  Queue.iter (fun f -> open_file env_session f) files;
  let run_strategy goal = M.run_strategy env_session sched
      ~context_unproved_goals_only:true (snd strategy) goal in
  let goals = session_all_subgoals env_session.S.session in
  List.iter run_strategy goals;
  O.main_loop ();
  eprintf "Nothing to be done. All goals are proved.@.";
  exit 0
  with e when not (Debug.test_flag Debug.stack_trace) ->
    eprintf "%a@." Exn_printer.exn_printer e;
    exit 1

(*
Local Variables:
compile-command: "unset LANG; make -C ../.. byte"
End:
*)
