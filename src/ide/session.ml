(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) 2010-                                                   *)
(*    François Bobot                                                     *)
(*    Jean-Christophe Filliâtre                                          *)
(*    Claude Marché                                                      *)
(*    Andrei Paskevich                                                    *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)


open Why
open Format

(***************************)
(*     provers             *)
(***************************)

type prover_data = 
    { prover_id : string;
      prover_name : string;
      prover_version : string;
      command : string;
      driver_name : string;
      driver : Driver.driver;
      mutable editor : string;
    }

let get_prover_data env id pr acc =
  try
    let dr = Driver.load_driver env pr.Whyconf.driver in
    Util.Mstr.add id
      { prover_id = id ;
      prover_name = pr.Whyconf.name;
      prover_version = pr.Whyconf.version;
      command = pr.Whyconf.command;
      driver_name = pr.Whyconf.driver;
      driver = dr;
      editor = pr.Whyconf.editor;
    }
      acc
  with _e ->
    eprintf "Failed to load driver %s for prover %s. prover disabled@."
      pr.Whyconf.driver pr.Whyconf.name;
    acc

(***************************)
(*      transformations    *)
(***************************)

type trans =
  | Trans_one of Task.task Trans.trans
  | Trans_list of Task.task Trans.tlist

type transformation_data = 
    { transformation_name : string;
      transformation : trans;
    }

let transformation_id t = t.transformation_name

let lookup_trans env name =
  try
    let t = Trans.lookup_transform name env in
    Trans_one t
  with Trans.UnknownTrans _ ->
    let t = Trans.lookup_transform_l name env in
    Trans_list t

let lookup_transformation env =
  let h = Hashtbl.create 13 in
  fun name ->
    try 
      Hashtbl.find h name
    with Not_found ->
      let t = {transformation_name = name;
	       transformation = lookup_trans env name }
      in Hashtbl.add h name t; t

(***************************)
(*     proof status        *)
(***************************)

type proof_attempt_status =
  | Undone
  | Scheduled (** external proof attempt is scheduled *)
  | Running (** external proof attempt is in progress *)
  | Done of Call_provers.prover_result (** external proof done *)
  | InternalFailure of exn (** external proof aborted by internal error *)


(***************************)
(*     main functor        *)
(***************************)

module type OBSERVER = sig
  type key
  val create: ?parent:key -> unit -> key
  val remove: key -> unit

  val timeout: ms:int -> (unit -> bool) -> unit
  val idle: (unit -> bool) -> unit
end

module Make(O : OBSERVER) = struct

(***************************)
(*     session state       *)
(***************************)

type proof_attempt =
    { prover : prover_data;
      proof_goal : goal;
      proof_key : O.key;
      mutable proof_state : proof_attempt_status;
      mutable proof_obsolete : bool;
      mutable edited_as : string;
    }

and goal_parent =
  | Parent_theory of theory
  | Parent_transf of transf

and goal =
    { goal_name : string;
      goal_expl : string option;
      parent : goal_parent;
      mutable task: Task.task option;
      checksum : string;
      goal_key : O.key;
      mutable proved : bool;
      external_proofs: (string, proof_attempt) Hashtbl.t;
      transformations : (string, transf) Hashtbl.t;
    }

and transf =
    { transf : transformation_data;
      parent_goal : goal;
      mutable transf_proved : bool;
      transf_key : O.key;
      mutable subgoals : goal list;
    }

and theory =
    { theory_name : string;
      mutable theory : Theory.theory option;
      theory_key : O.key;
      theory_parent : file;
      mutable goals : goal list;
      mutable verified : bool;
    }

and file =
    { file_name : string;
      file_key : O.key;
      mutable theories: theory list;
      mutable file_verified : bool;
    }

type any =
  | File of file
  | Theory of theory
  | Goal of goal
  | Proof_attempt of proof_attempt
  | Transformation of transf

let theory_name t = t.theory_name
let theory_key t = t.theory_key
let verified t = t.verified
let goals t = t.goals

let get_theory t = 
  match t.theory with 
    | None -> 
	eprintf "Session: theory not yet reimported, this should not happen@.";
	assert false
    | Some t -> t

let goal_name g = g.goal_name
let goal_expl g = 
  match g.goal_expl with 
    | None -> g.goal_name
    | Some s -> s
let goal_key g = g.goal_key
let goal_proved g = g.proved
let transformations g = g.transformations

let get_task g = 
  match g.task with 
    | None -> 
	begin
	  match g.parent with
	    | Parent_theory _th ->
		assert false (* should not happen *)
	    | Parent_transf _tr ->
		(* TODO: recompute the task from the parent transformation *)
		assert false
	end
    | Some t -> t

let all_files : file list ref = ref []

let get_all_files () = !all_files
 
(************************)
(* saving state on disk *)
(************************)

let save_result fmt r =
  fprintf fmt "@\n<result status=\"%s\" time=\"%.2f\"/>"
    (match r.Call_provers.pr_answer with
       | Call_provers.Valid -> "valid"
       | Call_provers.Failure _ -> "failure"
       | Call_provers.Unknown _ -> "unknown"
       | Call_provers.HighFailure -> "highfailure"
       | Call_provers.Timeout -> "timeout"
       | Call_provers.Invalid -> "invalid")
    r.Call_provers.pr_time

let save_status fmt s =
  match s with
    | Undone | Scheduled | Running ->
        fprintf fmt "<undone/>@\n" 
    | InternalFailure msg -> 
        fprintf fmt "<internalfailure reason=\"%s\"/>@\n" 
          (Printexc.to_string msg)
    | Done r -> save_result fmt r

let save_proof_attempt fmt _key a =
  fprintf fmt "@\n@[<v 1><proof prover=\"%s\" edited=\"%s\">" 
    a.prover.prover_id
    a.edited_as;
  save_status fmt a.proof_state;
  fprintf fmt "@]@\n</proof>"

let opt lab fmt = function
  | None -> ()
  | Some s -> fprintf fmt "%s=\"%s\" " lab s

let rec save_goal fmt g =
  fprintf fmt "@\n@[<v 1><goal name=\"%s\" %aproved=\"%b\">" 
    g.goal_name (opt "expl") g.goal_expl g.proved;
  Hashtbl.iter (save_proof_attempt fmt) g.external_proofs;
  Hashtbl.iter (save_trans fmt) g.transformations;
  fprintf fmt "@]@\n</goal>"

and save_trans fmt _ t =
  fprintf fmt "@\n@[<v 1><transf name=\"%s\" proved=\"%b\">" 
    t.transf.transformation_name t.transf_proved;
  List.iter (save_goal fmt) t.subgoals;
  fprintf fmt "@]@\n</transf>"

let save_theory fmt t =
  fprintf fmt "@\n@[<v 1><theory name=\"%s\" verified=\"%b\">" 
    t.theory_name t.verified;
  List.iter (save_goal fmt) t.goals;
  fprintf fmt "@]@\n</theory>"

let save_file fmt f =
  fprintf fmt "@\n@[<v 1><file name=\"%s\" verified=\"%b\">" f.file_name f.file_verified;
  List.iter (save_theory fmt) f.theories;
  fprintf fmt "@]@\n</file>"
  
let save fname =
  let ch = open_out fname in
  let fmt = formatter_of_out_channel ch in
  fprintf fmt "<?xml version=\"1.0\" encoding=\"UTF-8\"?>@\n";
  fprintf fmt "<!DOCTYPE why3session SYSTEM \"why3session.dtd\">@\n";
  fprintf fmt "@[<v 1><why3session name=\"%s\">" fname;
  List.iter (save_file fmt) (get_all_files());
  fprintf fmt "@]@\n</why3session>";
  fprintf fmt "@.";
  close_out ch

(************************)
(*     actions          *)
(************************)

let init_fun = ref (fun (_:O.key) (_:any) -> ())

let notify_fun = ref (fun (_:any) -> ())

let check_file_verified f =
  let b = List.for_all (fun t -> t.verified) f.theories in
  if f.file_verified <> b then
    begin
      f.file_verified <- b;
      !notify_fun (File f)
    end

let check_theory_proved t =
  let b = List.for_all (fun g -> g.proved) t.goals in
  if t.verified <> b then
    begin
      t.verified <- b;
      !notify_fun (Theory t);
      check_file_verified t.theory_parent
    end

let rec check_goal_proved g =
  let b1 = Hashtbl.fold
    (fun _ a acc -> acc ||
       match a.proof_state with
	 | Done { Call_provers.pr_answer = Call_provers.Valid} -> true
	 | _ -> false) g.external_proofs false
  in
  let b = Hashtbl.fold
    (fun _ t acc -> acc || t.transf_proved) g.transformations b1
  in
  if g.proved <> b then
    begin
      g.proved <- b;
      !notify_fun (Goal g);
      match g.parent with
        | Parent_theory t -> check_theory_proved t
        | Parent_transf t -> check_transf_proved t
    end

and check_transf_proved t =
  let b = List.for_all (fun g -> g.proved) t.subgoals in
  if t.transf_proved <> b then
    begin
      t.transf_proved <- b;
      !notify_fun (Transformation t);
      check_goal_proved t.parent_goal
    end

let set_proof_state ~obsolete a res =
  a.proof_state <- res;
  a.proof_obsolete <- obsolete;
  !notify_fun (Proof_attempt a);
  match res with
    | Done _ ->
	check_goal_proved a.proof_goal
    | _ -> ()

(*************************)
(*         Scheduler     *)
(*************************) 


(* timeout handler *)

let maximum_running_proofs = ref 2
let running_proofs = ref []

let proof_attempts_queue = Queue.create ()

let timeout_handler_activated = ref false
let timeout_handler_running = ref false

let timeout_handler () =
  assert (not !timeout_handler_running);
  timeout_handler_running := true;
  let l = List.fold_left
    (fun acc ((callback,call) as c) ->
       match Call_provers.query_call call with
	 | None -> c::acc
	 | Some post ->
	     let res = post () in callback (Done res);
	     acc)
    [] !running_proofs
  in
  let l =
    if List.length l < !maximum_running_proofs then
      begin try
	let (callback,pre_call) = Queue.pop proof_attempts_queue in
	callback Running;
	let call = pre_call () in
	(callback,call)::l
      with Queue.Empty -> l
      end
    else l
  in
  running_proofs := l;
  let continue =
    match l with
      | [] ->
(*
          eprintf "Info: timeout_handler stopped@.";
*)
          false
      | _ -> true
  in
  timeout_handler_activated := continue;
  timeout_handler_running := false;
  continue


let run_timeout_handler () =
  if !timeout_handler_activated then () else
    begin
      timeout_handler_activated := true;
(*
      eprintf "Info: timeout_handler started@.";
*)
      O.timeout ~ms:100 timeout_handler
    end

(* idle handler *)


type action =
  | Action_proof_attempt of bool * int * int * in_channel option * string * Driver.driver *
    (proof_attempt_status -> unit) * Task.task
  | Action_delayed of (unit -> unit)

let actions_queue = Queue.create ()

let idle_handler_activated = ref false

let idle_handler () =
  try
    begin
      match Queue.pop actions_queue with
	| Action_proof_attempt(debug,timelimit,memlimit,old,command,driver,
			       callback,goal) ->
	    callback Scheduled;
	    if debug then
	      Format.eprintf "Task for prover: %a@."
		(Driver.print_task driver) goal;
	    let pre_call =
	      Driver.prove_task ?old ~command ~timelimit ~memlimit driver goal
	    in
	    Queue.push (callback,pre_call) proof_attempts_queue;
	    run_timeout_handler ()
        | Action_delayed callback -> callback ()
    end;
    true
  with Queue.Empty ->
    idle_handler_activated := false;
(*
    eprintf "Info: idle_handler stopped@.";
*)
    false

let run_idle_handler () =
  if !idle_handler_activated then () else
    begin
      idle_handler_activated := true;
(*
      eprintf "Info: idle_handler started@.";
*)
      O.idle idle_handler
    end

(* main scheduling functions *)

let schedule_proof_attempt ~debug ~timelimit ~memlimit ?old
    ~command ~driver ~callback goal =
  Queue.push
    (Action_proof_attempt(debug,timelimit,memlimit,old,command,driver,
			callback,goal))
    actions_queue;
  run_idle_handler ()

let schedule_edition command callback =
  let precall =
    Call_provers.call_on_buffer ~command ~regexps:[] ~timeregexps:[]
      ~exitcodes:[(0,Call_provers.Unknown "")] ~filename:"" (Buffer.create 1)
  in
  callback Running;
  running_proofs := (callback, precall ()) :: !running_proofs;
  run_timeout_handler ()

let schedule_delayed_action callback =
  Queue.push (Action_delayed callback) actions_queue;
  run_idle_handler ()

let apply_transformation ~callback t task =
   match t.transformation with
    | Trans_one t ->
	callback [Trans.apply t task]
    | Trans_list t ->
	callback (Trans.apply t task)

let schedule_edit_proof ~debug:_ ~editor ~file ~driver ~callback goal =
  let old =
    if Sys.file_exists file
    then
      begin
	let backup = file ^ ".bak" in
        Sys.rename file backup;
        Some(open_in backup)
      end
    else None
  in
  let ch = open_out file in
  let fmt = formatter_of_out_channel ch in
  Driver.print_task ?old driver fmt goal;
  Util.option_iter close_in old;
  close_out ch;
  let command = editor ^ " " ^ file in
  schedule_edition command callback


(*******************************)
(* explanations *)
(****************)


  let expl_regexp = Str.regexp "expl:\\(.*\\)"

  let rec get_labels f =
    (match f.Term.f_node with
      | Term.Fbinop(Term.Fimplies,_,f) -> get_labels f
      | Term.Fquant(Term.Fforall,fq) ->
	  let (_,_,f) = Term.f_open_quant fq in get_labels f
      | Term.Flet(_,fb) ->
	  let (_,f) = Term.f_open_bound fb in get_labels f
      | Term.Fcase(_,[fb]) ->
	  let (_,f) = Term.f_open_branch fb in get_labels f
      | _ -> [])
    @ f.Term.f_label

  let get_explanation id fmla =
    let r = ref None in
(*
    let fl = Debug.lookup_flag "print_labels" in
    Debug.set_flag fl;
    Format.eprintf "searching expl in formula '%a'@." Pretty.print_fmla fmla;
*)
    List.iter
      (fun s ->
         if Str.string_match expl_regexp s 0 then
           begin
	     let e = Str.matched_group 1 s in
(*
	     Format.eprintf "found explanation: '%s'" e;
*)
	     r := Some e
	   end)
      (id.Ident.id_label @ get_labels fmla);
    !r


(**********************)
(*     check sum      *)
(**********************)

let task_checksum t =
  fprintf str_formatter "%a@." Pretty.print_task t;
  let s = flush_str_formatter () in
(*
  let tmp = Filename.temp_file "task" "out" in
  let c = open_out tmp in
  output_string c s;
  close_out c;
*)
  let sum = Digest.to_hex (Digest.string s) in
(*
  eprintf "task %s, sum = %s@." tmp sum;
*)
  sum



(******************************)
(* raw additions to the model *)
(******************************)

let raw_add_external_proof ~obsolete ~edit g p result =
  let key = O.create ~parent:g.goal_key () in
  let a = { prover = p;
            proof_goal = g;
            proof_key = key;
            proof_obsolete = obsolete;
	    proof_state = result;
            edited_as = edit;
          }
  in
  Hashtbl.add g.external_proofs p.prover_name a;
  let any = Proof_attempt a in
  !init_fun key any;
  !notify_fun any;
  (* !notify_fun (Goal g) ? *)
  a


(* [raw_add_goal parent name expl t] adds a goal to the given parent
   DOES NOT record the new goal in its parent, thus this should not be exported
*)
let raw_add_goal parent name expl topt =
  let parent_key = match parent with
    | Parent_theory mth -> mth.theory_key
    | Parent_transf mtr -> mtr.transf_key
  in
  let key = O.create ~parent:parent_key () in
  let sum = match topt with
    | None -> ""
    | Some t -> task_checksum t
  in
  let goal = { goal_name = name;
               goal_expl = expl;
	       parent = parent;
               task = topt ;
	       checksum = sum;
               goal_key = key;
               external_proofs = Hashtbl.create 7;
               transformations = Hashtbl.create 3;
               proved = false;
             }
  in
  let any = Goal goal in
  !init_fun key any;
  !notify_fun any;
  goal


(* [raw_add_transformation g name adds a transformation to the given goal g
   Adds no subgoals, thus this should not be exported
*)
let raw_add_transformation g trans =
  let parent = g.goal_key in
  let key = O.create ~parent () in
  let tr = { transf = trans;
	     parent_goal = g;
             transf_proved = false;
             transf_key = key;
             subgoals = [];
           }
  in
  Hashtbl.add g.transformations trans.transformation_name tr;
  let any = Transformation tr in
  !init_fun key any;
  !notify_fun any;
  tr

let raw_add_theory mfile thopt thname =
  let parent = mfile.file_key in
  let key = O.create ~parent () in
  let mth = { theory_name = thname;
	      theory = thopt;
              theory_key = key;
              theory_parent = mfile;
              goals = [] ;
              verified = false }
  in
  let any = Theory mth in
  !init_fun key any;
  !notify_fun any;
  mth



let add_theory mfile name th =
  let tasks = List.rev (Task.split_theory th None None) in
  let mth = raw_add_theory mfile (Some th) name in
  let goals =
    List.fold_left
      (fun acc t ->
         let id = (Task.task_goal t).Decl.pr_name in
         let name = id.Ident.id_string in
         let expl = get_explanation id (Task.task_goal_fmla t) in
         let goal = raw_add_goal (Parent_theory mth) name expl (Some t) in
         goal :: acc)
      []
      tasks
  in
  mth.goals <- List.rev goals;
  check_theory_proved mth;
  mth

let raw_add_file f =
  let key = O.create () in
  let mfile = { file_name = f;
                file_key = key;
                theories = [] ;
                file_verified = false }
  in
  all_files := !all_files @ [mfile];
  let any = File mfile in
  !init_fun key any;
  !notify_fun any;
  mfile

let current_env = ref None
let project_dir = ref ""

let read_file fn =
  let fn = Filename.concat !project_dir fn in
  let env = match !current_env with 
    | None -> assert false | Some e -> e 
  in
  let theories = Env.read_file env fn in
  let theories =
    Theory.Mnm.fold
      (fun name th acc ->
         match th.Theory.th_name.Ident.id_loc with
           | Some l -> (l,name,th)::acc
           | None   -> (Loc.dummy_position,name,th)::acc)
      theories []
  in
  List.sort
    (fun (l1,_,_) (l2,_,_) -> Loc.compare l1 l2)
    theories

let add_file f =
  let theories = read_file f in
  let mfile = raw_add_file f in
  let mths =
    List.fold_left
      (fun acc (_,name,t) ->
         let mth = add_theory mfile name t in
         mth :: acc)
      [] theories
  in
  mfile.theories <- List.rev mths;
  check_file_verified mfile


let file_exists fn =
  List.exists (fun f -> f.file_name = fn) !all_files





(**********************************)
(* reload a file                  *)
(**********************************)

let rec reimport_any_goal _parent gid _gname t goal __goal_obsolete =
  let _info = get_explanation gid (Task.task_goal_fmla t) in
  goal.task <- Some t;
  goal
(*
  let goal = raw_add_goal parent gname info t in
  let proved = ref false in
  let external_proofs = Db.external_proofs db_goal in
  Db.Hprover.iter
    (fun pid a ->
       let pname = Db.prover_name pid in
       try
         let p = Util.Mstr.find pname gconfig.provers in
         let s,t,o,edit = Db.status_and_time a in
         if goal_obsolete && not o then Db.set_obsolete a;
         let obsolete = goal_obsolete or o in
         let s = match s with
           | Db.Undone -> Call_provers.HighFailure
           | Db.Done r ->
	       if r = Call_provers.Valid then
		 if not obsolete then proved := true;
	       r
         in
	 let r = { Call_provers.pr_answer = s;
		   Call_provers.pr_output = "";
		   Call_provers.pr_time = t;
		 }
	 in
         let (_pa : Model.proof_attempt) =
           Helpers.add_external_proof_row ~obsolete ~edit goal p a
	     (Gscheduler.Done r)
         in
         ((* something TODO ?*))
       with Not_found ->
         eprintf
           "Warning: prover %s appears in database but is not installed.@."
           pname
    )
    external_proofs;
  let transformations = Db.transformations db_goal in
  Db.Htransf.iter
    (fun tr_id tr ->
       let trname = Db.transf_name tr_id in
       eprintf "Reimporting transformation %s for goal %s @." trname gname;
       let trans = trans_of_name trname in
       let subgoals = apply_trans trans t in
       let mtr = Helpers.add_transformation_row goal tr trname in
       let db_subgoals = Db.subgoals tr in
       let reimported_goals,db_subgoals,_ =
         List.fold_left
           (fun (acc,db_subgoals,count) subtask ->
              let id = (Task.task_goal subtask).Decl.pr_name in
              let subgoal_name = gname ^ "." ^ (string_of_int count) in
              let sum = task_checksum subtask in
              let subtask_db,db_subgoals =
                try
		  let g = Util.Mstr.find sum db_subgoals in
		  (* a subgoal has the same check sum *)
		  (Some g, Util.Mstr.remove sum db_subgoals)
                with Not_found -> None,db_subgoals
              in
              ((count,id,subgoal_name,subtask,sum,subtask_db) :: acc,
	       db_subgoals,
	       count+1))
           ([],db_subgoals,1) subgoals
       in
       let other_goals =
	 Util.Mstr.fold
	   (fun _ g acc -> (Db.goal_name g,g)::acc)
	   db_subgoals
	   []
       in
       let other_goals =
	 List.sort (fun (s1,_) (s2,_) -> String.compare s1 s2) other_goals
       in
       let rec merge_goals new_goals old_goals proved acc =
	 match new_goals with
	   | [] -> acc, proved
	   | (_,id,subgoal_name,subtask,sum,g_opt)::rem ->
	       let db_g,subgoal_obsolete,old_goals =
		 match g_opt with
		   | Some g -> g,false,old_goals
		   | None ->
		       match old_goals with
			 | [] ->
			     (* create a new goal in db *)
			     Db.add_subgoal tr subgoal_name sum,
			     false, old_goals
			 | (_goal_name,g) :: r ->
			     g, true, r
	       in
               let subgoal,subgoal_proved =
                 reimport_any_goal
                   (Model.Transf mtr) id
                   subgoal_name subtask db_g subgoal_obsolete
              in
              merge_goals rem old_goals (proved && subgoal_proved)
		(subgoal :: acc)
       in
       let goals, subgoals_proved =
	 merge_goals (List.rev reimported_goals) other_goals true []
       in
(*
       let goals,_,subgoals_proved =
         List.fold_left
           (fun (acc,count,proved) subtask ->
              let _id = (Task.task_goal subtask).Decl.pr_name in
              let subgoal_name = gname ^ "." ^ (string_of_int count) in
              let sum = task_checksum subtask in
              let subtask_db =
                try Util.Mstr.find sum db_subgoals
		  (* a subgoal has the same check sum *)
                with Not_found ->
		  (* otherwise, create a new one *)
                  Db.add_subgoal tr subgoal_name sum
              in
              let subgoal,subgoal_proved =
                reimport_any_goal
                  (Model.Transf mtr) subgoal_name subtask subtask_db
                  false (* subgoal_obsolete *)
              in
              (subgoal :: acc, count+1,proved && subgoal_proved))
           ([],1,true) subgoals
       in
*)
       mtr.Model.subgoals <- List.rev goals;
       if subgoals_proved (* TODO : && not obsolete *)
       then proved := true
    )
    transformations;
  if !proved then Helpers.set_proved ~propagate:false goal;
  goal,!proved

*)

let reimport_root_goal mth tname goals t : goal =
  (* re-imports database informations of a goal in theory mth (named tname)
     goals is a table, indexed by names of DB goals formerly known to be
     a in theory mth.  returns true whenever the task t is known to be
     proved *)
  let id = (Task.task_goal t).Decl.pr_name in
  let gname = id.Ident.id_string
  in
  let sum = task_checksum t in
  let goal, goal_obsolete =
    try
      let dbg = Util.Mstr.find gname goals in
      let db_sum = dbg.checksum in
      let goal_obsolete = sum <> db_sum in
      if goal_obsolete then
        begin
          eprintf "Goal %s.%s has changed@." tname gname;
(*
          Db.change_checksum dbg sum
*)
        end;
      dbg,goal_obsolete
    with Not_found ->
      assert false (* TODO *)
(*
      let dbg = Db.add_goal mth.Model.theory_db gname sum in
      dbg,false
*)
  in
  reimport_any_goal (Parent_theory mth) id gname t goal goal_obsolete

(* reloads a file *)

let reload_file mf =
  eprintf "[Reload] file '%s'@." mf.file_name;
  try
    let theories = read_file mf.file_name in
    let old_theories = List.fold_left
      (fun acc t -> Util.Mstr.add t.theory_name t acc)
      Util.Mstr.empty
      mf.theories 
    in
    let mths =
      List.fold_left
        (fun acc (_,tname,th) ->
	   eprintf "[Reload] theory '%s'@."tname;
	   let mth =
	     try
               let mth = Util.Mstr.find tname old_theories in
	       mth.theory <- Some th;
	       mth
	     with Not_found -> 
	       raw_add_theory mf (Some th) tname
	   in
	   let goals = List.fold_left
	     (fun acc g -> Util.Mstr.add g.goal_name g acc)
	     Util.Mstr.empty mth.goals 
	   in
	   let tasks = List.rev (Task.split_theory th None None) in
	   let goals = List.fold_left
	     (fun acc t ->
                let g = reimport_root_goal mth tname goals t in
                g::acc)
	     [] tasks
	   in
	   mth.goals <- List.rev goals;
	   (* TODO: what to do with remaining old theories?
	      for the moment they remain in the session
	   *)
	   check_theory_proved mth;
	   mth::acc
	)
        [] theories
    in
    (* TODO: detecter d'eventuelles vieilles theories, qui seraient donc
       dans [old_theories] mais pas dans [theories]
    *)
    mf.theories <- List.rev mths;
    check_file_verified mf
  with e ->
    eprintf "@[Error while reading file@ '%s':@ %a@.@]" mf.file_name
      Exn_printer.exn_printer e;
    exit 1
      

(* reloads all files *)
let reload_all () = List.iter reload_file !all_files

(****************************)
(*     session opening      *)
(****************************)

let load_result r =
  match r.Xml.name with
    | "result" ->
	let status = 
	  try List.assoc "status" r.Xml.attributes
	  with Not_found -> assert false
	in
	let answer = 
	  match status with
	    | "valid" -> Call_provers.Valid
	    | "invalid" -> Call_provers.Invalid
	    | "unknown" -> Call_provers.Unknown ""
	    | "timeout" -> Call_provers.Timeout
	    | "failure" -> Call_provers.Failure ""
	    | "highfailure" -> Call_provers.Failure ""
	    | s -> 
		eprintf "Session.load_result: unexpected status '%s'@."  s;
		assert false
	in
	let time =
	  try float_of_string (List.assoc "time" r.Xml.attributes)
	  with Not_found -> 0.0
	in
	{ Call_provers.pr_answer = answer;
	  Call_provers.pr_time = time;
	  Call_provers.pr_output = "";
	}
    | s -> 
	eprintf "Session.load_result: unexpected element '%s'@."  s;
	assert false
    
  
let rec load_goal ~env ~provers parent acc g = 
  match g.Xml.name with
    | "goal" ->
	let gname = 
	  try List.assoc "name" g.Xml.attributes
	  with Not_found -> assert false
	in
	let expl = 
	  try Some (List.assoc "expl" g.Xml.attributes)
	  with Not_found -> None
	in
	let mg = raw_add_goal parent gname expl None in
	List.iter (load_proof_or_transf ~env ~provers mg) g.Xml.elements;
	mg::acc
    | s -> 
	eprintf "Session.load_goal: unexpected element '%s'@."  s;
	assert false
	  

and load_proof_or_transf ~env ~provers mg a =
  match a.Xml.name with
    | "proof" -> 
	let prover = 
	  try List.assoc "prover" a.Xml.attributes
	  with Not_found -> assert false
	in
        let p = 
	  try
	    Util.Mstr.find prover provers 
	  with Not_found -> assert false (* TODO *)
	in
	let res = match a.Xml.elements with
	  | [r] -> Done (load_result r)
	  | [] -> Undone
	  | _ -> assert false
	in
	let edit = 
	  try List.assoc "edited" a.Xml.attributes
	  with Not_found -> assert false
	in
	let _pa = raw_add_external_proof ~obsolete:false 
	  ~edit mg p res
	in
	(* already done by raw_add_external_proof
	   Hashtbl.add mg.external_proofs prover pa *)
	()
    | "transf" -> 
	let trname = 
	  try List.assoc "name" a.Xml.attributes
	  with Not_found -> assert false
	in
        let tr = 
	  try 
	    lookup_transformation env trname 
	  with Not_found -> assert false (* TODO *)
	in
	let _proved = 
	  try List.assoc "proved" a.Xml.attributes
	  with Not_found -> assert false
	in
	let mtr = raw_add_transformation mg tr in
	mtr.subgoals <-
	  List.rev 
	  (List.fold_left 
	     (load_goal ~env ~provers (Parent_transf mtr)) 
	     [] a.Xml.elements);
	(* already done by raw_add_transformation
	   Hashtbl.add mg.transformations trname mtr *)
	()
    | s -> 
	eprintf "Session.load_proof_or_transf: unexpected element '%s'@."  s;
	assert false

let load_theory ~env ~provers mf acc th =
  match th.Xml.name with
    | "theory" ->
	let thname = 
	  try List.assoc "name" th.Xml.attributes
	  with Not_found -> assert false
	in
	let mth = raw_add_theory mf None thname in
	mth.goals <- 
	  List.rev 
	  (List.fold_left 
	     (load_goal ~env ~provers (Parent_theory mth)) 
	     [] th.Xml.elements);
	mth::acc
    | s -> 
	eprintf "Session.load_theory: unexpected element '%s'@."  s;
	assert false

let load_file ~env ~provers f =
  match f.Xml.name with
    | "file" ->
	let fn = 
	  try List.assoc "name" f.Xml.attributes
	  with Not_found -> assert false
	in
	let mf = raw_add_file fn in
	mf.theories <- 
	  List.rev 
	  (List.fold_left (load_theory ~env ~provers mf) [] f.Xml.elements)
    | s -> 
	eprintf "Session.load_file: unexpected element '%s'@."  s;
	assert false

let load_session ~env ~provers xml =
  let cont = xml.Xml.content in
  match cont.Xml.name with
    | "why3session" ->
	List.iter (load_file ~env ~provers) cont.Xml.elements
    | s -> 
	eprintf "Session.load_session: unexpected element '%s'@."  s;
	assert false
  
let db_filename = "why3session.xml"

let open_session ~env ~provers ~init ~notify dir = 
  match !current_env with
    | None ->
	init_fun := init; notify_fun := notify; 
	project_dir := dir; current_env := Some env;
	begin try
	  let xml = Xml.from_file (Filename.concat dir db_filename) in
	  load_session ~env ~provers xml;
	  reload_all ()
	with 
	  | Sys_error _ -> 
	      (* xml does not exist yet *)
	      ()
	  | Xml.Parse_error s ->
	      Format.eprintf "XML database corrupted, ignored (%s)@." s;
	      ()
	end
    | _ -> 
	eprintf "Session.open_session: session already opened@.";
	assert false

let save_session () = 
  match !current_env with
    | Some _ -> save (Filename.concat !project_dir db_filename)
    | None ->
	eprintf "Session.save_session: no session opened@.";
	assert false

(*****************************************************)
(* method: run a given prover on each unproved goals *)
(*****************************************************)

let redo_external_proof g a =
  (* check that the state is not Scheduled or Running *)
  let running = match a.proof_state with
    | Scheduled | Running -> true
    | Done _ | Undone | InternalFailure _ -> false
  in
  if running then ()
    (* info_window `ERROR "Proof already in progress" *)
  else
    let p = a.prover in
    let callback result =
      set_proof_state ~obsolete:false a result;
    in
    let old = if a.edited_as = "" then None else
      begin
	Format.eprintf "Info: proving using edited file %s@." a.edited_as;
	(Some (open_in a.edited_as))
      end
    in
    schedule_proof_attempt
      ~debug:false ~timelimit:10 ~memlimit:0
      ?old ~command:p.command ~driver:p.driver
      ~callback
      (get_task g)

let rec prover_on_goal p g =
  let id = p.prover_id in
  let a =
    try Hashtbl.find g.external_proofs id
    with Not_found ->
      raw_add_external_proof ~obsolete:false ~edit:"" g p Undone
  in
  let () = redo_external_proof g a in
  Hashtbl.iter
    (fun _ t -> List.iter (prover_on_goal p) t.subgoals)
    g.transformations

let rec prover_on_goal_or_children ~context_unproved_goals_only p g =
  if not (g.proved && context_unproved_goals_only) then
    begin
      let r = ref true in
      Hashtbl.iter
	(fun _ t ->
	   r := false;
           List.iter (prover_on_goal_or_children ~context_unproved_goals_only p)
             t.subgoals) g.transformations;
      if !r then prover_on_goal p g
    end

let run_prover ~context_unproved_goals_only pr a =
  match a with
    | Goal g -> 
	prover_on_goal_or_children ~context_unproved_goals_only pr g
    | Theory th -> 
	List.iter 
	  (prover_on_goal_or_children ~context_unproved_goals_only pr) 
	  th.goals
    | File file -> 
        List.iter
          (fun th ->
             List.iter 
	       (prover_on_goal_or_children ~context_unproved_goals_only pr)
	       th.goals)
          file.theories
    | Proof_attempt a ->
        prover_on_goal_or_children ~context_unproved_goals_only pr a.proof_goal
    | Transformation tr ->
        List.iter 
	  (prover_on_goal_or_children ~context_unproved_goals_only pr) 
	  tr.subgoals

(**********************************)
(* method: replay obsolete proofs *)
(**********************************)

let proof_successful a =
  match a.proof_state with
    | Done { Call_provers.pr_answer = Call_provers.Valid } -> true
    | _ -> false

let rec replay_on_goal_or_children ~context_unproved_goals_only g =
  Hashtbl.iter
    (fun _ a ->
       if a.proof_obsolete then
         if not context_unproved_goals_only || proof_successful a
         then redo_external_proof g a)
    g.external_proofs;
  Hashtbl.iter
    (fun _ t -> 
       List.iter 
	 (replay_on_goal_or_children ~context_unproved_goals_only) 
	 t.subgoals)
    g.transformations

let replay ~context_unproved_goals_only a =
  match a with
    | Goal g ->
        replay_on_goal_or_children ~context_unproved_goals_only g
    | Theory th ->
        List.iter 
	  (replay_on_goal_or_children ~context_unproved_goals_only)
	  th.goals
    | File file ->
        List.iter
          (fun th ->
             List.iter 
	       (replay_on_goal_or_children ~context_unproved_goals_only)
	       th.goals)
          file.theories
    | Proof_attempt a ->
        replay_on_goal_or_children ~context_unproved_goals_only a.proof_goal
    | Transformation tr ->
        List.iter 
	  (replay_on_goal_or_children ~context_unproved_goals_only)
	  tr.subgoals


(*****************************************************)
(* method: split selected goals *)
(*****************************************************)

let task_checksum t =
  fprintf str_formatter "%a@." Pretty.print_task t;
  let s = flush_str_formatter () in
(*
  let tmp = Filename.temp_file "task" "out" in
  let c = open_out tmp in
  output_string c s;
  close_out c;
*)
  let sum = Digest.to_hex (Digest.string s) in
(*
  eprintf "task %s, sum = %s@." tmp sum;
*)
  sum

let transformation_on_goal g tr =
  if not g.proved then
    let callback subgoals =
      let b =
 	match subgoals with
	  | [task] ->
              let s1 = task_checksum (get_task g) in
              let s2 = task_checksum task in
	      (*
                eprintf "Transformation returned only one task. sum before = %s, sum after = %s@." (task_checksum g.task) (task_checksum task);
                eprintf "addresses: %x %x@." (Obj.magic g.task) (Obj.magic task);
	      *)
              s1 <> s2
                (* task != g.task *)
	  | _ -> true
      in
      if b then
	let tr = raw_add_transformation g tr in
	let goal_name = g.goal_name in
	let fold =
	  fun (acc,count) subtask ->
	    let id = (Task.task_goal subtask).Decl.pr_name in
            let expl =
              get_explanation id (Task.task_goal_fmla subtask)
            in
	    let subgoal_name =
	      goal_name ^ "." ^ (string_of_int count)
	    in
	    let goal =
	      raw_add_goal (Parent_transf tr)
		subgoal_name expl (Some subtask)
	    in
	    (goal :: acc, count+1)
	in
	let goals,_ =
	  List.fold_left fold ([],1) subgoals
	in
	tr.subgoals <- List.rev goals
    in
    apply_transformation ~callback tr (get_task g)

let rec transform_goal_or_children ~context_unproved_goals_only tr g =
  if not g.proved then
    begin
      let r = ref true in
      Hashtbl.iter
	(fun _ t ->
	   r := false;
           List.iter 
	     (transform_goal_or_children ~context_unproved_goals_only tr)
             t.subgoals) 
	g.transformations;
      if !r then 
	schedule_delayed_action (fun () -> transformation_on_goal g tr)
    end


let transform ~context_unproved_goals_only tr a =
  let tr = transform_goal_or_children ~context_unproved_goals_only tr in
  match a with
    | Goal g -> tr g
    | Theory th -> List.iter tr th.goals
    | File file ->
        List.iter
          (fun th -> List.iter tr th.goals)
          file.theories
    | Proof_attempt a -> tr a.proof_goal
    | Transformation t -> List.iter tr t.subgoals



(*****************************)
(* method: edit current goal *)
(*****************************)


let ft_of_th th =
  (Filename.basename th.theory_parent.file_name,
   (*
     th.theory.Theory.th_name.Ident.id_string
   *)
   th.theory_name
  )

let rec ft_of_goal g =
  match g.parent with
    | Parent_transf tr -> ft_of_goal tr.parent_goal
    | Parent_theory th -> ft_of_th th

let ft_of_pa a =
  ft_of_goal a.proof_goal

let edit_proof ~default_editor ~project_dir a =
  (* check that the state is not Scheduled or Running *)
  let running = match a.proof_state with
    | Scheduled | Running -> true
    | Undone | Done _ | InternalFailure _ -> false
  in
  if running then ()
(*
    info_window `ERROR "Edition already in progress"
*)
  else
    let g = a.proof_goal in
    let t = (get_task g) in
    let driver = a.prover.driver in
    let file =
      match a.edited_as with
        | "" ->
	    let (fn,tn) = ft_of_pa a in
	    let file = Driver.file_of_task driver
              (Filename.concat project_dir fn) tn t
	    in
	    (* Uniquify the filename if it exists on disk *)
	    let i =
              try String.rindex file '.'
              with _ -> String.length file
	    in
	    let name = String.sub file 0 i in
	    let ext = String.sub file i (String.length file - i) in
	    let i = ref 1 in
	    while Sys.file_exists
	      (name ^ "_" ^ (string_of_int !i) ^ ext) do
		incr i
	    done;
	    let file = name ^ "_" ^ (string_of_int !i) ^ ext in
	    a.edited_as <- file;
	    file
	| f -> f
    in
    let callback res =
      match res with
        | Done _ ->
            set_proof_state ~obsolete:false a Undone
        | _ ->
            set_proof_state ~obsolete:false a res
    in
    let editor =
      match a.prover.editor with
        | "" -> default_editor
        | s -> s
    in
    schedule_edit_proof ~debug:false ~editor
      ~file
      ~driver
      ~callback
      t

(*************)
(* removing  *)
(*************)

(*

let remove_proof_attempt a =
  Db.remove_proof_attempt a.proof_db;
  let (_:bool) = goals_model#remove a.proof_row in
  let g = a.proof_goal in
  Hashtbl.remove g.external_proofs a.prover.prover_id;
  Helpers.check_goal_proved g

let remove_transf t =
  (* TODO: remove subgoals first !!! *)
  Db.remove_transformation t.transf_db;
  let (_:bool) = goals_model#remove t.transf_row in
  let g = t.parent_goal in
  Hashtbl.remove g.transformations "split" (* hack !! *);
  Helpers.check_goal_proved g


let confirm_remove_row r =
  let row = goals_model#get_iter r in
  match goals_model#get ~row ~column:index_column with
    | Row_goal _g ->
	info_window `ERROR "Cannot remove a goal"
    | Row_theory _th ->
	info_window `ERROR "Cannot remove a theory"
    | Row_file _file ->
	info_window `ERROR "Cannot remove a file"
    | Row_proof_attempt a ->
	info_window
	  ~callback:(fun () -> remove_proof_attempt a)
	  `QUESTION
	  "Do you really want to remove the selected proof attempt?"
    | Row_transformation tr ->
	info_window
	  ~callback:(fun () -> remove_transf tr)
	  `QUESTION
	  "Do you really want to remove the selected transformation
and all its subgoals?"

let confirm_remove_selection () =
  match goals_view#selection#get_selected_rows with
    | [] -> ()
    | [r] -> confirm_remove_row r
    | _ ->
        info_window `INFO "Please select exactly one item to remove"


let rec clean_goal g =
  if g.proved then
    begin
      Hashtbl.iter
        (fun _ a ->
           if a.proof_obsolete || not (proof_successful a) then
             remove_proof_attempt a)
        g.external_proofs;
      Hashtbl.iter
        (fun _ t ->
           if not t.transf_proved then
             remove_transf t)
        g.transformations
    end
  else
    Hashtbl.iter
      (fun _ t -> List.iter clean_goal t.subgoals)
      g.transformations


let clean_row r =
  let row = goals_model#get_iter r in
  match goals_model#get ~row ~column:index_column with
    | Row_goal g -> clean_goal g
    | Row_theory th ->
        List.iter clean_goal th.goals
    | Row_file file ->
        List.iter
          (fun th ->
             List.iter clean_goal th.goals)
          file.theories
    | Row_proof_attempt a ->
        clean_goal a.proof_goal
    | Row_transformation tr ->
        List.iter clean_goal tr.subgoals


*)

end

(*
Local Variables:
compile-command: "unset LANG; make -C ../.. bin/why3ide.byte"
End:
*)
