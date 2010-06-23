(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) 2010-                                                   *)
(*    Francois Bobot                                                      *)
(*    Jean-Christophe Filliatre                                           *)
(*    Johannes Kanig                                                      *)
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

(** Typing environments *)

open Util
open Ty
open Term
open Theory
open Env

val retrieve : string list -> retrieve_theory
  (** creates a new typing environment for a given loadpath *)

(** incremental parsing *)

val add_decl : env -> theory Mnm.t -> theory_uc -> Ptree.decl -> theory_uc

(** error reporting *)

type error

exception Error of error

val report : Format.formatter -> error -> unit


(******************************************************************************)
(** The following is exported for program typing (src/programs/pgm_typing.ml) *)
(******************************************************************************)

val specialize_fsymbol : 
  Ptree.qualid -> theory_uc -> lsymbol * Denv.dty list * Denv.dty

val specialize_psymbol : 
  Ptree.qualid -> theory_uc -> lsymbol * Denv.dty list

val specialize_tysymbol : 
  Loc.position -> Ptree.qualid -> theory_uc -> Ty.tysymbol * int

type denv

val create_denv : theory_uc -> denv

val find_user_type_var : string -> denv -> Denv.type_var

val mem_var : string -> denv -> bool
val find_var : string -> denv -> Denv.dty
val add_var : string -> Denv.dty -> denv -> denv

val type_term : denv -> vsymbol Mstr.t -> Ptree.lexpr -> term
val type_fmla : denv -> vsymbol Mstr.t -> Ptree.lexpr -> fmla

val dty : denv -> Ptree.pty -> Denv.dty
val dterm : denv -> Ptree.lexpr -> Denv.dterm
val dfmla : denv -> Ptree.lexpr -> Denv.dfmla
val dpat : denv -> Ptree.pattern -> denv * Denv.dpattern
val dpat_list : 
  denv -> Denv.dty list -> Ptree.pattern list -> denv * Denv.dpattern list

val qloc : Ptree.qualid -> Loc.position

val ts_tuple : int -> Ty.tysymbol
val fs_tuple : int -> Term.lsymbol

val with_tuples : 
  ?reset:bool -> (theory_uc -> 'a -> 'b) -> theory_uc -> 'a -> 'b



