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

type env

val create : string list -> env
  (** creates a new typing environment for a given loadpath *)

val add_theory : env -> Ptree.theory -> Theory.theory * env
  (** adds a local theory into the environment *)

val add_theories : env -> Ptree.theory list -> env

val find_theory : env -> Ptree.qualid -> Theory.theory
  (** searches for a theory using the environment's loadpath *)

val list_theory : env -> Theory.theory list

(** error reporting *)

type error

exception Error of error

val report : Format.formatter -> error -> unit

