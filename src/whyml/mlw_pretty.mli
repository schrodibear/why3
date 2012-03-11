(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) 2010-2011                                               *)
(*    François Bobot                                                      *)
(*    Jean-Christophe Filliâtre                                           *)
(*    Claude Marché                                                       *)
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

open Format
open Why3
open Mlw_ty
open Mlw_expr
open Mlw_decl
open Mlw_module

val forget_all      : unit -> unit     (* flush id_unique *)
val forget_regs     : unit -> unit     (* flush id_unique for regions *)
val forget_tvs_regs : unit -> unit     (* flush for type vars and regions *)
val forget_pv       : pvsymbol -> unit (* flush for a program variable *)

val print_reg : formatter -> region -> unit       (* region *)
val print_pv  : formatter -> pvsymbol -> unit     (* program variable *)

val print_its : formatter -> itysymbol -> unit    (* type symbol *)
val print_mod : formatter -> modul -> unit        (* module name *)

val print_ity : formatter -> ity -> unit          (* individual type *)
val print_pvty : formatter -> pvsymbol -> unit    (* variable : type *)

val print_type_decl : formatter -> ity_decl -> unit
val print_next_type_decl : formatter -> ity_decl -> unit

val print_pdecl : formatter -> pdecl -> unit

val print_module : formatter -> modul -> unit
