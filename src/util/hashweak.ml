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

module Make (S : Util.Tagged) =
struct

  type 'a t = { ht : (int,'a) Hashtbl.t;
                final : S.t -> unit}

  let wget w = Weak.get w 0
  let wref v =
    let w = Weak.create 1 in
    Weak.set w 0 (Some v);
    w

  let create i =
    let ht = Hashtbl.create i in
    let w = wref ht in
    let final x =
      match wget w with
        | None -> ()
        | Some h -> Hashtbl.remove h (S.tag x) in
    { ht = ht; final = final }

  let find h e = Hashtbl.find h.ht (S.tag e)

  let mem h e = Hashtbl.mem h.ht (S.tag e)


  exception AlreadyBounded

  let add h e v =
    let tag = S.tag e in
    if Hashtbl.mem h.ht tag
    then raise AlreadyBounded
    else begin
      Gc.finalise h.final e;
      Hashtbl.replace h.ht tag v
    end




end

