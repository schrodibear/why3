module Make (S:Util.Sstruct) = 
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

  let add h e v =
    let tag = S.tag e in
    let mem = Hashtbl.mem h.ht tag in
    Hashtbl.replace h.ht tag v;
    if not mem then Gc.finalise h.final e
end

