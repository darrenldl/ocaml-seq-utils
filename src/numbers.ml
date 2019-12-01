let zero_to_n_exc n : int Seq.t =
  let rec aux cur n =
    if cur < n then fun () -> Seq.Cons (cur, aux (cur + 1) n) else Seq.empty
  in
  aux 0 n

let zero_to_n_inc n = zero_to_n_exc (n + 1)

let mod_int n =
  let rec aux cur n =
    if cur < n then fun () -> Seq.Cons (cur, aux (cur + 1) n) else aux 0 n
  in
  aux 0 n
