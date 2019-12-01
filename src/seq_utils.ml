module Numbers = Numbers

let cons x s = fun () -> Seq.Cons (x, s)

let rec append s1 s2 =
  match s1 () with
  | Seq.Nil -> s2
  | Seq.Cons (x, ss1) ->
    cons x (append ss1 s2)

let rec concat (ss : 'a Seq.t Seq.t) : 'a Seq.t =
  Seq.flat_map (fun s -> s)
    ss

let iteri (f : int -> 'a -> unit) (s : 'a Seq.t) : unit =
  let rec aux f s i =
    match s () with
    | Seq.Nil -> ()
    | Seq.Cons (x, rest) ->
      f i x;
      aux f rest (succ i)
  in
  aux f s 0

let mapi (f : int -> 'a -> 'b) (s : 'a Seq.t) : 'b Seq.t =
  let rec aux f s i =
    match s () with
    | Seq.Nil -> Seq.empty
    | Seq.Cons (x, rest) -> cons (f i x) (aux f rest (i + 1))
  in
  aux f s 0

let rec iter2 (f : 'a -> 'b -> unit) (s1 : 'a Seq.t) (s2 : 'b Seq.t) : unit =
  match s1 (), s2 () with
  | Seq.Nil, Seq.Nil -> ()
  | Seq.Nil, _ | _, Seq.Nil -> raise (Invalid_argument "Seq_utils.iter2")
  | Seq.Cons (x1, rest1), Seq.Cons (x2, rest2) ->
    f x1 x2;
    iter2 f rest1 rest2

let rec map2 (f : 'a -> 'b -> 'c) (s1 : 'a Seq.t) (s2 : 'b Seq.t) : 'c Seq.t =
  match s1 (), s2 () with
  | Seq.Nil, Seq.Nil -> Seq.empty
  | Seq.Nil, _ | _, Seq.Nil -> raise (Invalid_argument "Seq_utils.map2")
  | Seq.Cons (x1, rest1), Seq.Cons (x2, rest2) ->
    cons (f x1 x2) (map2 f rest1 rest2)

let rec fold_left2 (f : 'a -> 'b -> 'c -> 'a) (acc : 'a) (s1 : 'b Seq.t) (s2 : 'c Seq.t) :
  'a =
  match s1 (), s2 () with
  | Seq.Nil, Seq.Nil -> Seq.empty
  | Seq.Nil, _ | _, Seq.Nil -> raise (Invalid_argument "Seq_utils.fold_left2")
  | Seq.Cons (x1, rest1), Seq.Cons (x2, rest2) ->
    let acc = f acc x1 x2 in
    fold_left2 f acc rest1 rest2

let rec for_all (f : 'a -> bool) (s : 'a Seq.t) : bool =
  match s () with
  | Seq.Nil -> true
  | Seq.Cons (x, rest) ->
    f x && for_all f rest

let rec exists (f : 'a -> bool) (s : 'a Seq.t) : bool =
  match s () with
  | Seq.Nil -> false
  | Seq.Cons (x, rest) ->
    f x || exists f rest

let rec for_all2 (f : 'a -> 'b -> bool) (s1 : 'a Seq.t) (s2 : 'b Seq.t) : bool =
  match s1 (), s2 () with
  | Seq.Nil, Seq.Nil -> true
  | Seq.Nil, _ | _, Seq.Nil -> raise (Invalid_argument "Seq_utils.for_all2")
  | Seq.Cons (x1, rest1), Seq.Cons (x2, rest2) ->
    f x1 x2 && for_all2 f rest1 rest2

let rec exists2 (f : 'a -> 'b -> bool) (s1 : 'a Seq.t) (s2 : 'b Seq.t) : bool =
  match s1 (), s2 () with
  | Seq.Nil, Seq.Nil -> false
  | Seq.Nil, _ | _, Seq.Nil -> raise (Invalid_argument "Seq_utils.for_all2")
  | Seq.Cons (x1, rest1), Seq.Cons (x2, rest2) ->
    f x1 x2 || exists2 f rest1 rest2

let mem e s = exists (fun x -> x = e) s
