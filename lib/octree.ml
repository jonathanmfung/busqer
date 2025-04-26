type rgb = Rgb of (int * int * int)

module BoolTripleMap = Map.Make (struct
  type t = bool * bool * bool

  let compare (x1, y1, z1) (x2, y2, z2) =
    match Stdlib.compare x1 x2 with
    | 0 -> (
        match Stdlib.compare y1 y2 with 0 -> Stdlib.compare z1 z2 | c -> c)
    | c -> c
end)

type t = Node of { data : t BoolTripleMap.t; count : int }

let empty = Node { data = BoolTripleMap.empty; count = 0 }

let int_to_bits (n : int) : bool Seq.t =
  (* MSB is head *)
  assert (n < 1 lsl 8);
  let len = 8 in
  let f i =
    let mask = 1 lsl (pred len - i) in
    n land mask = mask
  in
  Seq.init len f

let iter3 f xs ys zs =
  Seq.iter (fun (x, (y, z)) -> f x y z) (Seq.zip xs (Seq.zip ys zs))

let octal_sum (a : bool) (b : bool) (c : bool) =
  (4 * Bool.to_int a) + (2 * Bool.to_int b) + (1 * Bool.to_int c)

let rec equal (a : t) (b : t) : bool =
  match (a, b) with
  | ( Node { data = data_a; count = count_a },
      Node { data = data_b; count = count_b } ) ->
     count_a = count_b && BoolTripleMap.equal equal data_a data_b

let height (t : t) : int =
  let rec go n t' =
    match t' with
      Node {data; _} ->
      if BoolTripleMap.is_empty data
      then 0
      else
        BoolTripleMap.fold (fun _k v acc -> go (succ n) v + acc) data 0
  in go 0 t

let rec insert_seq (t : t) (r_b : bool Seq.t) g_b b_b : t =
  match (r_b (), g_b (), b_b ()) with
  | Seq.Cons (r, rs), Seq.Cons (g, gs), Seq.Cons (b, bs) -> (
      let key = (r, g, b) in
      match t with
      | Node { data; count } -> (
          match BoolTripleMap.find_opt key data with
          | None ->
              let data' =
                BoolTripleMap.add key (insert_seq empty rs gs bs) data
              in
              Node { data = data'; count = succ count }
          | Some (Node _ as child) ->
              let data' =
                BoolTripleMap.add key (insert_seq child rs gs bs) data
              in
              Node { data = data'; count }))
  | Seq.Nil, Seq.Nil, Seq.Nil -> Node { data = BoolTripleMap.empty; count = 0 }
  | _, _, _ -> failwith "rgb sequences of different lengths"

let insert (t : t) (Rgb (r_v, g_v, b_v) : rgb) : t =
  let r_b = int_to_bits r_v in
  let g_b = int_to_bits g_v in
  let b_b = int_to_bits b_v in
  insert_seq t r_b g_b b_b

let to_string (t : t) : string =
  let rec go n (t' : t) =
    match t' with
    | Node { data; count } ->
        if BoolTripleMap.is_empty data then "EMPTY"
        else
          BoolTripleMap.fold
            (fun (x, y, z) v acc ->
              Printf.sprintf "k=%i, v=[%s], ncount=%i;" (octal_sum x y z)
                ("\n" ^ String.make n ' ' ^ go (succ n) v)
                count
              ^ acc)
            data ""
  in
  go 1 t

module RgbMap = Map.Make (struct
  type t = rgb

  let compare (Rgb (r1, g1, b1)) (Rgb (r2, g2, b2)) =
    match Stdlib.compare r1 r2 with
    | 0 -> (
        match Stdlib.compare g1 g2 with 0 -> Stdlib.compare b1 b2 | c -> c)
    | c -> c
end)

let num_colors (_t : t) : int = 32

let reduce_step (t : t): t=
  (* let candidate = node with max count sum of children
     search for all leaf nodes, backtrack to parent, sum children counts, agg max
     then dfs for this value
   *)
  t

(* search for all nodes at deepest level.
   construct map of all childrens' colors to parent's
   set parent.data = BoolTripleMap.empty
 *)

let reduce (t : t) (n : int) : t option =
  let seq = Seq.iterate reduce_step t in
  Seq.find (fun x -> num_colors x < n) seq

(*
  00000000: 000
  00000001: 100
  00000002: 100
  ...
  00000010: 000
  00000011: 000
 *)
