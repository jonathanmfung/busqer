module IntMap = Map.Make (Int)

(* https://arxiv.org/pdf/1304.7465 pg 6 *)
type bihist = { (* l : int;  *) t : int; freqs : float IntMap.t }

let make_freqs (xs : int list) : float IntMap.t =
  let counts =
    List.fold_left
      (fun acc x ->
        IntMap.update x
          (function None -> Some 1 | Some y -> Some (succ y))
          acc)
      IntMap.empty xs
  in
  let n_tot = Float.of_int @@ IntMap.fold (fun _k v acc -> v + acc) counts 0 in
  IntMap.map (fun v -> Float.of_int v /. n_tot) counts

let split_class (freqs : float IntMap.t) (t : int) :
    float IntMap.t * float IntMap.t =
  IntMap.partition (fun k _v -> k <= t) freqs

let gen_bihist ~l (data : int list) : (int * bihist) list =
  let ts = List.init l (fun i -> i) in
  List.mapi (fun i t -> (i, { (* l; *) t; freqs = make_freqs data })) ts

let argmax (bs : (int * bihist) list) (f : bihist -> 'a) : int =
  let max (i1, x1) (i2, x2) = if f x1 < f x2 then (i2, x2) else (i1, x1) in
  let tmax, _bhmax =
    List.fold_left max (0, { (* l = 0; *) t = 0; freqs = IntMap.empty }) bs
  in
  tmax

let btwn_class_var (bh : bihist) : float =
  let pair_map (a, b) f = (f a, f b) in
  let c0, c1 = split_class bh.freqs bh.t in
  let p0, p1 =
    let class_pr ps = IntMap.fold (fun _k v acc -> v +. acc) ps 0.0 in
    pair_map (c0, c1) class_pr
  in
  let mu_threshold, mu_full =
    let moment fs =
      IntMap.fold (fun k v acc -> (Float.of_int k *. v) +. acc) fs 0.0
    in
    pair_map (c0, c1) moment
  in
  (* let mu0 = mu_threshold /. p0 in *)
  (* let mu1 = (mu_full -. mu_threshold) /. p1 in *)
  Float.pow ((mu_full *. p0) -. mu_threshold) 2.0 /. (p0 *. p1)

let binify (xs : float list) ~(l : int) : int list =
  let min = List.fold_left Float.min Float.infinity xs in
  let max = List.fold_left Float.max Float.neg_infinity xs in
  List.map
    (fun y -> Int.of_float @@ (Float.of_int l *. (y -. min) /. (max -. min)))
    xs
