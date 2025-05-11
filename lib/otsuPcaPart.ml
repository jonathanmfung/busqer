(* Maybe just try this naive approach to clustering,
   since further k-means does not lower MSE that much *)

(* Then can implement google's Score process to cull the generated cluster list *)

(*
PCA-Part & Var-Part
https://www.cse.iitd.ac.in/~rjaiswal/2015/col870/Homework/Papers/pca-part.pdf

Thus, PCA-Part chooses φp to be the component which contributes to the
largest SSE. The largest eigenvector of the covariance matrix, is the direc-
tion which contributes to the largest SSE [12]. Hence, PCA-Part picks the
largest eigenvector of the covariance matrix as the direction for projecting.
*)

open Lacaml.S

type data_t = mat

type cluster_node =
  | Leaf of data_t
  | Node of { centroid : vec; left : cluster_node; right : cluster_node }

let leaf d = Leaf d

type cluster_path =
  | Left of { centroid : vec; rctx : cluster_node }
  | Right of { centroid : vec; lctx : cluster_node }

type zipper = Zip of { tree : cluster_node; thread : cluster_path list }

let print (c : cluster_node) : unit =
  let rec go n c' =
    match c' with
    | Leaf x ->
        Format.printf "@[<2>%i:@\n@\n%a@]@.\n" n (Lacaml.Io.pp_lfmat ()) x
    | Node { left; right; _ } ->
        go (succ n) left;
        go (succ n) right
  in
  go 0 c

let mkzip (t : cluster_node) : zipper = Zip { tree = t; thread = [] }

let go_left (Zip { tree; thread = old_thread }) : zipper =
  match tree with
  | Leaf _ -> invalid_arg "go_left Leaf"
  | Node { centroid; left; right } ->
      Zip
        { thread = Left { centroid; rctx = right } :: old_thread; tree = left }

let go_right (Zip { tree; thread = old_thread }) : zipper =
  match tree with
  | Leaf _ -> invalid_arg "go_right Leaf"
  | Node { centroid; left; right } ->
      Zip
        { thread = Right { centroid; lctx = left } :: old_thread; tree = right }

let go_up (Zip { tree; thread }) : zipper =
  match thread with
  | [] -> invalid_arg "go_up already at top"
  | Left { rctx; centroid } :: ts ->
      Zip { tree = Node { centroid; left = tree; right = rctx }; thread = ts }
  | Right { lctx; centroid } :: ts ->
      Zip { tree = Node { centroid; left = lctx; right = tree }; thread = ts }

let rec unzip (Zip { tree; thread } : zipper) : cluster_node =
  match thread with
  | [] -> tree
  | Left { rctx; centroid } :: ts ->
      unzip
        (Zip
           { tree = Node { centroid; left = tree; right = rctx }; thread = ts })
  | Right { lctx; centroid } :: ts ->
      unzip
        (Zip
           { tree = Node { centroid; left = lctx; right = tree }; thread = ts })

let grow_leaf (Zip { tree; thread }) (f : data_t -> vec * data_t * data_t) :
    zipper =
  match tree with
  | Node _ -> invalid_arg "grow_leaf Node"
  | Leaf data ->
      let centroid, left, right = f data in
      Zip
        {
          tree = Node { centroid; left = Leaf left; right = Leaf right };
          thread;
        }

let canon z = mkzip @@ unzip z

let focus_max (z : zipper) (f : data_t -> float) : zipper =
  let max (z1 : zipper) (z2 : zipper) : zipper =
    match (z1, z2) with
    | Zip { tree = Leaf x1; _ }, Zip { tree = Leaf x2; _ } ->
        if f x2 < f x1 then z1 else z2
    | Zip { tree = Leaf _; _ }, z2 ->
        max (max z1 (go_left z2)) (max z1 (go_right z2))
    | z1, Zip { tree = Leaf _; _ } ->
        max (max z2 (go_left z1)) (max z2 (go_right z1))
    | _ -> failwith "focus_max::max Node"
  in
  let rec go (curr_max : zipper) (Zip { tree; _ } as z) =
    match tree with
    | Leaf _ -> max curr_max z
    | Node _ -> go (go_left z) (go_right z)
  in
  go (mkzip (Leaf Mat.empty)) (canon z)

(* NOTE: Matrix is stored as columns are vectors, n_rows=3 *)

let principal_component (m : mat) : vec =
  (* TODO: double check that pca of data is same as eigenvector of data
     Or if it should be eigenvector of covariance matrix
     "Hence, PCA-Part picks the largest eigenvector of the covariance matrix as the direction for projecting."
  *)
  (* TODO: Check if eigenvector is normalized or not *)
  let _, _, _, right_evec = geev m in
  (Mat.to_col_vecs right_evec).(0)

let mean (m : mat) : vec =
  (* input m has observations as columns *)
  let sum = Mat.fold_cols (Vec.add ~ofsy:1 ~incy:1) (Vec.make0 3) m in
  let denom = Vec.make 3 (Int.to_float @@ Mat.dim2 m) in
  Vec.div sum denom

let var_mat (m : mat) : mat =
  (* input m has observations as columns *)
  let num_vars = Mat.dim1 m in
  let m_arr =  Mat.to_array @@ Mat.transpose_copy m in (* obs are rows *)
  let m_mean = Vec.to_array (mean m) in
  (* https://en.wikipedia.org/wiki/Covariance#Calculating_the_sample_covariance *)
  Mat.init_rows num_vars num_vars (fun j k ->
      (* for some reason init_rows is indexed at 1 *)
      let k = k - 1 in
      let j = j - 1 in
      Array.fold_left
        (fun acc obs ->
          ((obs.(j) -. m_mean.(j)) *. (obs.(k) -. m_mean.(k))) +. acc)
        0.0 m_arr)

let sum_sqr_diff (m : mat) (v : vec) : float =
  let diffs = Array.map (Vec.ssqr_diff v) (Mat.to_col_vecs m) in
  Array.fold_left Float.add 0.0 diffs

let focus_max_sse z = focus_max z (fun m -> sum_sqr_diff m (mean m))

let project ~(onto : vec) (v : vec) : vec =
  let a = dot v onto /. nrm2 onto in
  let out = copy onto in
  scal a out;
  out

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

let split_step (z : zipper) : zipper =
  (* let split_data m = *)
  (*   let tuplefy (a, b) = (Mat.of_col_vecs_list a, Mat.of_col_vecs_list b) in *)
  (*   let threshold = mean m in *)
  (*   tuplefy (List.partition (fun v -> v < threshold) (Mat.to_col_vecs_list m)) *)
  (* in *)
  let combine_with xs ys f =
    List.map (fun (x, y) -> f x y) (List.combine xs ys)
  in
  let split_otsu m =
    let l = 256 in
    let mean = mean m in
    let zero_mean =
      List.map (fun x -> Vec.sub x mean) (Mat.to_col_vecs_list m)
    in
    let pca = principal_component (var_mat m) in
    (* Map of vec : projection : bin *)
    (* NOTE: let a' = project a onto b -> b - a' = [x; 0;...] AKA parallel *)
    let vec_proj_assoc =
      List.map
        (fun v -> (v, Array.get (Vec.to_array (project ~onto:pca v)) 0))
        zero_mean
    in
    let bins = binify ~l (List.map (fun (_v, p) -> p) vec_proj_assoc) in
    let vpb_assoc =
      combine_with vec_proj_assoc bins (fun (v, p) b -> (v, p, b))
    in
    let tstar = argmax (gen_bihist ~l bins) btwn_class_var in
    let pstar =
      (fun (_, p, _) -> p) (List.find (fun (_, _, b) -> b = tstar) vpb_assoc)
    in
    let offset = copy pca in
    scal pstar offset;
    let centroid = Vec.add mean offset in
    let to_mat xs = Mat.of_col_vecs_list @@ List.map (fun (v, _, _) -> v) xs in
    let left, right = List.partition (fun (_, _, b) -> b < tstar) vpb_assoc in
    (centroid, to_mat left, to_mat right)
  in
  let z' = focus_max_sse z in
  let z'' = grow_leaf z' split_otsu in
  z''

let rec n_times (n : int) (x : 'a) (f : 'a -> 'a) : 'a =
  match n with 0 -> x | n -> n_times (pred n) (f x) f

let cluster d k =
  let init = mkzip @@ Leaf d in
  unzip @@ n_times k init split_step

let () =
  let d = Lacaml.S.Mat.random 3 12 in
  let x = cluster d 3 in
  Format.printf "@[<2>d:@\n@\n%a@]@.\n" (Lacaml.Io.pp_lfmat ()) d;
  print x
