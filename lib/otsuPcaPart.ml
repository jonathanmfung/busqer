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

type vect3 = int * int * int
type data_t = mat

type cluster_node =
  | Leaf of data_t
  | Node of { threshold : vect3; left : cluster_node; right : cluster_node }

type cluster_path =
  | Left of { threshold : vect3; rctx : cluster_node }
  | Right of { threshold : vect3; lctx : cluster_node }

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

let replace (Zip { thread; _ }) tree = Zip { tree; thread }
let mkzip (t : cluster_node) : zipper = Zip { tree = t; thread = [] }

let go_left (Zip { tree; thread = old_thread }) : zipper =
  match tree with
  | Leaf _ -> invalid_arg "go_left Leaf"
  | Node { threshold; left; right } ->
      Zip
        { thread = Left { threshold; rctx = right } :: old_thread; tree = left }

let go_right (Zip { tree; thread = old_thread }) : zipper =
  match tree with
  | Leaf _ -> invalid_arg "go_right Leaf"
  | Node { threshold; left; right } ->
      Zip
        {
          thread = Right { threshold; lctx = left } :: old_thread;
          tree = right;
        }

let go_up (Zip { tree; thread }) : zipper =
  match thread with
  | [] -> invalid_arg "go_up already at top"
  | Left { rctx; threshold } :: ts ->
      Zip { tree = Node { threshold; left = tree; right = rctx }; thread = ts }
  | Right { lctx; threshold } :: ts ->
      Zip { tree = Node { threshold; left = lctx; right = tree }; thread = ts }

let rec unzip (Zip { tree; thread } : zipper) : cluster_node =
  match thread with
  | [] -> tree
  | Left { rctx; threshold } :: ts ->
      unzip
        (Zip
           { tree = Node { threshold; left = tree; right = rctx }; thread = ts })
  | Right { lctx; threshold } :: ts ->
      unzip
        (Zip
           { tree = Node { threshold; left = lctx; right = tree }; thread = ts })

let grow_leaf (Zip { tree; thread }) (f : data_t -> data_t * data_t) : zipper =
  match tree with
  | Node _ -> invalid_arg "grow_leaf Node"
  | Leaf data ->
      let left, right = f data in
      Zip
        {
          tree =
            Node { threshold = (0, 0, 0); left = Leaf left; right = Leaf right };
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

type 'a origin = Origin of 'a

type ('a, 'vec) offset3 =
  | Offset of { offset : 'vec; x : 'a; f : 'a -> 'vec -> 'a }

let center (Offset { offset; x; f }) : 'a origin = Origin (f x offset)

let principal_component (m : mat) : vec =
  (* TODO: double check that pca of data is same as eigenvector of data
     Or if it should be eigenvector of covariance matrix *)
  (* TODO: Check if eigenvector is normalized or not *)
  let _, _, _, right_evec = geev m in
  (Mat.to_col_vecs right_evec).(0)

let mean (m : mat) : vec =
  let sum = Mat.fold_cols (Vec.add ~ofsy:1 ~incy:1) (Vec.make0 3) m in
  let denom = Vec.make 3 (Int.to_float @@ Mat.dim2 m) in
  Vec.div sum denom

let ssd (m : mat) (v : vec) : float =
  let diffs = Array.map (Vec.ssqr_diff v) (Mat.to_col_vecs m) in
  Array.fold_left Float.add 0.0 diffs

let focus_max_sse z = focus_max z (fun m -> ssd m (mean m))

let project ~(onto : vec) (v : vec) : vec =
  let a = dot v onto /. nrm2 onto in
  let out = copy onto in
  scal a out;
  out

module IntMap = Map.Make (Int)

(* https://arxiv.org/pdf/1304.7465 pg 6 *)
type bihist = { l : int; t : int; freqs : float IntMap.t }

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
  List.mapi (fun i t -> (i, { l; t; freqs = make_freqs data })) ts

let argmax (bs : (int * bihist) list) (f : bihist -> 'a) : int =
  let max (i1, x1) (i2, x2) = if f x1 < f x2 then (i2, x2) else (i1, x1) in
  let tmax, _bhmax =
    List.fold_left max (0, { l = 0; t = 0; freqs = IntMap.empty }) bs
  in
  tmax

let mu_full ({ freqs; _ } : bihist) : float =
  (* mu_threshold is just this but on class_0 *)
  IntMap.fold (fun k v acc -> (Float.of_int k *. v) +. acc) freqs 0.0

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
  (* TODO: incorporate principal_component and projection *)
  (* data: [coord]
     threshold: {offset:coord, vec}

     project data onto threshold
     1. adj = data - offset
     1a. ensure |max(adj)|< |vec| (so that projection is valid)
     2. a = vec_project adj onto vec
     3. partition (a < vec)

     tstar is argmax (btwn_class_var)
     split_class tstar is bihist split
     actual split is grow_leaf z (f : data_t -> data_t * data_t)

     Handle init case (no prev threshold)
  *)
  let split_data m =
    let tuplefy (a, b) = (Mat.of_col_vecs_list a, Mat.of_col_vecs_list b) in
    let threshold = mean m in
    tuplefy (List.partition (fun v -> v < threshold) (Mat.to_col_vecs_list m))
  in
  let combine_with xs ys f =
    List.map (fun (x, y) -> f x y) (List.combine xs ys)
  in
  let split_otsu m =
    let l = 256 in
    let tuplefy (a, b) =
      let f xs = Mat.of_col_vecs_list @@ List.map (fun (v, _, _) -> v) xs in
      (f a, f b)
    in
    let mean = mean m in
    let zero_mean =
      List.map (fun x -> Vec.sub x mean) (Mat.to_col_vecs_list m)
    in
    let pca = principal_component m in
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
    (* TODO: attach centroid to new node *)
    tuplefy (List.partition (fun (_, _, b) -> b < tstar) vpb_assoc)
  in
  let z' = focus_max_sse z in
  let z'' = grow_leaf z' split_data in
  z''

(*   let pc_axis = principal_component data in *)
(*   let project (x, _, _) = x in *)
(*   let alpha = *)
(*     (project @@ List.fold_left coord_add (0, 0, 0) data) / List.length data *)
(*   in *)
(*   let l, r = List.partition (fun (x, _, _) -> x <= alpha) data in *)
(*   Node { threshold = alpha; left = Leaf l; right = Leaf r } *)

let rec n_times (n : int) (x : 'a) (f : 'a -> 'a) : 'a =
  match n with 0 -> x | n -> n_times (pred n) (f x) f

let cluster (d : data_t) (k : int) : cluster_node =
  let init = mkzip @@ Leaf d in
  unzip @@ n_times k init split_step

(* let () = *)
(*   let a = Mat.random 5 3 in *)
(*   let lv, wr, wi, rv = geev a in *)
(*   Format.printf "@[<2>a:@\n@\n%a@]@.\n" (Lacaml.Io.pp_lfmat ()) a; *)
(*   Format.printf "@[<2>lv:@\n@\n%a@]@.\n" (Lacaml.Io.pp_lfmat ()) lv; *)
(*   Format.printf "@[<2>wr:@\n@\n%a@]@.\n" (Lacaml.Io.pp_lfvec ()) wr; *)
(*   Format.printf "@[<2>wi:@\n@\n%a@]@.\n" (Lacaml.Io.pp_lfvec ()) wi; *)
(*   Format.printf "@[<2>rv:@\n@\n%a@]@.\n" (Lacaml.Io.pp_lfmat ()) rv *)

let () =
  let d = Lacaml.S.Mat.random 3 10 in
  let x = cluster d 3 in
  Format.printf "@[<2>d:@\n@\n%a@]@.\n" (Lacaml.Io.pp_lfmat ()) d;
  print x
