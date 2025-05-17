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

let ( let* ) = Lwt.bind

open Lacaml.S

type elt_t = vec
type data_t = mat

type cluster_node =
  | Leaf of data_t
  | Node of { centroid : elt_t; left : cluster_node; right : cluster_node }

let leaf d = Leaf d

let leafs_to_list node =
  let rec go n xs =
    match n with
    | Leaf x -> x :: xs
    | Node { left = Leaf x; right = Leaf y; _ } -> x :: y :: xs
    | Node { left = n'; right = Leaf x; _ }
    | Node { left = Leaf x; right = n'; _ } ->
        go n' (x :: xs)
    | Node { left = n'; right = n''; _ } -> go n'' (go n' xs)
  in
  go node []

let to_hexstring v =
  let int_to_hexadecimal n =
    (* if n < 0 || 255 < n then *)
    (*   failwith "to_hexstring::int_to_hexadecimal n not in range 0..255"; *)
    Printf.sprintf "%02X" n
  in
  let f x = int_to_hexadecimal (Int.of_float x) in
  match Vec.to_list v with
  | [ a; b; c ] -> (fun (x, y, z) -> "#" ^ f x ^ f y ^ f z) (a, b, c)
  | _ -> failwith "to_hexstring vec is not size=3"

let centroids node =
  let rec go n xs =
    match n with
    | Leaf _ -> xs
    | Node { centroid; left = Leaf _; right = Leaf _; _ } -> centroid :: xs
    | Node { centroid; left = n'; right = Leaf _; _ }
    | Node { centroid; left = Leaf _; right = n'; _ } ->
        go n' (centroid :: xs)
    | Node { centroid; left = n'; right = n''; _ } ->
        go n'' (go n' (centroid :: xs))
  in
  go node []

type cluster_path =
  | Left of { centroid : elt_t; rctx : cluster_node }
  | Right of { centroid : elt_t; lctx : cluster_node }

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

let grow_leaf (Zip { tree; thread }) (f : data_t -> (elt_t * data_t * data_t) Lwt.t) :
      zipper Lwt.t=
  match tree with
  | Node _ -> invalid_arg "grow_leaf Node"
  | Leaf data ->
     let* centroid, left, right = f data in
     let tree = Node { centroid; left = Leaf left; right = Leaf right } in
     Lwt.return @@ Zip {tree; thread;}

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

let principal_component (covar : mat) : vec =
  (* TODO: Check if eigenvector is normalized or not *)
  let _, _, _, right_evec = geev covar in
  (Mat.to_col_vecs right_evec).(0)

let mean (m : mat) : vec =
  (* input m has observations as columns *)
  let sum = Mat.fold_cols (Vec.add ~ofsy:1 ~incy:1) (Vec.make0 3) m in
  let denom = Vec.make 3 (Int.to_float @@ Mat.dim2 m) in
  Vec.div sum denom

let var_mat (m : mat) : mat =
  (* input m has observations as columns *)
  let num_vars = Mat.dim1 m in
  let m_arr = Mat.to_array @@ Mat.transpose_copy m in
  (* obs are rows *)
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

let split_step (z : zipper) : zipper Lwt.t =
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
    let pca = principal_component (var_mat @@ Mat.of_col_vecs_list zero_mean) in
    (* Map of vec : projection : bin *)
    (* NOTE: let a' = project a onto b -> b - a' = [x; 0;...] AKA parallel *)
    let vec_proj_assoc =
      List.map
        (* TODO: Check that ~onto is greater maginitude than v (e.g. scale to max mag of zero_mean)
           TODO: actually nevermind mag of ~onto doesn't matter
        *)
          (fun v -> (v, Array.get (Vec.to_array (project ~onto:pca v)) 0))
        zero_mean
    in
    let bins = Bihist.binify ~l (List.map (fun (_v, p) -> p) vec_proj_assoc) in
    let vpb_assoc =
      combine_with vec_proj_assoc bins (fun (v, p) b -> (v, p, b))
    in
    let* bihists = Bihist.gen_bihist ~l bins in
    let tstar = Bihist.argmax bihists Bihist.btwn_class_var in
    let pstar =
      (fun (_, p, _) -> p) (List.find (fun (_, _, b) -> b = tstar) vpb_assoc)
    in
    let offset = copy pca in
    scal pstar offset;
    let centroid = Vec.add mean offset in
    let to_mat xs =
      Mat.of_col_vecs_list @@ List.map (fun (v, _, _) -> Vec.add mean v) xs
    in
    let left, right = List.partition (fun (_, _, b) -> b < tstar) vpb_assoc in
    Lwt.return (centroid, to_mat left, to_mat right)
  in
  let z' = focus_max_sse z in
  let z'' = grow_leaf z' split_otsu in
  z''

let cluster d k =
  (* TODO: maybe switch to Seq.iterate *)
  let init = Lwt.return @@ mkzip @@ Leaf d in
  let seq = Seq.iterate (fun x -> Lwt.bind x split_step) init in
  let kth = Seq.drop k @@ Seq.take (k + 1) seq in
  match Seq.uncons kth with
  | None -> failwith "cluster: unreachable, drop more than take"
  | Some (x, xs) -> (
      match xs () with
      | Seq.Cons _ -> failwith "cluster: unreachable, take more than drop"
      | Seq.Nil -> Lwt.map unzip x)

(* let () = *)
(*   let d = Lacaml.S.Mat.random 3 12 in *)
(*   let x = cluster d 3 in *)
(*   Format.printf "@[<2>d:@\n@\n%a@]@.\n" (Lacaml.Io.pp_lfmat ()) d; *)
(*   print x *)
