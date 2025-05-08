(* Maybe just try this naive approach to clustering,
   since further k-means does not lower MSE that much *)

(* Then can implement google's Score process to cull the generated cluster list *)

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

let  print (c : cluster_node) : unit =
  let rec go n c' =
    match c' with
    | Leaf x -> Format.printf "@[<2>%i:@\n@\n%a@]@.\n"  n  (Lacaml.Io.pp_lfmat ()) x
    | Node {left; right;_} -> go (succ n) left; go (succ n) right
  in go 0 c


let replace (Zip { thread; _ }) tree = Zip { tree; thread }
let mkzip (t : cluster_node) : zipper = Zip { tree = t; thread = [] }

let go_left (Zip { tree; thread = old_thread }) : zipper =
  match tree with
  | Leaf _ -> invalid_arg "goLeft Leaf"
  | Node { threshold; left; right } ->
      Zip
        { thread = Left { threshold; rctx = right } :: old_thread; tree = left }

let go_right (Zip { tree; thread = old_thread }) : zipper =
  match tree with
  | Leaf _ -> invalid_arg "goRight Leaf"
  | Node { threshold; left; right } ->
      Zip
        {
          thread = Right { threshold; lctx = left } :: old_thread;
          tree = right;
        }

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
    | Zip { tree = Leaf _; _ }, z2 -> max (max z1 (go_left z2)) (max z1 (go_right z2))
    | z1, Zip { tree = Leaf _; _ } -> max (max z2 (go_left z1)) (max z2 (go_right z1))
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

let split_step (z : zipper) : zipper =
  let tuplefy (a, b) = (Mat.of_col_vecs_list a, Mat.of_col_vecs_list b) in
  let split_data m =
    let threshold = mean m in
    tuplefy (List.partition (fun v -> v < threshold) (Mat.to_col_vecs_list m))
  in
  let z' = focus_max_sse z in
  let z'' = grow_leaf z' split_data in
  z''

let rec n_times (n : int) (x : 'a) (f : 'a -> 'a) : 'a =
  match n with
  | 0 -> x
  | n -> n_times (pred n) (f x) f

let cluster (d : data_t) (k: int) : cluster_node =
  let init = mkzip @@ Leaf(d) in
  unzip @@ n_times k init split_step

(*   let pc_axis = principal_component data in *)
(*   let project (x, _, _) = x in *)
(*   let alpha = *)
(*     (project @@ List.fold_left coord_add (0, 0, 0) data) / List.length data *)
(*   in *)
(*   let l, r = List.partition (fun (x, _, _) -> x <= alpha) data in *)
(*   Node { threshold = alpha; left = Leaf l; right = Leaf r } *)

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
