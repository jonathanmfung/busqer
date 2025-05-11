type data_t = Lacaml.S.mat
type cluster_node

val leaf : data_t -> cluster_node

type zipper
val mkzip : cluster_node -> zipper

val go_left : zipper -> zipper
val go_right : zipper -> zipper
val go_up : zipper -> zipper
val unzip : zipper -> cluster_node
val grow_leaf : zipper -> (data_t -> Lacaml.S.vec * data_t * data_t) -> zipper
val focus_max_sse : zipper -> zipper

type bihist

val btwn_class_var : bihist -> float
val argmax : (int * bihist) list -> (bihist -> 'a) -> int
val binify : float list -> l:int -> int list

val cluster : data_t -> int -> cluster_node
