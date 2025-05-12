type elt_t = Lacaml.S.vec
type data_t = Lacaml.S.mat
type cluster_node

val print : cluster_node -> unit
val leaf : data_t -> cluster_node
val leafs_to_list : cluster_node -> data_t list
val centroids : cluster_node -> elt_t list
val to_hexstring : elt_t -> string

type zipper

val mkzip : cluster_node -> zipper
val go_left : zipper -> zipper
val go_right : zipper -> zipper
val go_up : zipper -> zipper
val unzip : zipper -> cluster_node
val grow_leaf : zipper -> (data_t -> elt_t * data_t * data_t) -> zipper
val focus_max_sse : zipper -> zipper
val cluster : data_t -> int -> cluster_node
