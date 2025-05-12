type bihist

val binify : float list -> l:int -> int list
val btwn_class_var : bihist -> float
val argmax : (int * bihist) list -> (bihist -> 'a) -> int
val gen_bihist : l:int -> int list -> (int * bihist) list
