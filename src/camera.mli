type t 

val create : Vector.t -> Vector.t -> Vector.t -> t
val get_up : t -> Vector.t 
val get_side : t -> Vector.t 
val get_dir : t -> Vector.t 