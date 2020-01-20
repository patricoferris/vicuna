type t 
type closest_shape = { dist: float; shape: Shapes.t }

val scene : Shapes.t list -> Vector.t list -> t
val get_shapes : t -> Shapes.t list 
val get_lights : t -> Vector.t list
val sdf : Vector.t -> t -> closest_shape