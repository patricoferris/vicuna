type t 

val normal : Vector.t -> t -> Vector.t 
val sdf : Vector.t -> t -> float
val make_circle : float -> Vector.t -> t 
val make_cube  : Vector.t -> Vector.t -> t
val pp_shape : out_channel -> t -> unit 