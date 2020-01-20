type t 

val normal : Vector.t -> t -> Vector.t 
val sdf : Vector.t -> t -> float
val make_circle : float -> Vector.t -> Color.t -> t 
val make_cube  : Vector.t -> Vector.t -> Color.t -> t
val apply_transform : Transforms.t -> t -> t
val get_color : t -> Color.t
val pp_shape : out_channel -> t -> unit 