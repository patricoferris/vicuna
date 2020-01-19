type t 
type axis = X | Y | Z

val pi : float
val half_pi : float

(* val transpose : t -> t *)
val const : float -> t 
val (@->) : t -> t -> t
val ($) : t -> Vector.t -> Vector.t
val rot_mat : float -> axis -> t