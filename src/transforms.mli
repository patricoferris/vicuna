type t 
type axis = X | Y | Z

val pi : float
val half_pi : float

(* val transpose : t -> t *)
val (@->) : t -> t -> t
val rot_mat : float -> axis -> t
val rot : t -> Vector.t -> Vector.t