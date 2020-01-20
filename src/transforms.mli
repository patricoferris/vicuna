type t 
type axis = X | Y | Z

val pi : float
val half_pi : float

val transpose : t -> t
val const : float -> t 
val (@->) : t -> t -> t
val ($) : t -> Vector.t -> Vector.t
val rot_mat : float -> axis -> t
val trans_mat : float -> float -> float -> t
val inverse : t -> t
val to_array : t -> float array array
val equal : t -> t -> bool
val pp_trans : out_channel -> t -> unit 