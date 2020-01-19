type t 
type axis

val transpose : t -> t
val rotatation_matrix : float -> axis -> t
val rotate : t -> Vector.t -> Vector.t