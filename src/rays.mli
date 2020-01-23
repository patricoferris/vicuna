val precision : float 
val step : int 

val get_ray_dir : Camera.t -> Vector.t -> Vector.t -> float -> Vector.t
(** Calculates a director from a camera, the coordinates, a resolution and the plane distance *)

val ray_march : Vector.t -> Scene.t -> Vector.t -> Color.t 