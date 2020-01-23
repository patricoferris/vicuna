val precision : float 
val step : int 

val get_ray_dir : Vector.t -> Vector.t -> Vector.t -> Vector.t -> float -> Vector.t
val ray_march : Vector.t -> Vector.t -> Scene.t -> Color.t 