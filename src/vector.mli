(* Vector Module *)

type t

val vector: float -> float -> float -> float -> t

val get_x: t -> float
val get_y: t -> float 
val get_z: t -> float
val get_w: t -> float

val add : t -> t -> t
val sub : t -> t -> t
val cross : t -> t -> t
val dot : t -> t -> float 
val length : t -> float 
val abs : t -> t 
val vmax : t -> float -> t
val vmin : t -> float -> t

val equal : t -> t -> bool

val const : float -> t
(** Const takes a float, f, and generate the vector of { x = f; y = f; z = f } *)

val scale : float -> t -> t
val norm : t -> t 
val pp_vec : out_channel -> t -> unit 
