type t 

val color : int -> int -> int -> t

val clamp : t -> t
(** Takes a color and ensures 255 is the limit of any color channel *)

val integerize : float * float * float -> t
val mix : t -> t -> t
val scale : float -> t -> t
val color_to_string : t -> string 