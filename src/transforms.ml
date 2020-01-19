open Vector

type t = Vector.t * Vector.t * Vector.t 
(** Each vector is a row in a matrix *)

type axis = X | Y | Z 

let rotation_matrix theta axis =
  let c = cos theta in 
  let s = sin theta in 
    match axis with 
    | X ->
      ((vector 1. 0. 0.   ),
       (vector 0. c  (-.s)),
       (vector 0. s  c    ))
    | Y ->
      ((vector c     0.  s),
       (vector 0.    1.  s),
       (vector (-.s) 0. c ))
    | Z ->
      ((vector c  (-.s) 0.),
       (vector s  c     0.),
       (vector 0. 0.    1.))

let rotate mx vec = 
  let (v1, v2, v3) = mx in
    ((vector (dot vec v1)),
     (vector (dot vec v2)),
     (vector (dot vec v3)))
