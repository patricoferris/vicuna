open Vector

type t = Vector.t * Vector.t * Vector.t 
(** Each vector is a row in a matrix *)

type axis = X | Y | Z

let pi = 3.14159
let half_pi = 1.570795

let transpose m = 
  let (v1, v2, v3) = m in 
  let row_1 = vector (get_x v1) (get_x v2) (get_x v3) in 
  let row_2 = vector (get_y v1) (get_y v2) (get_y v3) in 
  let row_3 = vector (get_z v1) (get_z v2) (get_z v3) in 
    (row_1, row_2, row_3)

let mat_mul m n = 
  let (m1, m2, m3) = m in 
  let (n1, n2, n3) = transpose n in 
    (vector (dot m1 n1) (dot m1 n2) (dot m1 n3),
     vector (dot m2 n1) (dot m2 n2) (dot m2 n3),
     vector (dot m3 n1) (dot m3 n2) (dot m3 n3))

let (@->) m n = mat_mul m n

let rot_mat theta axis =
  let c = cos theta in 
  let s = sin theta in 
    match axis with 
    | X ->
      ((vector 1. 0. 0.   ),
       (vector 0. c  (-.s)),
       (vector 0. s  c    ))
    | Y ->
      ((vector c     0.  s),
       (vector 0.    1. 0.),
       (vector (-.s) 0. c ))
    | Z ->
      ((vector c  (-.s) 0.),
       (vector s  c     0.),
       (vector 0. 0.    1.))

let rot mx vec = 
  let (v1, v2, v3) = mx in
    vector (dot vec v1) (dot vec v2) (dot vec v3)
