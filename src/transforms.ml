open Vector

type t = Vector.t * Vector.t * Vector.t * Vector.t
(** Each vector is a row in a matrix *)

type axis = X | Y | Z

let pi = 3.14159
let half_pi = 1.570795

let const f = 
  (vector f  0. 0. 0.,
   vector 0. f  0. 0.,
   vector 0. 0. f  0.,
   vector 0. 0. 0. 1.)
   
let transpose m = 
  let (v1, v2, v3, v4) = m in 
  let row_1 = vector (get_x v1) (get_x v2) (get_x v3) (get_x v4) in 
  let row_2 = vector (get_y v1) (get_y v2) (get_y v3) (get_y v4) in 
  let row_3 = vector (get_z v1) (get_z v2) (get_z v3) (get_z v4) in 
  let row_4 = vector (get_w v1) (get_w v2) (get_w v3) (get_w v4) in
    (row_1, row_2, row_3, row_4)

let mat_mul m n = 
  let (m1, m2, m3, m4) = m in 
  let (n1, n2, n3, n4) = transpose n in 
    (vector (dot m1 n1) (dot m1 n2) (dot m1 n3) (dot m1 n4), 
     vector (dot m2 n1) (dot m2 n2) (dot m2 n3) (dot m2 n4),
     vector (dot m3 n1) (dot m3 n2) (dot m3 n3) (dot m3 n4),
     vector (dot m4 n1) (dot m4 n2) (dot m4 n3) (dot m4 n4))

let (@->) m n = mat_mul m n

let rot_mat theta axis =
  let c = cos theta in 
  let s = sin theta in 
    match axis with 
    | X ->
      (vector 1. 0. 0.    0.,
       vector 0. c  (-.s) 0.,
       vector 0. s  c     0.,
       vector 0. 0. 0.    1.)
    | Y ->
      (vector c     0. s  0.,
       vector 0.    1. 0. 0.,
       vector (-.s) 0. c  0.,
       vector 0.    0. 0. 1.)
    | Z ->
      (vector c  (-.s) 0. 0.,
       vector s  c     0. 0.,
       vector 0. 0.    1. 0.,
       vector 0. 0.    0. 1.)

let app mx vec = 
  let (v1, v2, v3, v4) = mx in
    vector (dot vec v1) (dot vec v2) (dot vec v3) (dot vec v4)

let ($) mx vec = app mx vec
