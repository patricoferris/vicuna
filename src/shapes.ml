open Vector 
open Transforms 

type t = 
  | Circle of float * position
  | Cube of dimension * position 
  
  and position = Vector.t
  and dimension = Vector.t 

let make_circle r pos = Circle (r, pos)
let make_cube dim pos = Cube (dim, pos)

let sdf pos = function 
  | Circle (r, shape_pos) -> (length (sub pos shape_pos)) -. r
  | Cube (dim, shape_pos) -> 
    let rx = rot_mat (half_pi /. 2.) X in 
    let ry = rot_mat (half_pi /. 2.) Y in 
    let q = sub (Vector.abs ( rot (rx @-> ry) (sub pos shape_pos)) ) dim in
    let q_x = get_x q and q_y = get_y q and q_z = get_z q in 
      (length (vmax q 0.0)) +. (min (max q_x (max q_y q_z)) 0.0)

let calc_normal pos shape = 
  let eps = 0.001 in
  let epsilon_x = vector eps 0.  0.  in 
  let epsilon_y = vector 0.  eps 0.  in 
  let epsilon_z = vector 0.  0.  eps in 
   norm (vector 
        ((sdf (add pos epsilon_x) shape) -. (sdf (sub pos epsilon_x) shape)) 
        ((sdf (add pos epsilon_y) shape) -. (sdf (sub pos epsilon_y) shape)) 
        ((sdf (add pos epsilon_z) shape) -. (sdf (sub pos epsilon_z) shape))) 

let normal pos = function 
  | Circle (r, _) -> (norm (vector (get_x pos) (get_y pos) ((get_z pos) -. r))) 
  | c -> calc_normal pos c 

let pp_shape oc = function 
  | Circle (r, pos) -> Printf.fprintf oc "CIRCLE with Radius: %f Position: " r; Vector.pp_vec oc pos 
  | Cube (dim, pos) -> Vector.pp_vec oc dim; Vector.pp_vec oc pos 
