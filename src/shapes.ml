open Vector 
open Transforms 

type t = 
  | Circle of float * position * Transforms.t
  | Cube of dimension * position * Transforms.t 
  
  and position = Vector.t
  and dimension = Vector.t 

let make_circle r pos = Circle (r, pos, Transforms.const 1.)
let make_cube dim pos = Cube (dim, pos, Transforms.const 1.)

let sdf pos = function 
  | Circle (r, shape_pos, t) -> (length (t $ (sub pos shape_pos))) -. r
  | Cube (dim, shape_pos, t) -> 
    let q = sub (Vector.abs (t $ (sub pos shape_pos))) dim in 
    let q_x = get_x q and q_y = get_y q and q_z = get_z q in 
      (length (vmax q 0.0)) +. (min (max q_x (max q_y q_z)) 0.0)

let calc_normal pos shape = 
  let eps = 0.001 in
  let epsilon_x = vector eps 0.  0.  0. in 
  let epsilon_y = vector 0.  eps 0.  0. in 
  let epsilon_z = vector 0.  0.  eps 0. in 
   norm (vector 
        ((sdf (add pos epsilon_x) shape) -. (sdf (sub pos epsilon_x) shape)) 
        ((sdf (add pos epsilon_y) shape) -. (sdf (sub pos epsilon_y) shape)) 
        ((sdf (add pos epsilon_z) shape) -. (sdf (sub pos epsilon_z) shape))
        1.) 

let normal pos = function 
  | Circle (r, _, _) -> (norm (vector (get_x pos) (get_y pos) ((get_z pos) -. r) 1.)) 
  | c -> calc_normal pos c 

let apply_transform trans = function 
  | Circle (r, p, t)   -> Circle (r, p, trans @-> t) 
  | Cube (dim, pos, t) -> Cube (dim, pos, trans @-> t)

let pp_shape oc = function 
  | Circle (r, pos, _) -> Printf.fprintf oc "CIRCLE with Radius: %f Position: " r; Vector.pp_vec oc pos 
  | Cube (dim, pos, _) -> Vector.pp_vec oc dim; Vector.pp_vec oc pos 
