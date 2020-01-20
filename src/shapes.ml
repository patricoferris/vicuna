open Vector 
open Transforms 

type t = 
  | Circle of float * position * Transforms.t * Color.t
  | Cube of dimension * position * Transforms.t * Color.t
  
  and position = Vector.t
  and dimension = Vector.t 

let make_circle r pos c = Circle (r, pos, Transforms.const 1., c)
let make_cube dim pos c = Cube (dim, pos, Transforms.const 1., c)

let get_color = function 
  | Circle (_, _, _, c) -> c
  | Cube (_, _, _, c) -> c

let sdf pos = function 
  | Circle (r, shape_pos, t, _) -> (length (t $ (sub pos shape_pos))) -. r
  | Cube (dim, shape_pos, t, _) -> 
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
  | Circle (r, _, _, _) -> (norm (vector (get_x pos) (get_y pos) ((get_z pos) -. r) 1.)) 
  | c -> calc_normal pos c 

let apply_transform trans = function 
  | Circle (r, p, t, c)   -> Circle (r, p, trans @-> t, c) 
  | Cube (dim, pos, t, c) -> Cube (dim, pos, trans @-> t, c)

let pp_shape oc = function 
  | Circle (r, pos, _, _) -> Printf.fprintf oc "CIRCLE with Radius: %f Position: " r; Vector.pp_vec oc pos 
  | Cube (dim, pos, _, _) -> Vector.pp_vec oc dim; Vector.pp_vec oc pos 
