open Vector 

type t = 
  | Circle of float * position
  | Cube of dimension * position 

  and position = Vector.t
  and dimension = Vector.t 

let make_circle r pos = Circle (r, pos)
let make_cube dim pos = Cube (dim, pos)

(* let calc_normal pos shape = 
  let eps = 0.001 in
  let epsilon_x = vector eps 0. 0. in 
  let epsilon_y = vector 0. eps 0. in 
  let epsilon_z = vector 0. 0. eps in 
   norm (vector () () ()) *)

let normal pos = function 
  | Circle (r, _) -> (norm (vector (get_x pos) (get_y pos) ((get_z pos) -. r))) 
  | _ -> vector 0. 0. 0.

let sdf pos = function 
  | Circle (r, shape_pos) -> (length (sub pos shape_pos)) -. r
  | Cube (dim, shape_pos) -> 
    let q = sub (Vector.abs shape_pos) dim in
    let q_x = get_x q and q_y = get_y q and q_z = get_z q in 
      (length (vmax q 0.0)) +. (min (max q_x (max q_y q_z)) 0.0)

let pp_shape oc = function 
  | Circle (r, pos) -> Printf.fprintf oc "CIRCLE with Radius: %f Position: " r; Vector.pp_vec oc pos 
  | Cube (dim, pos) -> Vector.pp_vec oc dim; Vector.pp_vec oc pos 
