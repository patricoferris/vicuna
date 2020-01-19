type t = { x: float ; y: float; z: float }

let vector x y z = {x; y; z}
let get_x v = v.x
let get_y v = v.y
let get_z v = v.z

let add a b   = { x = a.x +. b.x; y = a.y +. b.y; z = a.z +. b.z }
let sub a b   = { x = a.x -. b.x; y = a.y -. b.y; z = a.z -. b.z }
let scale s a = { x = s   *. a.x; y = s   *. a.y; z = s   *. a.z }
let const s   = { x = s; y = s; z = s }
let length a  = sqrt (a.x**2. +. a.y**2. +. a.z**2.)
let abs a = { x = abs_float a.x; y = abs_float a.y; z = abs_float a.z }

let vmax a f = { x = max a.x f; y = max a.y f; z = max a.z f }
let vmin a f = { x = min a.x f; y = min a.y f; z = min a.z f }

let norm a = 
    let normalization_constant = length a in 
        scale (1.0 /. normalization_constant) a

let pp_vec oc a = Printf.fprintf oc "(%f, %f, %f) \n" a.x a.y a.z

let cross a b = { 
    x = a.y *. b.z -. a.z *. b.y;
    y = a.z *. b.x -. a.x *. b.z;
    z = a.x *. b.y -. a.y *. b.x
    }

let dot a b = a.x *. b.x +. a.y *. b.y +. a.z *. b.z 

