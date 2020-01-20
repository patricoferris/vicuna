type t = { x: float ; y: float; z: float; w: float}

let vector x y z w= {x; y; z; w}
let get_x v = v.x
let get_y v = v.y
let get_z v = v.z
let get_w v = v.w

let add a b   = { x = a.x +. b.x; y = a.y +. b.y; z = a.z +. b.z; w = 1.0 }
let sub a b   = { x = a.x -. b.x; y = a.y -. b.y; z = a.z -. b.z; w = 1.0 }
let scale s a = { x = s   *. a.x; y = s   *. a.y; z = s   *. a.z; w = 1.0 }
let const s   = { x = s; y = s; z = s; w = s}
let length a  = sqrt (a.x**2. +. a.y**2. +. a.z**2.)
let abs a = { x = abs_float a.x; y = abs_float a.y; z = abs_float a.z; w = 1.0 }

let vmax a f = { x = max a.x f; y = max a.y f; z = max a.z f; w = max a.w f }
let vmin a f = { x = min a.x f; y = min a.y f; z = min a.z f; w = min a.w f }

let norm a = 
    let normalization_constant = length a in 
        scale (1.0 /. normalization_constant) a

let pp_vec oc a = Printf.fprintf oc "(%f, %f, %f, %f) \n" a.x a.y a.z a.w

let eq_ep f1 f2 ep = (Float.abs(f1 -. f2)) < ep

let equal v1 v2 = 
	let ep = 0.00001 in 
    	eq_ep (get_x v1) (get_x v2) ep &&
		eq_ep (get_y v1) (get_y v2) ep &&
		eq_ep (get_z v1) (get_z v2) ep &&
		eq_ep (get_w v1) (get_w v2) ep

let to_array v = 
	let arr = Array.make 4 0. in 
		arr.(0) <- (get_x v); arr.(1) <- (get_y v); arr.(2) <- (get_z v); arr.(3) <- (get_w v); arr 

exception ArrayTooSmall
let from_array arr = 
	if Array.length arr < 4 then raise ArrayTooSmall
	else { x = arr.(0); y = arr.(1); z = arr.(2); w = arr.(3); }

let cross a b = 
    { x = a.y *. b.z -. a.z *. b.y;
      y = a.z *. b.x -. a.x *. b.z;
      z = a.x *. b.y -. a.y *. b.x; 
      w = 1.0 }

let dot a b = a.x *. b.x +. a.y *. b.y +. a.z *. b.z +. a.w *. b.w

