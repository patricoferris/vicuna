open Vector

let diffuse pos normal light = 
  let pos_to_light = norm (sub light pos) in 
  let dot_p = dot normal pos_to_light in 
      min 1.0 (max (abs_float dot_p) 0.0) 

let specular pos normal light = 
  let pos_to_light = norm (sub light pos) in 
  let norm_norm = (norm normal) in 
  let norm_light = sub (const 0.) (norm pos_to_light) in
  let r = add (scale (2. *. (dot norm_norm norm_light)) norm_norm) norm_light in
  let rv = dot (norm pos) (r) in 
    (min 1. rv) ** 50.

let illuminate pos light shape = 
  let normal = Shapes.normal pos shape in 
  let diff = diffuse pos normal light in
  let _spec = specular pos normal light in 
  let open Color in 
  let red = color 255 0 0 in
  let _white = color 255 255 255 in
    scale diff red
    (* mix (scale diff red) (scale _spec _white) *)