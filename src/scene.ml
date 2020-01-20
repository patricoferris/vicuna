type t = Shapes.t list * Vector.t list 
type closest_shape = { dist: float; shape: Shapes.t }
let scene shapes lights = (shapes, lights)

let get_shapes scene = 
  let (shapes, _lights) = scene in shapes 

let get_lights scene = 
  let (_shapes, lights) = scene in lights 

let sdf pos scene = 
  let (shapes, _lights) = scene in
  let rec min (dist, shape) = function 
    | []    -> { dist; shape }
    | s::ss -> 
      let sdf = Shapes.sdf pos s in 
        min (if sdf < dist then (sdf, s) else (dist, shape)) ss
  in  
    min (Float.max_float, List.hd shapes) shapes
