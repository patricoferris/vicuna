type t = Shapes.t list * light list 
  and light = Vector.t 

let sdf shapes = 
  List.fold_left (fun acc shape -> min (Shapes.sdf shape) acc) 10000. shapes  
