type t = int * int * int

let color r g b = (r, g, b)

let clamp (r, g, b) = 
  let upper = min 255 in 
    (upper r, upper g, upper b)

let integerize (r, g, b) = (int_of_float r, int_of_float g, int_of_float b) 
let mix (r1, g1, b1) (r2, g2, b2) = clamp (r1 + r2, g1 + g2, b1 + b2) 
let scale s (r1, g1, b1) = clamp (integerize (s *. (float_of_int r1), s *. (float_of_int g1), s *. (float_of_int b1)))
let color_to_string (r, g, b) =  string_of_int r ^ " " ^ string_of_int g ^ " " ^ string_of_int b ^ " " 