open Vicuna

let scene =
    let cube = Shapes.make_cube (Vector.const 1.) (Vector.vector 0. (-1.0) 20. 1.) (Color.color 0 255 0) in
    let translation = Transforms.trans_mat 1. (-1.) 0. in 
    let rotation_x  = Transforms.(rot_mat (half_pi /. 2.) X) in 
    let rotation_y  = Transforms.(rot_mat (half_pi /. 2.) Y) in 
    let cube = Shapes.apply_transform Transforms.(translation @-> rotation_x @-> rotation_y) cube in
    let circle = Shapes.make_circle 2. (Vector.vector 0. (-1.0) 20. 1.) (Color.color 255 0 0) in 
    let light_source = Vector.vector 2. 0. 19. 1. in 
        Scene.scene ([cube; circle]) ([light_source])

let () = Vicuna.Main.scan scene 900 600