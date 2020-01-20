open Vector
open Transforms

let precision = 0.01
let step = 35

let resolution_scale a res = 
    let x = get_x a in 
    let y = get_y a in 
    let z = get_z a in 
    let res_x = get_x res and res_y = get_y res in 
        vector (x *. (res_x /. res_y)) y z 1.

(* Ray Marching Functions *)
let get_ray_dir camDir camUp coord res planeDist =
    let camSide = norm (cross camUp camDir) in 
    let coord = (sub (scale 2.0 coord) (Vector.const 1.0)) in
    let p = resolution_scale coord res in
    let p_x = get_x p and p_y = get_y p in 
    let cs = scale p_x camSide in 
    let cu = scale p_y camUp in
    let cd = scale planeDist camDir in 
        norm (add cs (add cu cd)) 

let ray_march position direction = 
    let sp = ref 0 in 
    let pos = ref position in
    let cube = Shapes.make_cube (Vector.const 1.) (vector 0. (-1.0) 20. 1.) (Color.color 0 255 0) in
    let translation = trans_mat 1. (-1.) 0. in 
    let rotation_x  = rot_mat (half_pi /. 2.) X in 
    let rotation_y  = rot_mat (half_pi /. 2.) Y in 
    let cube = Shapes.apply_transform (translation @-> rotation_x @-> rotation_y) cube in
    let circle = Shapes.make_circle 2. (vector 0. (-1.0) 20. 1.) (Color.color 255 0 0) in 
    let light_source = vector 2. 0. 19. 1. in 
    let scene = Scene.scene ([cube; circle]) ([light_source]) in 
    let closest = ref (Scene.sdf position scene) in 
        while ((abs_float (!closest).dist) > precision && !sp < step) do
            pos := add !pos (scale ((!closest).dist) direction);
            closest := Scene.sdf !pos scene;
            sp := !sp + 1
        done;
        let background = Color.color 244 244 244 in
        if !sp < step then Shade.illuminate !pos scene (!closest).shape else background