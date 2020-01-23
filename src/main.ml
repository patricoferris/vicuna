
open Printf
open Vector

let dimX = 1080
let dimY = 720
let ppm_hdr_1 = "P3"
let ppm_hdr_2 = string_of_int dimX ^ " " ^ string_of_int dimY
let ppm_hdr_3 = "255"

let write_line oc line = 
    fprintf oc "%s\n" line

let write oc line = 
    fprintf oc "%s" line

let write_hdr oc = 
    write_line oc ppm_hdr_1;
    write_line oc ppm_hdr_2;
    write_line oc ppm_hdr_3

let scene =
    let cube = Shapes.make_cube (Vector.const 1.) (vector 0. (-1.0) 20. 1.) (Color.color 0 255 0) in
    let translation = Transforms.trans_mat 1. (-1.) 0. in 
    let rotation_x  = Transforms.(rot_mat (half_pi /. 2.) X) in 
    let rotation_y  = Transforms.(rot_mat (half_pi /. 2.) Y) in 
    let cube = Shapes.apply_transform Transforms.(translation @-> rotation_x @-> rotation_y) cube in
    let circle = Shapes.make_circle 2. (vector 0. (-1.0) 20. 1.) (Color.color 255 0 0) in 
    let light_source = vector 2. 0. 19. 1. in 
        Scene.scene ([cube; circle]) ([light_source])

let scan dimX dimY = 
    let camDir = vector 0. 0. 1. 1. in
    let camUp =  vector 0. 1. 0. 1. in
    let res = vector (float_of_int dimX) (float_of_int dimY) 0. 1. in
    let planeDist = 3. in
    let oc = open_out "image.ppm" in
    write_hdr oc;
    let lines = ref [] in 
    for i = 0 to dimY - 1 do 
        let line = ref [] in 
        for j = 0 to dimX - 1 do 
            let xcoord = float_of_int j /. float_of_int dimX in 
            let ycoord = float_of_int i /. float_of_int dimY in
            let pos = vector xcoord ycoord 0. 1. in
            let dir = Rays.get_ray_dir camDir camUp pos res planeDist in
            let c = Rays.ray_march (const 0.) dir scene in
               line := (Color.color_to_string c) :: !line
        done;
        lines := !line :: !lines 
    done;
    List.iter (fun line -> write_line oc (String.concat "" (List.rev line))) !lines 
let () = scan dimX dimY