
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

let scan scene dimX dimY = 
    let camDir = vector 0. 0. 1. 1. in
    let camUp =  vector 0. 1. 0. 1. in
    let camSide = norm (cross camUp camDir) in 
    let camera = Camera.create camUp camSide camDir in 
    let res = vector (float_of_int dimX) (float_of_int dimY) 0. 1. in
    let planeDist = 3. in
    let lines = ref [] in 
    for i = 0 to dimY - 1 do 
        let line = ref [] in 
        for j = 0 to dimX - 1 do 
            let xcoord = float_of_int j /. float_of_int dimX in 
            let ycoord = float_of_int i /. float_of_int dimY in
            let pos = vector xcoord ycoord 0. 1. in
            let dir = Rays.get_ray_dir camera pos res planeDist in
            let c = Rays.ray_march (const 0.) dir scene in
               line := (Color.color_to_string c) :: !line
        done;
        lines := !line :: !lines 
    done;
    let oc = open_out "image.ppm" in
        write_hdr oc;
        List.iter (fun line -> write_line oc (String.concat "" (List.rev line))) !lines;
        close_out oc