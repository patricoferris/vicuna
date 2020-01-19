
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

let scan dimX dimY = 
    let xcoord = ref 0. in 
    let ycoord = ref 0. in
    let zcoord = ref 0. in
    let camDir = vector 0. 0. 1.0 in
    let camUp =  vector 0. 1.0 0. in
    let res = vector (float_of_int dimX) (float_of_int dimY) 0. in
    let planeDist = 3.0 in
    let oc = open_out "test.ppm" in
    write_hdr oc;
    for i = 0 to dimY - 1 do 
        for j = 0 to dimX - 1 do 
            xcoord := float_of_int j /. float_of_int dimX;
            ycoord := float_of_int i /. float_of_int dimY;
            let pos = vector !xcoord !ycoord !zcoord in
            let dir = Rays.get_ray_dir camDir camUp pos res planeDist in
            let c = Rays.ray_march (const 0.) dir in
                write oc (Color.color_to_string c) 
        done;
        write oc "\n";
    done

let () = scan dimX dimY