
open Printf
open Vector
open Lwt

let write_line oc line = 
    fprintf oc "%s\n" line

let write oc line = 
    fprintf oc "%s" line

let write_hdr oc dimX dimY = 
	let ppm_hdr_1 = "P3" in 
	let ppm_hdr_2 = string_of_int dimX ^ " " ^ string_of_int dimY in 
	let ppm_hdr_3 = "255" in 
    write_line oc ppm_hdr_1;
    write_line oc ppm_hdr_2;
    write_line oc ppm_hdr_3

let scan scene dimX dimY = 
	let compute () = 
		let camDir = vector 0. 0. 1. 1. in
		let camUp =  vector 0. 1. 0. 1. in
		let camSide = norm (cross camUp camDir) in 
		let camera = Camera.create camUp camSide camDir in 
		let res = vector (float_of_int dimX) (float_of_int dimY) 0. 1. in
		let planeDist = 3. in
		let lines = ref [] in 
		for%lwt i = 0 to dimY - 1 do 
			let work = ref [] in 
			for j = 0 to dimX - 1 do 
				let xcoord = float_of_int j /. float_of_int dimX in 
				let ycoord = float_of_int i /. float_of_int dimY in
				let pos = vector xcoord ycoord 0. 1. in
					work := (fun () -> Rays.get_ray_dir camera pos res planeDist |> Rays.ray_march (const 0.) scene |> Color.color_to_string |> Lwt.return) :: !work 
			done;
			Lwt_list.map_p (fun compute -> compute ()) !work >>= fun line -> Lwt.return (lines := line :: !lines)
		done >>= fun () ->
		let oc = open_out "image.ppm" in
			write_hdr oc dimX dimY;
			List.iter (fun line -> write_line oc (String.concat "" (List.rev line))) !lines;
			Lwt.return (close_out oc)
	in 
		Lwt_main.run (compute ())