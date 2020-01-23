type t = { up : Vector.t; side : Vector.t; dir : Vector.t }

let create up side dir = {up; side; dir}
let get_up t = t.up
let get_side t = t.side
let get_dir t = t.dir