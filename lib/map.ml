type t = Unimplemented
type coord = int * int

type encounters = Unimplemented

type tile_type = 
| Path
| Grass of encounters
| Obstacle
let init_map j = raise (Failure "Unimplemented: State.current_room_id")

let get_dim map = raise (Failure "Unimplemented: State.current_room_id")

let get_type tile = raise (Failure "Unimplemented: State.current_room_id")

let get_graphic_id tile = raise (Failure "Unimplemented: State.current_room_id")

let encounter_creature encounter = raise (Failure "Unimplemented: State.current_room_id")