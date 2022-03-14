exception Out_of_Bounds

type coord = int * int

type encounter = {
  name : string;
  rate : int;
  levels : int list;
}

type encounters = encounter list

type tile_type =
  | Path
  | Grass of encounters
  | Obstacle

type tile = {
  graphic : string;
  ttype : tile_type;
}

type t = tile array array

let init_map j = raise (Failure "Unimplemented")
let get_dim map = (Array.length map, Array.get map 0 |> Array.length)

(** [get_tile t (x, y)] is the tile at the coordinate (x, y) in map [t].
    Raises [Out_of_Bounds] if [c] is not a coordinate in [t] *)
let get_tile (t : t) ((x, y) : coord) =
  try
    let x_arr = Array.get t x in
    Array.get x_arr y
  with Invalid_argument x -> raise Out_of_Bounds

let get_type t c =
  match get_tile t c with
  | { ttype; _ } -> ttype

let get_graphic_id t c =
  match get_tile t c with
  | { graphic; _ } -> graphic

let encounter_creature encounter = raise (Failure "Unimplemented")