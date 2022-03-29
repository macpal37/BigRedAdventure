open Yojson.Basic.Util

exception Out_of_Bounds
exception Malformed_Json of string

type coord = int * int

type encounter = {
  name : string;
  rate : float;
  levels : int * int;
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

let json_val json f key = json |> member key |> f
let json_list json f key = json |> member key |> to_list |> List.map f

let string_to_range (s : string) =
  match String.split_on_char '-' s |> List.map int_of_string with
  | [ a; b ] -> (a, b)
  | _ | (exception Failure _) ->
      raise (Malformed_Json "Invalid level range")

let encounter_of_json json =
  {
    name = json_val json to_string "creatures";
    rate = json_val json to_float "encounter_rate";
    levels = json_val json to_string "level_range" |> string_to_range;
  }

let encounters_of_json json =
  json |> to_list |> List.map encounter_of_json

let all_encounters_of_json json =
  json_list json encounters_of_json "encounters" |> Array.of_list

let sprites_of_json json =
  json_list json to_string "tile_sprites" |> Array.of_list

let list_to_tile_type encounters (s : string) =
  let type_code = String.get s 0 in
  if type_code = 'p' then Path
  else if type_code = 'o' then Obstacle
  else
    Grass
      (String.sub s 1 (String.length s - 1)
      |> int_of_string |> Array.get encounters)

let code_to_tile graphics encounters (s : string) =
  match String.split_on_char '|' s with
  | [ h; t ] ->
      {
        graphic = int_of_string t |> Array.get graphics;
        ttype = list_to_tile_type encounters h;
      }
  | _ -> raise (Malformed_Json "Invalid map tile")

let ll_to_matrix ll = Array.of_list (List.map Array.of_list ll)
let matrix_map f m = Array.(map (map f) m)
let codes_of_json json = json |> to_string |> String.split_on_char ','

let init_map map_name =
  let json =
    Yojson.Basic.from_file ("assets/maps/" ^ map_name ^ ".json")
  in
  let e = all_encounters_of_json json in
  let g = sprites_of_json json in
  json |> member "map" |> to_list |> List.map codes_of_json
  |> ll_to_matrix
  |> matrix_map (code_to_tile g e)

let get_dim map = (Array.length map, Array.get map 0 |> Array.length)

(** [get_tile t (x, y)] is the tile at the coordinate (x, y) in map [t].
    Raises [Out_of_Bounds] if [c] is not a coordinate in [t] *)
let get_tile (t : t) ((x, y) : coord) =
  try
    let x_arr = Array.get t x in
    Array.get x_arr y
  with Invalid_argument _ -> raise Out_of_Bounds

let get_type t c =
  match get_tile t c with
  | { ttype; _ } -> ttype

let get_graphic_id t c =
  match get_tile t c with
  | { graphic; _ } -> graphic

let creature_level (min, max) = min + Random.int (max - min + 1)

type random_pokemon = {
  name : string;
  level : int;
}

let rec creature_type (e : encounters) (v : float) =
  match e with
  | [] -> None
  | h :: t ->
      if h.rate >= v then
        Some { name = h.name; level = creature_level h.levels }
      else creature_type t v

let encounter_creature e =
  match creature_type e (Random.float 1.0) with
  | None -> None
  | Some { name; level } -> Some (Creature.create_creature name level)
