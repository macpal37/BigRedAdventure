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
  graphic : int;
  ttype : tile_type;
}

type t = {
  tiles : tile array array;
  spritesheet : Spritesheet.sprite_sheet;
}

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
(* let encounters_of_json json = json |> to_list |> List.map
   encounter_of_json *)

(* let all_encounters_of_json json = json_list json encounters_of_json
   "encounters" |> Array.of_list *)

let encounters =
  let json = Yojson.Basic.from_file "assets/util/encounters.json" in
  json |> member "test_map"
  (*Unnecessary member? Json will likely be reformatted*)
  |> member "encounters"
  |> to_list
  |> List.map (fun j -> j |> to_list |> List.map encounter_of_json)

let encounter_of_id = Util.list_index_fun encounters
(* let sprites_of_json json = json_list json to_string "tile_sprites" |>
   Array.of_list *)
(* let list_to_tile_type encounters (s : string) = let type_code =
   String.get s 0 in if type_code = 'p' then Path else if type_code =
   'o' then Obstacle else Grass (String.sub s 1 (String.length s - 1) |>
   int_of_string |> Array.get encounters) *)
(* let code_to_tile graphics encounters (s : string) = match
   String.split_on_char '|' s with | [ h; t ] -> { graphic =
   int_of_string t |> Array.get graphics; ttype = list_to_tile_type
   encounters h; } | _ -> raise (Malformed_Json "Invalid map tile") *)

let ll_to_matrix ll = Array.of_list (List.map Array.of_list ll)
let matrix_map f m = Array.(map (map f) m)
(* let matrix_process f m = for i = 0 to Array.length m - 1 do let sub_m
   = Array.get m i in for j = 0 to Array.length sub_m - 1 do let e =
   Array.get sub_m j in Array.set sub_m j (f e) done done *)

let matrix_get i j m = Array.get (Array.get m i) j
(* let codes_of_json json = json |> to_string |> String.split_on_char
   ',' *)

let read_dim json =
  (json |> member "width" |> to_int, json |> member "height" |> to_int)

let rec json_build_matrix_r (l : 'a list) w i (acc : 'a list) =
  match l with
  | [] -> []
  | h :: t ->
      if i = 1 then (h :: acc) :: json_build_matrix_r t w w []
      else json_build_matrix_r t w (i - 1) (h :: acc)

let json_build_matrix w json =
  let l = List.rev (json_list json to_int "data") in
  ll_to_matrix (json_build_matrix_r l w w [])

let build_id_arrays json w =
  json |> member "layers" |> to_list |> List.map (json_build_matrix w)

let json_tilesets json = json |> member "tilesets" |> to_list

let tileset_path_parser p =
  "assets/" ^ String.sub p 3 (String.length p - 3)

let json_tileset json =
  let src_path = json |> member "source" |> to_string in
  let src_json =
    Yojson.Basic.from_file (tileset_path_parser src_path)
  in
  ( json |> member "firstgid" |> to_int,
    src_json |> member "tiles" |> to_list
    |> List.map (fun j ->
           j |> member "properties" |> to_list
           |> List.map (fun j -> j |> member "value" |> to_string)
           |> List.hd) )

let json_encounters json =
  let src_path = json |> member "source" |> to_string in
  let src_json =
    Yojson.Basic.from_file (tileset_path_parser src_path)
  in
  ( json |> member "firstgid" |> to_int,
    src_json |> member "tiles" |> to_list
    |> List.map (fun j ->
           j |> member "properties" |> to_list
           |> List.map (fun j -> j |> member "value" |> to_int)
           |> List.hd) )

let build_tile_matrix id_m tileset =
  let offset, l = tileset in
  let tile_f = Util.list_index_fun l in
  matrix_map
    (fun t ->
      let ot = t - offset in
      match tile_f ot with
      | "Grass" -> { graphic = ot; ttype = Grass [] }
      | "Path" -> { graphic = ot; ttype = Path }
      | "Obstacle" -> { graphic = ot; ttype = Obstacle }
      | _ -> raise (Malformed_Json "Impossible tile type"))
    id_m

let set_encounters tile_m encounter_m e =
  let offset, l = e in
  let e_f = Util.list_index_fun l in
  for i = 0 to Array.length tile_m - 1 do
    let sub_m = Array.get tile_m i in
    for j = 0 to Array.length sub_m - 1 do
      let t = Array.get sub_m j in
      Array.set sub_m j
        (let e_id = matrix_get i j encounter_m in
         if e_id = 0 then t
         else
           match t.ttype with
           | Grass [] ->
               {
                 t with
                 ttype = Grass (encounter_of_id (e_f (e_id - offset)));
               }
           | _ -> raise (Malformed_Json "Impossible tile type"))
    done
  done

let spritesheet_cache = Hashtbl.create 16

let json_spritesheet json =
  let src_path = json |> member "source" |> to_string in
  let png_path =
    "assets/"
    ^ String.sub src_path 3 (String.length src_path - 8)
    ^ ".png"
  in
  match Hashtbl.find_opt spritesheet_cache png_path with
  | Some s -> s
  | None ->
      let t_json =
        Yojson.Basic.from_file (tileset_path_parser src_path)
      in
      let tilewidth = t_json |> member "tilewidth" |> to_int in
      let tileheight = t_json |> member "tileheight" |> to_int in
      let sprite_sheet =
        Spritesheet.init_spritesheet png_path tilewidth tileheight 4
      in
      Hashtbl.add spritesheet_cache png_path sprite_sheet;
      sprite_sheet

let load_map map_name =
  let json =
    Yojson.Basic.from_file ("assets/maps/" ^ map_name ^ ".json")
  in
  let w, _ = read_dim json in
  match build_id_arrays json w with
  | [ tile_id_m; encounter_id_m ] -> (
      match json_tilesets json with
      | [ tile_t; encounter_t ] ->
          let tileset_l = json_tileset tile_t in
          let encounter_l = json_encounters encounter_t in
          let tile_m = build_tile_matrix tile_id_m tileset_l in
          set_encounters tile_m encounter_id_m encounter_l;
          let spritesheet = json_spritesheet tile_t in
          { tiles = tile_m; spritesheet }
      | [] | _ :: _ -> raise (Malformed_Json "Impossible case!"))
  | [] | _ :: _ -> raise (Malformed_Json "Impossible case!")

(*let e = all_encounters_of_json json in let g = sprites_of_json json in
  json |> member "map" |> to_list |> List.map codes_of_json |>
  ll_to_matrix |> matrix_map (code_to_tile g e)*)

let get_dim map =
  let map = map.tiles in
  (Array.get map 0 |> Array.length, Array.length map)

let get_width map = Array.length (Array.get map.tiles 0)
let get_height map = Array.length map.tiles

(** [get_tile t (x, y)] is the tile at the coordinate (x, y) in map [t].
    Raises [Out_of_Bounds] if [c] is not a coordinate in [t] *)
let get_tile (t : t) ((x, y) : coord) =
  try
    let x_arr = Array.get t.tiles y in
    Array.get x_arr x
  with Invalid_argument _ -> raise Out_of_Bounds

let get_type t c =
  match get_tile t c with
  | { ttype; _ } -> ttype

let get_graphic_id t c =
  match get_tile t c with
  | { graphic; _ } -> graphic

let get_sprite t c =
  Spritesheet.get_sprite t.spritesheet (get_graphic_id t c)

let creature_level (min, max) = min + Random.int (max - min + 1)

type random_pokemon = {
  name : string;
  level : int;
}

(* let shuffle d = let nd = List.map (fun c -> (Random.bits (), c)) d in
   let sond = List.sort compare nd in List.map snd sond *)

let rec creature_type (e : encounters) (v : float) =
  match e with
  | [] -> None
  | h :: t ->
      if h.rate >= v then
        Some { name = h.name; level = creature_level h.levels }
      else creature_type t (v -. h.rate)

let encounter_creature e =
  Random.self_init ();
  (* let shuffled_encounters = shuffle e in *)
  match creature_type e (Random.float 1.0) with
  | None -> None
  | Some { name; level } -> Some (Creature.create_creature name level)

let graphics_matrix m =
  matrix_map
    (fun t ->
      if t.graphic < 10 then string_of_int t.graphic ^ " "
      else string_of_int t.graphic)
    m.tiles

let string_of_encounters e =
  List.fold_left
    (fun a (b : encounter) ->
      let low, high = b.levels in
      a ^ " ; {name = " ^ b.name ^ "; rate = " ^ string_of_float b.rate
      ^ "; levels = " ^ string_of_int low ^ "-" ^ string_of_int high
      ^ "}")
    "" e
