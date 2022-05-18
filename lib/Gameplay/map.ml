open Yojson.Basic.Util
open Entity
open Util

exception Out_of_Bounds
exception Malformed_Json of string

type coord = int * int

let entity_sprites =
  Spritesheet.init_spritesheet
    "assets/entity_sprites/entity_sprites.png" 16 16 3

let trainer_sprites =
  Spritesheet.init_spritesheet
    "assets/entity_sprites/trainer_sprites.png" 32 32 3

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
  sprite : Draw.sprite;
  ttype : tile_type;
}

type t = {
  tiles : tile array array;
  entities : (coord * entity) list;
  name : string;
}

let null_map = { tiles = [||]; entities = []; name = "" }
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

let encounters map_name =
  let json = Yojson.Basic.from_file "assets/util/encounters.json" in

  json
  |> member (List.hd (String.split_on_char '.' map_name))
  (*Unnecessary member? Json will likely be reformatted*)
  |> member "encounters"
  |> to_list
  |> List.map (fun j -> j |> to_list |> List.map encounter_of_json)

let encounter_of_id map_name = Util.list_index_fun (encounters map_name)
let ll_to_matrix ll = Array.of_list (List.map Array.of_list ll)
let matrix_map f m = Array.(map (map f) m)
let matrix_get i j m = Array.get (Array.get m i) j

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
  let layers = json |> member "layers" |> to_list in
  let rec build_id_arrays_rec = function
    | [] -> []
    | h :: t -> (
        try json_build_matrix w h :: build_id_arrays_rec t
        with Yojson.Basic.Util.Type_error (_, _) ->
          build_id_arrays_rec t)
  in

  let rec build_entities_list = function
    | [] -> []
    | h :: t -> (
        try h |> member "objects" |> to_list
        with Yojson.Basic.Util.Type_error (_, _) ->
          build_entities_list t)
  in

  (build_id_arrays_rec layers, build_entities_list layers)

(* json |> member "layers" |> to_list |> List.map (json_build_matrix
   w) *)

let tileset_path_parser p =
  "assets/" ^ String.sub p 3 (String.length p - 3)

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

(** [json_tilesets j] is an association list of (sourcename, tileset ref
    json)*)
let json_tilesets json =
  let tilesets = json |> member "tilesets" |> to_list in
  List.map
    (fun ts ->
      ( List.nth
          (String.split_on_char '/'
             (ts |> member "source" |> to_string))
          2,
        ts ))
    tilesets

let json_tileset json =
  let src_path = json |> member "source" |> to_string in
  (* print_endline src_path; *)
  let src_json =
    Yojson.Basic.from_file (tileset_path_parser src_path)
  in
  ( json |> member "firstgid" |> to_int,
    src_json |> member "tiles" |> to_list
    |> List.map (fun j ->
           j |> member "properties" |> to_list
           |> List.map (fun j -> j |> member "value" |> to_string)
           |> List.hd) )

let build_tile_matrix id_m tilesets =
  let tilesets_rev = List.rev tilesets in
  Util.print_int "Tile Set LENGTH: " (List.length tilesets);
  let rec find_tileset id tilesets =
    (* precondition: tilesets is in order of highest to lowest
       firstgid*)
    match tilesets with
    | (_, j) :: t ->
        let s = j |> member "firstgid" |> to_int in
        print_int "ID: " id;
        if id >= s then j else find_tileset id t
    | [] -> failwith "id not within domain"
  in

  matrix_map
    (fun t ->
      let json = find_tileset t tilesets_rev in
      (* print_endline "Worked YAY!"; *)
      let offset, l = json_tileset json in
      let tile_f = Util.list_index_fun l in
      let ot = t - offset in
      let spritesheet = json_spritesheet json in
      let sprite = Spritesheet.get_sprite spritesheet ot in

      (* Util.print_int "OT: " ot; *)

      (* Util.print_int "T: " t; *)
      match tile_f ot with
      | "Grass" -> { sprite; ttype = Grass [] }
      | "Path" -> { sprite; ttype = Path }
      | "Obstacle" -> { sprite; ttype = Obstacle }
      | _ -> raise (Malformed_Json "Impossible tile type"))
    id_m

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

let to_entity gid json =
  let properties = json |> member "properties" |> to_list in
  ( gid + (json |> member "id" |> to_int),
    List.nth
      (List.filter
         (fun j -> j |> member "name" |> to_string = "entity_type")
         properties)
      0
    |> member "value" |> to_string )

let json_entities json =
  let src_path = json |> member "source" |> to_string in
  let src_json =
    Yojson.Basic.from_file (tileset_path_parser src_path)
  in
  let gid = json |> member "firstgid" |> to_int in
  src_json |> member "tiles" |> to_list |> List.map (to_entity gid)

let set_encounters tile_m encounter_m e map_name =
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
           {
             t with
             ttype =
               Grass (encounter_of_id map_name (e_f (e_id - offset)));
           })
    done
  done

let orie_of_json json =
  match json |> to_string with
  | "South" -> S
  | "North" -> N
  | "East" -> E
  | "West" -> W
  | _ -> S

let find_o props name f def =
  let filter =
    List.filter (fun j -> j |> member "name" |> to_string = name) props
  in
  if List.length filter = 1 then List.hd filter |> member "value" |> f
  else def

let grass_sprite = Util.null ()

let generate_party json =
  Creature.create_creature
    (json |> member "creature" |> to_string)
    (json |> member "level" |> to_int)

let generate_sight (x, y) dir =
  match dir with
  | S -> Array.to_list (Array.init 4 (fun i -> (x, y - i)))
  | _ -> []

let generate_entities h tiles objs trainers =
  let entity_of_json json =
    let gid = json |> member "gid" |> to_int in

    let pos : coord =
      ( (json |> member "x" |> to_int) / 16,
        h - ((json |> member "y" |> to_int) / 16) )
    in

    try
      let props = json |> member "properties" |> to_list in
      let orie = find_o props "orientation" orie_of_json S in
      let dialogue = find_o props "dialogue" to_string "" in

      let e_type, sprite =
        match List.assoc gid tiles with
        | "Trainer" ->
            let name = find_o props "trainer_name" to_string "Eve" in
            let sprite =
              match name with
              | "Martha" -> Spritesheet.get_sprite trainer_sprites 3
              | "Clarkson" -> Spritesheet.get_sprite trainer_sprites 4
              | "Pekora" -> Spritesheet.get_sprite trainer_sprites 5
              | "Collin" -> Spritesheet.get_sprite trainer_sprites 6
              | "Alan" -> Spritesheet.get_sprite trainer_sprites 7
              | _ ->
                  Spritesheet.get_sprite trainer_sprites
                    (Util.rand 3 ())
            in
            let t = trainers |> member name in
            ( Trainer
                {
                  name;
                  alt_dialogue =
                    t |> member "defeat_dialogue" |> to_string;
                  party =
                    t |> member "party" |> to_list
                    |> List.map generate_party;
                  sight = generate_sight pos orie;
                },
              sprite )
        | "Sign" -> (Sign, Spritesheet.get_sprite entity_sprites 1)
        | "Item" ->
            ( Item
                {
                  name = find_o props "item_name" to_string "potion";
                  given = false;
                  disappear = find_o props "will_disappear" to_bool true;
                },
              Spritesheet.get_sprite entity_sprites 4 )
        | "Heal" -> (Heal, Spritesheet.get_sprite entity_sprites 5)
        | "Merchant" ->
            (Merchant, Spritesheet.get_sprite entity_sprites 6)
        | "Door" ->
            let map_name =
              find_o props "next_map" to_string "test_map" ^ ".json"
            in
            (* let json = Yojson.Basic.from_file ("assets/maps/" ^
               map_name) in let _, h2 = read_dim json in *)
            ( Door
                ( map_name,
                  ( find_o props "teleport_x" to_int 0,
                    find_o props "teleport_y" to_int 0 ) ),
              Draw.empty_sprite )
        | s ->
            print_endline ("strange" ^ s);
            (NoEntity, Draw.empty_sprite)
      in

      ( pos,
        {
          e_type;
          orie;
          pos;
          dialogue;
          sprite;
          state = 0;
          obstacle = true;
        } )
    with Yojson.Basic.Util.Type_error (_, _) -> (
      (* print_endline ("gid" ^ string_of_int gid); *)
      match List.assoc_opt gid tiles with
      | Some "Grass" ->
          let sprite =
            match !grass_sprite with
            | Some s -> s
            | None ->
                let s =
                  Draw.change_dpi
                    (Spritesheet.get_sprite entity_sprites 0)
                    4
                in
                grass_sprite := Some s;
                s
          in
          ( pos,
            {
              e_type = Grass;
              orie = S;
              pos;
              dialogue = "";
              state = 0;
              sprite;
              obstacle = false;
            } )
      | _ ->
          ( (0, 0),
            {
              e_type = NoEntity;
              orie = S;
              pos = (0, 0);
              dialogue = "";
              state = 0;
              sprite = Draw.empty_sprite;
              obstacle = false;
            } ))
  in

  List.map entity_of_json objs

let load_map map_name =
  let json = Yojson.Basic.from_file ("assets/maps/" ^ map_name) in
  let w, h = read_dim json in
  Util.print_int "W: " w;
  Util.print_int "H: " h;
  match build_id_arrays json w with
  | [ tile_id_m; encounter_id_m ], entities_m ->
      let tilesets = json_tilesets json in

      let encounter_l =
        json_encounters (List.assoc "id_tiles.json" tilesets)
      in
      let entities_l =
        json_entities (List.assoc "entities_tilesets.json" tilesets)
      in

      (* let tile_t = List.assoc "forest_tileset.json" tilesets in *)
      (* let tileset_l = json_tileset tilesets in *)

      (* let spritesheet = json_spritesheet tile_t in *)
      let tile_m = build_tile_matrix tile_id_m tilesets in

      set_encounters tile_m encounter_id_m encounter_l map_name;

      let trainers =
        Yojson.Basic.from_file "assets/util/trainers.json"
      in

      let entities =
        generate_entities h entities_l entities_m trainers
      in

      { tiles = tile_m; entities; name = map_name }
  | [], _ | _ :: _, _ -> raise (Malformed_Json "Impossible case!")

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

let get_sprite t c = (get_tile t c).sprite
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

let get_name (m : t) = m.name
let loaded_maps = Util.null ()

let load_maps _ =
  let t = Hashtbl.create 16 in
  Yojson.Basic.from_file "assets/maps/maps.json"
  |> to_list
  |> List.iter (fun j ->
         let file = j |> to_string in
         Hashtbl.add t file (load_map file));
  loaded_maps *= t

let get_entities t = t.entities
let get_map s = Hashtbl.find ~!loaded_maps s

let get_map2 s =
  Hashtbl.find ~!loaded_maps ("assets/maps/" ^ s ^ ".json")

let get_maps _ = ~!loaded_maps
