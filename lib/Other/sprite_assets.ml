open Yojson.Basic.Util

type loaded_assets = {
  spritesheets : (string, Spritesheet.sprite_sheet) Hashtbl.t;
  (* creatures : (string, Creature.creature) Hashtbl.t; *)
  sprites : (string, Draw.sprite) Hashtbl.t;
}

let assets_ref = ref None

let assets _ =
  match !assets_ref with
  | Some a -> a
  | None -> failwith "Assets not loaded yet"

let get_spritesheet s = Hashtbl.find (assets ()).spritesheets s

let get_spritesheet2 name folder =
  Hashtbl.find (assets ()).spritesheets (Draw.sprite_path name folder)

let get_sprite s = Hashtbl.find (assets ()).sprites s

let get_sprite2 name folder =
  Hashtbl.find (assets ()).sprites (Draw.sprite_path name folder)

let load_spritesheets _ =
  let t = Hashtbl.create 16 in
  Yojson.Basic.from_file "assets/entity_sprites/entity_sprites.json"
  |> member "spritesheets" |> to_list
  |> List.iter (fun j ->
         let file =
           "assets/entity_sprites/" ^ (j |> member "file" |> to_string)
         in
         let w = j |> member "w" |> to_int in
         let h = j |> member "h" |> to_int in
         let dpi = j |> member "dpi" |> to_int in
         Hashtbl.add t file (Spritesheet.init_spritesheet file w h dpi));
  Yojson.Basic.from_file "assets/util/creature_list.json"
  |> to_assoc
  |> List.iter (fun (_, j) ->
         let name = j |> member "name" |> to_string in
         let file =
           "assets/creature_sprites/"
           ^ String.lowercase_ascii name
           ^ ".png"
         in
         Hashtbl.add t file (Spritesheet.init_spritesheet file 80 80 3));
  Yojson.Basic.from_file "assets/item_sprites/item_sprites.json"
  |> member "spritesheets" |> to_list
  |> List.iter (fun j ->
         let file =
           "assets/item_sprites/" ^ (j |> member "file" |> to_string)
         in
         let w = j |> member "w" |> to_int in
         let h = j |> member "h" |> to_int in
         let dpi = j |> member "dpi" |> to_int in
         Hashtbl.add t file (Spritesheet.init_spritesheet file w h dpi));
  t

let load_sprites _ =
  let t = Hashtbl.create 16 in
  Yojson.Basic.from_file "assets/gui_sprites/gui_sprites.json"
  |> member "sprites" |> to_list
  |> List.iter (fun j ->
         let file =
           "assets/gui_sprites/" ^ (j |> member "name" |> to_string)
         in
         let dpi = j |> member "dpi" |> to_int in
         Hashtbl.add t file (Draw.load_sprite_from_filepath file dpi ()));
  t

let load _ =
  assets_ref :=
    Some
      { sprites = load_sprites (); spritesheets = load_spritesheets () }
