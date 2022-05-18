open Draw
open Yojson.Basic.Util
open Util

type sprite_sheet = {
  sprites : sprite array;
  rows : int;
  columns : int;
  sheet_width : int;
  sheet_height : int;
  dpi : int;
}

let empty_spritesheet =
  {
    sprites = [||];
    rows = 0;
    columns = 0;
    sheet_width = 0;
    sheet_height = 0;
    dpi = 1;
  }

let rec find x lst =
  match lst with
  | [] -> -1
  | h :: t -> if x = h then 0 else 1 + find x t

let little_sprite (image : Image.image) x y w h =
  let palette = ref [] in
  let pixels = Array.make (w * h) 0 in
  for j = 0 to h - 1 do
    for i = 0 to w - 1 do
      let color, alpha =
        Image.read_rgba image (i + x) (j + y)
          (fun r g b a () -> (Draw.rgb r g b, a))
          ()
      in
      if List.mem color !palette = false then
        palette := !palette @ [ color ];
      if alpha > 0 then
        let index = find color !palette in
        pixels.(i + (j * w)) <- index + 1
      else pixels.(i + (j * w)) <- 0
    done
  done;
  (pixels, !palette)

let init_spritesheet filepath sprite_width sprite_height dpi =
  let image = ImageLib_unix.openfile filepath in
  let w = image.width / sprite_width in
  let h = image.height / sprite_height in
  let sprites = Array.make (w * h) Draw.empty_sprite in
  for j = 0 to h - 1 do
    for i = 0 to w - 1 do
      let pixels, palette =
        little_sprite image (sprite_width * i) (sprite_height * j)
          sprite_width sprite_height
      in
      let sprite =
        create_sprite pixels palette sprite_width sprite_height dpi
      in

      sprites.(i + (j * w)) <- sprite
    done
  done;
  {
    sprites;
    rows = h;
    columns = w + 1;
    sheet_width = image.width + 1;
    sheet_height = image.height + 1;
    dpi;
  }

let get_sprite ss i =
  if i >= 0 && i < Array.length ss.sprites then ss.sprites.(i)
  else empty_sprite

let set_text_color ss i c =
  for j = 0 to Array.length ss.sprites - 1 do
    change_color ss.sprites.(j) (i + 1) c
  done

let loaded_spritesheets = Util.null ()

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
         Hashtbl.add t file (init_spritesheet file w h dpi));
  Yojson.Basic.from_file "assets/util/creature_list.json"
  |> to_assoc
  |> List.iter (fun (_, j) ->
         let name = j |> member "name" |> to_string in
         let file =
           "assets/creature_sprites/"
           ^ String.lowercase_ascii name
           ^ ".png"
         in
         Hashtbl.add t file (init_spritesheet file 80 80 3));
  Yojson.Basic.from_file "assets/item_sprites/item_sprites.json"
  |> member "spritesheets" |> to_list
  |> List.iter (fun j ->
         let file =
           "assets/item_sprites/" ^ (j |> member "file" |> to_string)
         in
         let w = j |> member "w" |> to_int in
         let h = j |> member "h" |> to_int in
         let dpi = j |> member "dpi" |> to_int in
         Hashtbl.add t file (init_spritesheet file w h dpi));
  loaded_spritesheets *= t

let get_spritesheet s = Hashtbl.find ~!loaded_spritesheets s

let get_spritesheet2 name folder =
  Hashtbl.find ~!loaded_spritesheets (Draw.sprite_path name folder)
