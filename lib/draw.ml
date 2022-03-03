open Yojson.Basic.Util
open Graphics

let pokemon_sprite_size = 240
let div = 8

let pixels_of_json json =
  json |> member ("xyrgb" ^ string_of_int div) |> to_string

let get_xyrgb values =
  let xyrgb = String.split_on_char '|' values in
  let x = int_of_string (List.nth xyrgb 0)
  and y = int_of_string (List.nth xyrgb 1)
  and r = int_of_string (List.nth xyrgb 2)
  and g = int_of_string (List.nth xyrgb 3)
  and b = int_of_string (List.nth xyrgb 4) in
  (x, y, r, g, b)

let draw_pixel size x y () =
  fill_rect (x - (size / 2)) (y - (size / 2)) size size

let draw_xyrgb xyrgb img_size o_x o_y w h =
  if xyrgb <> "" then begin
    let x, y, r, g, b = get_xyrgb xyrgb in
    let color = rgb r g b in
    set_color color;
    if x < w then
      if y < h then
        draw_pixel 3
          (o_x - (img_size / 2) + x)
          (o_y + (img_size / 2) - y)
          ()
  end

let rec draw_from_pixels list img_size o_x o_y width height =
  match list with
  | [] -> ()
  | h :: t ->
      draw_xyrgb h img_size o_x o_y width height;
      draw_from_pixels t img_size o_x o_y width height

let load_pokemon name () =
  let json =
    Yojson.Basic.from_file ("assets/pokemon_sprites/" ^ name ^ ".json")
  in
  let pixel_divs =
    json |> member "pixels" |> to_list |> List.map pixels_of_json
  in
  List.fold_left
    (fun x y -> x @ String.split_on_char ',' y)
    [] pixel_divs

let draw_sprite pixels o_x o_y width height () =
  draw_from_pixels pixels pokemon_sprite_size o_x o_y width height

let draw_pokemon pixels o_x o_y () =
  draw_from_pixels pixels pokemon_sprite_size o_x o_y
    pokemon_sprite_size pokemon_sprite_size
