open Yojson.Basic.Util
open Graphics

type pixel = {
  x : int;
  y : int;
  rgb : int * int * int;
}

(* let width = 800 *)

(* let height = 720 *)
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

let load_creature name () =
  let json =
    Yojson.Basic.from_file ("assets/creature_sprites/" ^ name ^ ".json")
  in
  let pixel_divs =
    json |> member "pixels" |> to_list |> List.map pixels_of_json
  in
  List.fold_left
    (fun x y -> x @ String.split_on_char ',' y)
    [] pixel_divs

let draw_sprite pixels o_x o_y width height () =
  draw_from_pixels pixels pokemon_sprite_size o_x o_y width height

let draw_creature pixels o_x o_y () =
  draw_from_pixels pixels pokemon_sprite_size o_x o_y
    pokemon_sprite_size pokemon_sprite_size

let string_to_char_list s =
  let rec exp i l = if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let remove_space text =
  if String.sub text 0 1 = " " then
    String.sub text 1 (String.length text - 1)
  else text

let rec wait () =
  if Graphics.key_pressed () then begin
    print_endline "Stop?";
    if Graphics.read_key () = 'x' then ()
  end
  else wait ()

let draw_text text () =
  moveto 30 145;
  let len = String.length text in
  let levels = len / 32 in
  let rec scroll_text text start max =
    if start mod 3 = 0 then
      if start <> 0 then begin
        wait ();
        set_color (rgb 200 50 50);
        fill_rect 0 0 800 212;
        set_color black;
        print_endline text;
        print_endline (string_of_int start)
      end;
    if start <> max + 1 then begin
      let text = remove_space text in
      let short_text =
        if String.length text > 32 then String.sub text 0 32 else text
      in
      let rest_text =
        if String.length text > 32 then
          String.sub text 32
            (String.length text - String.length short_text)
        else ""
      in
      let char_list = string_to_char_list short_text in
      let rec draw_chars chars =
        match chars with
        | [] -> ()
        | h :: t ->
            draw_char h;
            rmoveto 2 0;
            Unix.sleepf 0.025;
            draw_chars t
      in
      moveto 30 (145 - (50 * (start mod 3)));
      draw_chars char_list;

      scroll_text rest_text (start + 1) max
    end
  in
  scroll_text text 0 levels
