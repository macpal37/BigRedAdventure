open Yojson.Basic.Util
open Graphics

type sprite = {
  pixels : int list;
  width : int;
  height : int;
  mutable palette1 : color list;
  mutable palette2 : color list;
  dpi : int;
}

let font_size = ref 50
let text_color = rgb 68 68 68

let empty_sprite =
  {
    pixels = [];
    width = 0;
    height = 0;
    palette1 = [];
    palette2 = [];
    dpi = 1;
  }

let text_bg1 =
  ref
    {
      pixels = [];
      width = 0;
      height = 0;
      palette1 = [];
      palette2 = [];
      dpi = 1;
    }

let text_bg2 =
  ref
    {
      pixels = [];
      width = 0;
      height = 0;
      palette1 = [];
      palette2 = [];
      dpi = 1;
    }

let is_sticky = ref false
let erase_mode = ref false
let synced_mode = ref true

(* let blue = rgb 200 200 240 *)
let width = 800
let height = 720
let sync flag () = auto_synchronize flag
let usync flag () = if synced_mode.contents then auto_synchronize flag

let set_text_bg bg1 bg2 =
  text_bg1.contents <- bg1;
  text_bg2.contents <- bg2

let set_font_size size () =
  font_size.contents <- size;
  set_font
    ("-*-fixed-bold-r-semicondensed--" ^ string_of_int size
   ^ "-*-*-*-*-*-iso8859-1")

let get_dimension sprite = (sprite.width, sprite.height)
let get_font_size = font_size.contents
let set_sticky_text flag = is_sticky.contents <- flag
let set_erase_mode flag = erase_mode.contents <- flag
let set_synced_mode flag = synced_mode.contents <- flag

let sync_draw draw () =
  usync false ();
  draw ();
  usync true ()

let draw_pixel size x y () =
  fill_rect (x - (size / 2)) (y - (size / 2)) size size

let draw_from_pixels sprite x y min_w min_h max_w max_h () =
  let rec draw_from_pixels_rec pixels x y tx ty =
    match pixels with
    | [] -> set_color text_color
    | h :: t ->
        if h <> 0 && tx >= min_w && ty >= min_h then begin
          if erase_mode.contents then set_color (point_color 0 0)
          else set_color (List.nth sprite.palette1 (h - 1));

          draw_pixel sprite.dpi (x + tx) (y + ty) ()
        end;
        if ty < max_h then
          if tx < max_w - sprite.dpi then
            draw_from_pixels_rec t x y (tx + sprite.dpi) ty
          else draw_from_pixels_rec t x y 0 (ty + sprite.dpi)
        else set_color text_color
  in

  draw_from_pixels_rec sprite.pixels x y 0 0

let pixels_of_json json =
  let pixels_strings = String.split_on_char ',' (json |> to_string) in
  List.map (fun x -> int_of_string x) pixels_strings

let string_to_color color_json =
  let color_str = color_json |> to_string in
  let rgblist = String.split_on_char ',' color_str in
  rgb
    (int_of_string (List.nth rgblist 0))
    (int_of_string (List.nth rgblist 1))
    (int_of_string (List.nth rgblist 2))

let load_json json dpi =
  {
    pixels =
      List.rev
        (json |> member "pixels" |> to_list |> List.map pixels_of_json)
      |> List.fold_left (fun y x -> y @ x) [];
    width = (json |> member "width" |> to_int) * dpi;
    height = (json |> member "height" |> to_int) * dpi;
    palette1 =
      json |> member "color_palette1" |> to_list
      |> List.map string_to_color;
    palette2 =
      json |> member "color_palette2" |> to_list
      |> List.map string_to_color;
    dpi;
  }

let load_sprite filename dpi () =
  let json = Yojson.Basic.from_file ("assets/" ^ filename ^ ".json") in
  load_json json dpi

let load_creature name () =
  let json =
    Yojson.Basic.from_file ("assets/creature_sprites/" ^ name ^ ".json")
  in
  load_json json 3

let clear_sprite sprite x y () =
  usync false ();
  set_erase_mode true;
  draw_from_pixels sprite x y 0 0 sprite.width sprite.height ();
  set_erase_mode false;
  usync true ()

let draw_sprite_crop
    sprite
    x
    y
    (width_min, width_max)
    (height_min, height_max)
    () =
  usync false ();
  draw_from_pixels sprite x y width_min height_min width_max height_max
    ();
  usync true ()

let draw_sprite sprite x y () =
  usync false ();
  draw_from_pixels sprite x y 0 0 sprite.width sprite.height ();
  usync true ()

let draw_creature sprite player () =
  if player then draw_sprite sprite 50 166 ()
  else
    draw_sprite sprite
      (width - sprite.width - 50)
      (height - 50 - sprite.height)
      ()

let string_to_char_list s =
  let rec exp i l = if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let remove_space text =
  if String.length text > 0 && String.sub text 0 1 = " " then
    String.sub text 1 (String.length text - 1)
  else text

let rec wait () =
  if is_sticky.contents = true then ()
  else if Graphics.key_pressed () then begin
    if Graphics.read_key () = 'x' then ()
  end
  else wait ()

let clear_text () =
  draw_sprite text_bg1.contents 3 0 ();
  draw_sprite text_bg2.contents 400 0 ()

let text_char_cap = ref 28
let set_text_char_cap cap = text_char_cap.contents <- cap

let draw_text text font_size () =
  set_font_size font_size ();
  let char_cap = text_char_cap.contents in
  clear_text ();
  set_color text_color;
  let start_x = 35 in
  let start_y = 132 in
  moveto start_x start_y;
  let len = String.length text in
  let levels = len / char_cap in
  let rec scroll_text text start max =
    if start mod 2 = 0 then
      if start <> 0 && is_sticky.contents = false then begin
        wait ();
        clear_text ();
        set_color text_color
      end;
    if start <> max + 1 then begin
      let text = remove_space text in
      let short_text =
        if String.length text > char_cap then String.sub text 0 char_cap
        else text
      in
      let rest_text =
        if String.length text > char_cap then
          String.sub text char_cap
            (String.length text - String.length short_text)
        else ""
      in
      let char_list = string_to_char_list short_text in
      let rec draw_chars chars =
        match chars with
        | [] -> ()
        | h :: t ->
            set_color text_color;
            draw_char h;
            rmoveto (-15) 4;
            set_color white;
            draw_char h;
            rmoveto 2 (-4);
            set_color text_color;
            Input.sleep 0.025 ();
            draw_chars t
      in

      moveto start_x (start_y - (60 * (start mod 2)));
      draw_chars char_list;

      scroll_text rest_text (start + 1) max
    end
  in
  scroll_text text 0 levels;
  if levels <= 0 then wait ()

(* create a gradient of colors from black at 0,0 to white at w-1,h-1 *)
let gradient arr w h =
  for y = 0 to h - 1 do
    for x = 0 to w - 1 do
      let s = 255 * (x + y) / (w + h - 2) in
      arr.(y).(x) <- rgb s s s
    done
  done

let draw_gradient w h =
  (* w and h are flipped from perspective of the matrix *)
  let arr = Array.make_matrix h w white in
  gradient arr w h;
  draw_image (make_image arr) 0 0

let draw_string_colored x y dif text custom_color () =
  moveto x y;
  set_color text_color;
  draw_string text;
  moveto (x + dif - 1) (y + dif);
  set_color custom_color;
  draw_string text;
  set_color text_color
