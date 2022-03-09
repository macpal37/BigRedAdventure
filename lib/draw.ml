open Yojson.Basic.Util
open Graphics

type pixel = {
  r : int;
  g : int;
  b : int;
}

let text_bg1 = ref [ "" ]
let text_bg2 = ref [ "" ]
let blue = rgb 200 200 240
let width = 800
let height = 720
let pokemon_sprite_size = 240
let div = 8
let sync flag () = auto_synchronize flag

let set_text_bg bg1 bg2 =
  text_bg1.contents <- bg1;
  text_bg2.contents <- bg2

let sync_draw draw () =
  sync false ();
  draw ();
  sync true ()

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

(* let divs_to_rgb acc pixel = let vals = String.split_on_char '|' pixel
   in match vals with | [] -> acc | [ a; b; c ] -> (a, b, c) :: acc | _
   -> acc *)

let load_sprite filename () =
  let json = Yojson.Basic.from_file ("assets/" ^ filename ^ ".json") in
  let pixel_divs =
    json |> member "pixels" |> to_list |> List.map pixels_of_json
  in
  List.fold_left
    (fun x y -> x @ String.split_on_char ',' y)
    [] pixel_divs

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
  sync false ();
  draw_from_pixels pixels pokemon_sprite_size o_x o_y width height;
  sync true ()

let draw_creature_pos pixels o_x o_y () =
  sync false ();
  draw_from_pixels pixels pokemon_sprite_size o_x o_y
    pokemon_sprite_size pokemon_sprite_size;
  sync true ()

let draw_creature pixels player () =
  if player then
    draw_creature_pos pixels
      ((pokemon_sprite_size / 2) + 50)
      ((pokemon_sprite_size / 2) + 160)
      ()
  else
    draw_creature_pos pixels
      (width - (pokemon_sprite_size / 2) - 50)
      (height - (pokemon_sprite_size / 2) - 50)
      ()

let string_to_char_list s =
  let rec exp i l = if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let remove_space text =
  if String.length text > 0 && String.sub text 0 1 = " " then
    String.sub text 1 (String.length text - 1)
  else text

let rec wait () =
  if Graphics.key_pressed () then begin
    print_endline "Stop?";
    if Graphics.read_key () = 'x' then ()
  end
  else wait ()

let clear_text () =
  draw_sprite text_bg1.contents 128 88 396 216 ();
  draw_sprite text_bg2.contents 524 88 396 216 ()

let text_char_cap = ref 28
let set_text_char_cap cap = text_char_cap.contents <- cap

let draw_text text () =
  let char_cap = text_char_cap.contents in
  (* set_color (rgb 200 50 50); fill_rect 0 0 width 212; *)
  clear_text ();
  set_color black;
  let start_x = 35 in
  let start_y = 132 in
  moveto start_x start_y;
  let len = String.length text in
  let levels = len / char_cap in
  let rec scroll_text text start max =
    if start mod 3 = 0 then
      if start <> 0 then begin
        wait ();
        clear_text ();
        set_color black
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
            set_color black;
            let x = current_x () in
            draw_char h;
            print_endline (string_of_int (current_x () - x));
            rmoveto (-15) 4;
            set_color white;
            draw_char h;
            rmoveto 2 (-4);
            set_color black;
            Unix.sleepf 0.025;
            draw_chars t
      in

      moveto start_x (start_y - (50 * (start mod 3)));
      draw_chars char_list;

      scroll_text rest_text (start + 1) max
    end
  in
  scroll_text text 0 levels;
  wait ()

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

let damage_render creature_pixels player () =
  let rec damage_render_rec c creature_pixels player () =
    if c = 0 then draw_creature creature_pixels player ()
    else begin
      if c mod 2 = 0 then draw_creature creature_pixels player ()
      else begin
        set_color blue;
        sync false ();
        draw_pixel
          (pokemon_sprite_size + 4)
          (width - (pokemon_sprite_size / 2) - 50)
          (height - (pokemon_sprite_size / 2) - 50)
          ();
        sync true ()
      end;
      Unix.sleepf 0.20;
      damage_render_rec (c - 1) creature_pixels player ()
    end
  in
  damage_render_rec 7 creature_pixels player ();
  set_color black

let rec faint base c creature_pixels () =
  if c = base - 2 then
    sync_draw
      (draw_pixel
         (pokemon_sprite_size + 4)
         (width - (pokemon_sprite_size / 2) - 50)
         (height - (pokemon_sprite_size / 2) - 50))
      ()
  else begin
    set_color blue;

    sync_draw
      (draw_pixel
         (pokemon_sprite_size + 4)
         (width - (pokemon_sprite_size / 2) - 50)
         (height - (pokemon_sprite_size / 2) - 50))
      ();
    draw_sprite creature_pixels
      (width - (pokemon_sprite_size / 2) - 50)
      (height
      - (pokemon_sprite_size / 2)
      - 50
      - (pokemon_sprite_size - (pokemon_sprite_size / c)))
      240 (240 / c) ();
    Unix.sleepf 0.05;
    set_color blue;

    faint base (c + 1) creature_pixels ()
  end;
  set_color black

let animate_faint creature () = faint 20 2 creature ()

let draw_health_bar max before after player () =
  let blank = rgb 84 97 89 in
  let yellow = rgb 221 193 64 in
  let red = rgb 246 85 55 in
  let green = rgb 103 221 144 in
  let d a b c =
    let x = a * b in
    x / c
  in
  let hwidth = 320 in
  let hheight = 10 in
  let xh, yh =
    if player then (width - hwidth - 40, 240) else (80, 600)
  in
  set_color black;
  set_line_width 8;
  draw_rect xh yh hwidth hheight;

  set_color blank;
  fill_rect xh yh hwidth hheight;
  print_endline (string_of_int (d before 100 max));
  set_color green;
  let before_bar = d before 100 max in
  fill_rect xh yh (d before_bar hwidth 100) hheight;

  let after_bar = d after 100 max in
  let rec render_health start target =
    if start = target then ()
    else if start > target then begin
      if start == 1 then set_color blank
      else if start <= 20 then set_color red
      else if start <= 50 then set_color yellow
      else set_color green;
      fill_rect xh yh (d start hwidth 100) hheight;
      set_color blank;
      fill_rect (xh + d start hwidth 100 - 4) yh 4 hheight;

      Unix.sleepf 0.05;
      render_health (start - 1) target
    end
    else begin
      if start == 1 then set_color blank
      else if start <= 20 then set_color red
      else if start <= 50 then set_color yellow
      else set_color green;

      fill_rect xh yh (d hwidth start 100) hheight;
      Unix.sleepf 0.05;
      render_health (start + 1) target
    end
  in
  render_health before_bar after_bar
