open Yojson.Basic.Util
open Graphics
open Util

type sprite = {
  pixels : int list;
  width : int;
  height : int;
  mutable palette1 : color list;
  mutable palette2 : color list;
}

let text_color = rgb 68 68 68

let empty_sprite =
  { pixels = []; width = 0; height = 0; palette1 = []; palette2 = [] }

let text_bg1 =
  ref
    { pixels = []; width = 0; height = 0; palette1 = []; palette2 = [] }

let text_bg2 =
  ref
    { pixels = []; width = 0; height = 0; palette1 = []; palette2 = [] }

let is_sticky = ref false
let blue = rgb 200 200 240
let width = 800
let height = 720
let sync flag () = auto_synchronize flag

let set_text_bg bg1 bg2 =
  text_bg1.contents <- bg1;
  text_bg2.contents <- bg2

let set_font_size size () =
  set_font
    ("-*-fixed-bold-r-semicondensed--" ^ string_of_int size
   ^ "-*-*-*-*-*-iso8859-1")

let set_sticky_text flag = is_sticky.contents <- flag

let sync_draw draw () =
  sync false ();
  draw ();
  sync true ()

let draw_pixel size x y () =
  fill_rect (x - (size / 2)) (y - (size / 2)) size size

let draw_from_pixels sprite x y width height () =
  let rec draw_from_pixels_rec pixels x y tx ty width height =
    match pixels with
    | [] -> set_color text_color
    | h :: t ->
        if h <> 0 then begin
          let color = List.nth sprite.palette1 (h - 1) in
          set_color color;
          draw_pixel 3 (x + tx) (y - ty) ()
        end;
        if ty < height - 3 then
          if tx < width - 3 then
            draw_from_pixels_rec t x y (tx + 3) ty width height
          else draw_from_pixels_rec t x y 0 (ty + 3) width height
        else set_color text_color
  in

  draw_from_pixels_rec sprite.pixels x y 0 0 width height

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

let load_sprite filename () =
  let json = Yojson.Basic.from_file ("assets/" ^ filename ^ ".json") in
  {
    pixels =
      json |> member "pixels" |> to_list |> List.map pixels_of_json
      |> List.fold_left (fun x y -> x @ y) [];
    width = (json |> member "width" |> to_int) * 3;
    height = (json |> member "height" |> to_int) * 3;
    palette1 =
      json |> member "color_palette1" |> to_list
      |> List.map string_to_color;
    palette2 =
      json |> member "color_palette2" |> to_list
      |> List.map string_to_color;
  }

let load_creature name () =
  let json =
    Yojson.Basic.from_file ("assets/creature_sprites/" ^ name ^ ".json")
  in
  {
    pixels =
      json |> member "pixels" |> to_list |> List.map pixels_of_json
      |> List.fold_left (fun x y -> x @ y) [];
    width = (json |> member "width" |> to_int) * 3;
    height = (json |> member "height" |> to_int) * 3;
    palette1 =
      json |> member "color_palette1" |> to_list
      |> List.map string_to_color;
    palette2 =
      json |> member "color_palette2" |> to_list
      |> List.map string_to_color;
  }

let draw_sprite_crop sprite x y width height () =
  sync false ();
  draw_from_pixels sprite x y width height ();
  sync true ()

let draw_sprite sprite x y () =
  sync false ();
  draw_from_pixels sprite x y sprite.width sprite.height ();
  sync true ()

let draw_creature sprite player () =
  if player then draw_sprite sprite 50 (sprite.height + 164) ()
  else draw_sprite sprite (width - sprite.width - 50) (height - 50) ()

let string_to_char_list s =
  let rec exp i l = if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let remove_space text =
  if String.length text > 0 && String.sub text 0 1 = " " then
    String.sub text 1 (String.length text - 1)
  else text

let rec wait () =
  if is_sticky.contents = true then ();
  if Graphics.key_pressed () then begin
    if Graphics.read_key () = 'x' then ()
  end
  else wait ()

let clear_text () =
  sync false ();
  let h = 216 in
  draw_sprite text_bg1.contents 3 h ();
  draw_sprite text_bg2.contents 400 h ();
  sync true ()

let text_char_cap = ref 28
let set_text_char_cap cap = text_char_cap.contents <- cap

let draw_text text () =
  let char_cap = text_char_cap.contents in
  (* set_color (rgb 200 50 50); fill_rect 0 0 width 212; *)
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
            (* let x = current_x () in *)
            draw_char h;
            (* print_endline (string_of_int (current_x () - x)); *)
            rmoveto (-15) 4;
            set_color white;
            draw_char h;
            rmoveto 2 (-4);
            set_color text_color;
            Unix.sleepf 0.025;
            draw_chars t
      in

      moveto start_x (start_y - (60 * (start mod 2)));
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

let damage_render sprite player () =
  let rec damage_render_rec c creature_pixels player () =
    if c = 0 then draw_creature creature_pixels player ()
    else begin
      if c mod 2 = 0 then draw_creature creature_pixels player ()
      else begin
        set_color blue;
        sync false ();
        draw_pixel (sprite.width + 4)
          (width - (sprite.width / 2) - 50)
          (height - (sprite.height / 2) - 50)
          ();
        sync true ()
      end;
      Unix.sleepf 0.20;
      damage_render_rec (c - 1) creature_pixels player ()
    end
  in
  damage_render_rec 7 sprite player ();
  set_color text_color

let rec faint base c sprite () =
  if c = base - 2 then
    sync_draw
      (draw_pixel (sprite.width + 4)
         (width - (sprite.width / 2) - 50)
         (height - (sprite.height / 2) - 50))
      ()
  else begin
    set_color blue;

    sync false ();
    draw_pixel (sprite.width + 4)
      (width - (sprite.width / 2) - 50)
      (height - (sprite.height / 2) - 50)
      ();
    draw_sprite_crop sprite
      (width - 50 - sprite.width)
      (height - 50 - (sprite.height - (sprite.height / c)))
      240 (240 / c) ();
    Unix.sleepf 0.05;
    set_color blue;

    faint base (c + 1) sprite ()
  end;
  set_color text_color

let animate_faint creature () = faint 20 2 creature ()

let hp_to_string hp =
  if hp < 10 then "  " ^ string_of_int hp
  else if hp < 100 then " " ^ string_of_int hp
  else string_of_int hp

let draw_hp_val x y curr max player () =
  if player = false then ()
  else
    let combat_bg = rgb 248 248 216 in
    set_font_size 30 ();
    moveto x y;
    set_color combat_bg;
    fill_rect (current_x () - 2) (current_y () + 4) 100 24;
    set_color text_color;
    draw_string (hp_to_string curr ^ "/" ^ hp_to_string max)

let draw_health_bar max before after player () =
  let max, before, after =
    (bound max 0 max, bound before 0 max, bound after 0 max)
  in
  let blank = rgb 84 97 89 in
  let bar_yellow = rgb 221 193 64 in
  let bar_red = rgb 246 85 55 in
  let bar_green = rgb 103 221 144 in
  let d a b c =
    let x = a * b in
    x / c
  in
  let hwidth = 210 in
  let hheight = 6 in
  let xh, yh =
    if player then (width - hwidth - 31 - 10, 296) else (130, 615)
  in
  set_color text_color;
  set_line_width 8;
  draw_rect xh yh hwidth hheight;
  if player then
    draw_hp_val
      (xh + (hwidth / 2))
      (yh - hheight - 5 - 22)
      before max player ();
  set_color blank;
  fill_rect xh yh hwidth hheight;
  set_color bar_green;
  let before_bar = d before 100 max in
  fill_rect xh yh (d before_bar hwidth 100) hheight;

  let after_bar = d after 100 max in
  let rec render_health start target =
    if start = target || start <= 0 then ()
    else if start > target then begin
      (*=====LOSING HEALTH=====*)
      if start == 1 then set_color blank
      else if start <= 20 then set_color bar_red
      else if start <= 50 then set_color bar_yellow
      else set_color bar_green;
      fill_rect xh yh (d start hwidth 100) hheight;
      set_color blank;
      fill_rect (xh + d start hwidth 100 - 4) yh 4 hheight;
      (* HP NUMBER *)
      draw_hp_val
        (xh + (hwidth / 2))
        (yh - hheight - 5 - 22)
        (d max start 100) max player ();

      Unix.sleepf 0.05;
      render_health (start - 1) target
    end
    else begin
      (*=====Gaining HEALTH=====*)
      if start == 1 then set_color blank
      else if start <= 20 then set_color bar_red
      else if start <= 50 then set_color bar_yellow
      else set_color bar_green;
      (* HP NUMBER *)
      draw_hp_val
        (xh + (hwidth / 2))
        (yh - hheight - 5 - 22)
        (d max start 100) max player ();

      fill_rect xh yh (d hwidth start 100) hheight;
      Unix.sleepf 0.05;
      render_health (start + 1) target
    end
  in
  render_health before_bar after_bar;
  draw_hp_val
    (xh + (hwidth / 2))
    (yh - hheight - 5 - 22)
    (if after >= 0 then after else 0)
    max player ()

let draw_exp_bar max before after () =
  let max, before, after =
    (bound max 0 max, bound before 0 after, bound after 0 max)
  in
  let blank = rgb 209 199 156 in
  let bar_color = rgb 77 195 232 in
  let d a b c =
    let x = a * b in
    x / c
  in
  let hwidth = 250 in
  let hheight = 6 in
  let xh, yh = (width - hwidth - 30, 240) in
  set_color text_color;
  (* set_line_width 8; draw_rect xh yh hwidth hheight; *)
  set_color blank;
  fill_rect xh yh hwidth hheight;
  set_color bar_color;
  let before_bar = d before 100 max in
  fill_rect xh yh (d before_bar hwidth 100) hheight;

  let after_bar = d after 100 max in
  let rec render_bar_progress start target =
    if start = target || start <= 0 then ()
    else if start <= target then begin
      set_color bar_color;
      fill_rect xh yh (d start hwidth 100 + 4) hheight;
      (* set_color blank; fill_rect (xh + d start hwidth 100) yh 2
         hheight; *)
      Unix.sleepf 0.05;
      render_bar_progress (start + 1) target
    end
  in
  render_bar_progress before_bar after_bar;
  set_color text_color

let draw_combat_hud sprite name level player (max, before, after) () =
  if player then begin
    set_font_size 30 ();
    draw_sprite sprite (width - 368 + 30) (456 - 100) ();
    moveto (width - 320) 316;
    draw_string (String.uppercase_ascii name);
    moveto (width - 100) 316;
    draw_string ("Lv" ^ string_of_int level);
    draw_health_bar max before after player ()
  end
  else begin
    set_font_size 30 ();
    draw_sprite sprite 42 (height - 45) ();
    moveto 60 (height - 85);
    draw_string (String.uppercase_ascii name);
    moveto 280 (height - 85);
    draw_string ("Lv" ^ string_of_int level);
    draw_health_bar max before after player ()
  end;
  set_font_size 40 ()

let draw_combat_commands c redraw () =
  set_font_size 50 ();
  let x, y = (475, 120) in
  sync false ();
  if redraw then clear_text ();
  moveto x y;
  draw_string "FIGHT";
  moveto x (y - 75);
  draw_string "PARTY";
  moveto (x + 200) y;
  draw_string "BAG";
  moveto (x + 200) (y - 75);
  draw_string "RUN";
  moveto
    (x - 40 + (200 * if c = 1 || c = 3 then 1 else 0))
    (y - (75 * if c >= 2 then 1 else 0));
  draw_char '>';
  sync true ();
  set_font_size 40 ()
