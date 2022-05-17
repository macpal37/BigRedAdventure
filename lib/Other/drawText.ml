open Draw
open Spritesheet

type font = {
  font_sprite : sprite_sheet;
  hspacing : int;
  vspacing : int;
}

let f1 =
  {
    font_sprite =
      init_spritesheet "assets/gui_sprites/text-font-small.png" 18 32 1;
    hspacing = 14;
    vspacing = 32;
  }

let f2 =
  {
    font_sprite =
      init_spritesheet "assets/gui_sprites/text-font-medium.png" 20 40 1;
    hspacing = 16;
    vspacing = 46;
  }

let f3 =
  {
    font_sprite =
      init_spritesheet "assets/gui_sprites/text-font-large.png" 32 64 1;
    hspacing = 28;
    vspacing = 64;
  }

let f4 =
  {
    font_sprite =
      init_spritesheet "assets/gui_sprites/text-font-medium.png" 20 40 2;
    hspacing = 30;
    vspacing = 80;
  }

type font_size =
  | Small
  | Medium
  | Large
  | Huge

let get_font i =
  match i with
  | Small -> f1
  | Medium -> f2
  | Large -> f3
  | Huge -> f4
(* let get_font i = match i with | 0 -> f2 | 1 -> f4 | 2 -> f3 | 3 -> f1
   | _ -> f2 *)

(* let font = Util.malloc f1 *)

(* let spacing = 16 *)
let draw_pt = Util.new_point ()
let battle_bot = load_sprite "battle_bot" GUI_Folder 3 ()

let draw_char c font () =
  let v = int_of_char c - 33 in
  let x, y = (draw_pt.x, draw_pt.y) in
  draw_sprite (Spritesheet.get_sprite font v) x y ()

let draw_string x y (font : font) (t : string) () =
  draw_pt.x <- x;
  draw_pt.y <- y;
  for i = 0 to String.length t - 1 do
    draw_char t.[i] font.font_sprite ();
    draw_pt.x <- draw_pt.x + font.hspacing
  done

let moveto x y =
  draw_pt.x <- x;
  draw_pt.y <- y

(* let rmoveto dx dy = draw_pt.x <- draw_pt.x + dx; draw_pt.y <-
   draw_pt.y + dy *)

let draw_string_colored x y f_size text cust_c shdw_c () =
  let font = get_font f_size in
  moveto x y;
  set_text_color font.font_sprite 1 shdw_c;
  set_text_color font.font_sprite 0 cust_c;
  draw_string x y font text ()

let get_text_transcript (t : string) (cc : int) : string list =
  let words = String.split_on_char ' ' t in
  let rec calc_levels w lst = function
    | [] -> lst @ [ w ]
    | h :: t ->
        let new_w = w ^ " " ^ h in
        if String.length new_w < cc then calc_levels new_w lst t
        else calc_levels h (lst @ [ w ]) t
  in
  match words with
  | [] -> []
  | h :: t -> calc_levels h [] t

let draw_text_string_pos x y f_size char_cap (text : string) () =
  let font = get_font f_size in
  set_text_color font.font_sprite 0 white;
  let transcript = get_text_transcript text char_cap in
  for i = 0 to List.length transcript - 1 do
    draw_string x
      (y - (font.vspacing * i))
      font (List.nth transcript i) ()
  done

let text_display = ref ""
let set_text_display (s : string) = text_display := s
let box_cap = 44

let clear_text clear_sprite () =
  let sx = 46 in
  let sy = 136 in
  draw_sprite clear_sprite 3 0 ();
  draw_text_string_pos sx sy Medium box_cap !text_display ()
