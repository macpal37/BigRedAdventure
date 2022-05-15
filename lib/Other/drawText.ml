open Draw
open Spritesheet

type font = {
  font_sprite : sprite_sheet;
  hspacing : int;
  vspacing : int;
}

let f1 = init_spritesheet "assets/gui_sprites/text-font.png" 20 36 1
let f2 = init_spritesheet "assets/gui_sprites/text-font.png" 20 36 2

let get_font i =
  match i with
  | 0 -> f1
  | 1 -> f2
  | _ -> f1

(* let font = Util.malloc f1 *)

let spacing = 16
let draw_pt = Util.new_point ()
let battle_bot = load_sprite "battle_bot" GUI_Folder 3 ()

let draw_char c font () =
  let v = int_of_char c - 33 in
  let x, y = (draw_pt.x, draw_pt.y) in
  draw_sprite (Spritesheet.get_sprite font v) x y ()

let draw_string x y font (t : string) () =
  draw_pt.x <- x;
  draw_pt.y <- y;
  for i = 0 to String.length t - 1 do
    draw_char t.[i] font ();
    draw_pt.x <- draw_pt.x + spacing
  done

let moveto x y =
  draw_pt.x <- x;
  draw_pt.y <- y

(* let rmoveto dx dy = draw_pt.x <- draw_pt.x + dx; draw_pt.y <-
   draw_pt.y + dy *)

let draw_string_colored x y f_size text cust_c shdw_c () =
  let font = get_font f_size in
  moveto x y;
  set_text_color font 1 shdw_c;
  set_text_color font 0 cust_c;
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
  set_text_color font 0 white;
  let transcript = get_text_transcript text char_cap in
  for i = 0 to List.length transcript - 1 do
    draw_string x (y - (40 * i)) font (List.nth transcript i) ()
  done

let text_display = ref ""
let set_text_display (s : string) = text_display := s
let box_cap = 40

let clear_text clear_sprite () =
  let sx = 30 in
  let sy = 128 in
  draw_sprite clear_sprite 3 0 ();
  draw_text_string_pos sx sy 0 box_cap !text_display ()
