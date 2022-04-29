open Draw
open Spritesheet

let f1 = init_spritesheet "assets/gui_sprites/text-font.png" 20 36 1
let f2 = init_spritesheet "assets/gui_sprites/text-font.png" 20 36 2

let get_font i =
  match i with
  | 0 -> f1
  | 1 -> f2
  | _ -> f1

(* let font = Util.malloc f1 *)
let spacing = 16
let cap = 28
let wait_time = 175000
let draw_pt = Util.new_point ()
let battle_bot = load_sprite "battle_bot" GUI_Folder 3 ()

let remove_space text =
  if String.length text > 0 && String.sub text 0 1 = " " then
    String.sub text 1 (String.length text - 1)
  else text

let clear_text clear_sprite () = draw_sprite clear_sprite 3 0 ()

let draw_char c font () =
  let v = int_of_char c - 33 in
  let x, y = (draw_pt.x, draw_pt.y) in
  draw_sprite (Spritesheet.get_sprite font v) x y ()

let explode s =
  let rec exp i l = if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let draw_string x y font str () =
  let char_list = explode str in
  draw_pt.x <- x;
  draw_pt.y <- y;
  for i = 0 to String.length str - 1 do
    draw_char (List.nth char_list i) font ();
    draw_pt.x <- draw_pt.x + spacing
  done

let string_to_char_list s =
  let rec exp i l = if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let moveto x y =
  draw_pt.x <- x;
  draw_pt.y <- y

let rmoveto dx dy =
  draw_pt.x <- draw_pt.x + dx;
  draw_pt.y <- draw_pt.y + dy

let draw_string_colored x y f_size text cust_c shdw_c () =
  let font = get_font f_size in
  moveto x y;
  set_text_color font 1 shdw_c;
  set_text_color font 0 cust_c;
  draw_string x y font text ()

let draw_text text f_size auto sticky () =
  let font = get_font f_size in
  let wait_time = if auto then wait_time else -1 in
  let wait_time = if sticky then 0 else wait_time in
  present_draw (clear_text battle_bot) ();
  set_text_color font 0 text_color;
  let start_x = 30 in
  let start_y = 128 in
  moveto start_x start_y;
  let words = String.split_on_char ' ' text in
  let rec calc_levels w lst = function
    | [] -> lst @ [ w ]
    | h :: t ->
        let new_w = w ^ " " ^ h in
        if String.length new_w < cap then calc_levels new_w lst t
        else calc_levels h (lst @ [ w ]) t
  in
  let levels =
    match words with
    | [] -> []
    | h :: t -> calc_levels h [] t
  in

  let rec scroll_text start max = function
    | [] ->
        if start = 1 then wait wait_time ();
        set_text_color font 0 text_color
    | h :: t ->
        let char_list = string_to_char_list h in
        let rec draw_chars chars =
          match chars with
          | [] -> ()
          | h :: t ->
              draw_char h font ();
              rmoveto spacing 0;
              Input.sleep 0.025 ();
              draw_chars t
        in
        moveto start_x (start_y - (80 * start));
        draw_chars char_list;
        if start == max then begin
          wait wait_time ();
          if sticky = false then present_draw (clear_text battle_bot) ();

          set_text_color font 0 text_color;
          scroll_text 0 max t
        end
        else scroll_text (start + 1) max t
  in

  set_text_color font 0 text_color;
  scroll_text 0 1 levels;
  present ();
  moveto 0 0

let draw_text_string_pos x y f_size char_cap text _ () =
  let font = get_font f_size in
  moveto x y;
  let words = String.split_on_char ' ' text in
  let rec calc_levels w lst = function
    | [] -> lst @ [ w ]
    | h :: t ->
        let new_w = w ^ " " ^ h in
        if String.length new_w < char_cap then calc_levels new_w lst t
        else calc_levels h (lst @ [ w ]) t
  in
  let levels =
    match words with
    | [] -> []
    | h :: t -> calc_levels h [] t
  in

  let rec scroll_text i = function
    | [] -> set_text_color font 0 text_color
    | h :: t ->
        let char_list = string_to_char_list h in
        let rec draw_chars chars =
          match chars with
          | [] -> ()
          | h :: t ->
              draw_char h font ();

              rmoveto spacing 0;
              draw_chars t
        in
        moveto x (y - ((spacing + 5) * i));
        draw_chars char_list;
        scroll_text (i + 1) t
  in
  set_text_color font 1 text_color;
  scroll_text 0 levels

let draw_text_string text () =
  let font = get_font 1 in
  clear_text battle_bot ();
  set_text_color font 1 text_color;
  let start_x = 30 in
  let start_y = 142 in
  moveto start_x start_y;
  let len = String.length text in
  let levels = len / cap in
  let rec scroll_text text start max =
    if start mod 3 = 0 then
      if start <> 0 then set_text_color font 1 text_color;
    if start <> max + 1 then begin
      let text = remove_space text in
      let short_text =
        if String.length text > cap then String.sub text 0 cap else text
      in
      let rest_text =
        if String.length text > cap then
          String.sub text cap
            (String.length text - String.length short_text)
        else ""
      in
      let char_list = string_to_char_list short_text in
      let rec draw_chars chars =
        match chars with
        | [] -> ()
        | h :: t ->
            draw_char h font ();
            rmoveto spacing 0;

            draw_chars t
      in
      moveto start_x (start_y - (50 * (start mod 3)));
      draw_chars char_list;

      scroll_text rest_text (start + 1) max
    end
  in
  scroll_text text 0 levels;
  moveto 0 0
