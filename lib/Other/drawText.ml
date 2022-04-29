open Draw

(* let text_font = Spritesheet.init_spritesheet
   "assets/gui_sprites/text-font3.png" 32 50 1 *)
let text_font =
  Spritesheet.init_spritesheet "assets/gui_sprites/text-font4.png" 21 33
    1

let font_size = ref 50
let set_font_size size () = font_size := size
let get_font_size () = !font_size
let spacing = 18
let text_char_cap = 28
let auto_text_time = 175000
let draw_point = Util.new_point ()
let battle_bot = load_sprite "battle_bot" GUI_Folder 3 ()

let remove_space text =
  if String.length text > 0 && String.sub text 0 1 = " " then
    String.sub text 1 (String.length text - 1)
  else text

let clear_text clear_sprite () =
  (* sync false (); *)
  draw_sprite clear_sprite 3 0 ()

(* sync true () *)
let draw_char c () =
  let v = int_of_char c in
  let x, y = (draw_point.x, draw_point.y) in
  if v >= 65 && v <= 90 then
    draw_sprite (Spritesheet.get_sprite text_font (v - 65)) x y ()
  else if v >= 97 && v <= 122 then
    draw_sprite (Spritesheet.get_sprite text_font (v - 70)) x y ()
  else if v >= 48 && v <= 57 then
    draw_sprite (Spritesheet.get_sprite text_font (v + 6)) x y ()
  else if v = 47 then
    draw_sprite (Spritesheet.get_sprite text_font 73) x y ()
(* else draw_sprite (Spritesheet.get_sprite text_font 74) x y () *)

let explode s =
  let rec exp i l = if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let draw_string str x y () =
  let char_list = explode str in
  draw_point.x <- x;
  draw_point.y <- y;
  for i = 0 to String.length str - 1 do
    draw_char (List.nth char_list i) ();
    draw_point.x <- draw_point.x + spacing
  done

let string_to_char_list s =
  let rec exp i l = if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let draw_string_colored
    x
    y
    shadow_offset
    font_size
    text
    custom_color
    shadow_color
    () =
  let cache_font_size = get_font_size () in
  set_font_size font_size ();
  moveto x y;
  set_color shadow_color;
  draw_string text x y ();
  moveto (x + shadow_offset - 1) (y + shadow_offset);
  set_color custom_color;
  draw_string text x y ();
  set_color text_color;
  set_font_size cache_font_size ();
  moveto 0 0

let draw_text text font_size auto sticky () =
  set_font_size font_size ();
  let wait_time = if auto then auto_text_time else -1 in
  let wait_time = if sticky then 0 else wait_time in

  present_draw (clear_text battle_bot) ();

  set_color text_color;
  let start_x = 30 in
  let start_y = 128 in
  moveto start_x start_y;
  let words = String.split_on_char ' ' text in
  let rec calc_levels w lst = function
    | [] -> lst @ [ w ]
    | h :: t ->
        let new_w = w ^ " " ^ h in
        if String.length new_w < text_char_cap then
          calc_levels new_w lst t
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
        set_color text_color
    | h :: t ->
        let char_list = string_to_char_list h in
        let rec draw_chars chars =
          match chars with
          | [] -> ()
          | h :: t ->
              draw_char h ();
              rmoveto spacing 0;

              Input.sleep 0.025 ();
              draw_chars t
        in
        moveto start_x (start_y - (60 * start));
        draw_chars char_list;
        if start == max then begin
          wait wait_time ();
          if sticky = false then present_draw (clear_text battle_bot) ();

          set_color text_color;
          scroll_text 0 max t
        end
        else scroll_text (start + 1) max t
  in

  set_color text_color;
  scroll_text 0 1 levels;
  present ();
  moveto 0 0

let draw_text_string_pos x y font_size char_cap text _ () =
  set_font_size font_size ();
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
    | [] -> set_color text_color
    | h :: t ->
        let char_list = string_to_char_list h in
        let rec draw_chars chars =
          match chars with
          | [] -> ()
          | h :: t ->
              draw_char h ();

              rmoveto spacing 0;
              draw_chars t
        in
        moveto x (y - ((font_size + 5) * i));
        draw_chars char_list;
        scroll_text (i + 1) t
  in
  set_color text_color;
  scroll_text 0 levels;
  moveto 0 0

let draw_text_string text () =
  set_font_size 40 ();
  clear_text battle_bot ();
  set_color text_color;
  let start_x = 30 in
  let start_y = 142 in
  moveto start_x start_y;
  let len = String.length text in
  let levels = len / text_char_cap in
  let rec scroll_text text start max =
    if start mod 3 = 0 then if start <> 0 then set_color text_color;
    if start <> max + 1 then begin
      let text = remove_space text in
      let short_text =
        if String.length text > text_char_cap then
          String.sub text 0 text_char_cap
        else text
      in
      let rest_text =
        if String.length text > text_char_cap then
          String.sub text text_char_cap
            (String.length text - String.length short_text)
        else ""
      in
      let char_list = string_to_char_list short_text in
      let rec draw_chars chars =
        match chars with
        | [] -> ()
        | h :: t ->
            draw_char h ();
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
