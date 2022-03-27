open Draw
open Graphics
open Creature
open Animation

(* let max_creatures = 5 let max_creatures = ref 0 *)
let party_menu_bg = load_sprite "party_menu" GUI_Folder 3 ()
let active = load_sprite "active_party_creature" GUI_Folder 3 ()
let minimenu1 = load_sprite "party_minimenu" GUI_Folder 3 ()
let chibi = load_sprite "rafu_front" Creature_Folder 1 ()
let menu_position = Util.new_point ()
let minimenu_position = Util.new_point ()

let move_x x () =
  if minimenu_position.x = -1 then
    if
      menu_position.x + x >= 0
      && menu_position.x + x < 2
      && List.length (Player.party (State.player ())) > 1
    then menu_position.x <- menu_position.x + x

let move_y y () =
  if minimenu_position.x = -1 then begin
    if
      menu_position.y + y >= 0
      && menu_position.y + y
         < List.length (Player.party (State.player ())) - 1
    then menu_position.y <- menu_position.y + y
  end
  else if minimenu_position.y + y >= 0 && minimenu_position.y + y < 4
  then minimenu_position.y <- minimenu_position.y + y

let draw_menu lead_creature () =
  Ui.add_first_background (draw_sprite party_menu_bg 0 0);
  Ui.add_first_foreground
    (draw_string_colored 40 36 3 50 "Choose a CREATURE" text_color2);
  Ui.add_first_foreground
    (draw_string_colored 24 605 4 60 "PARTY" (rgb 255 170 40));
  Ui.add_first_gameplay
    (draw_sprite (get_front_sprite lead_creature) 12 318);
  Ui.add_first_gameplay
    (draw_string_colored 20 246 1 30 (get_nickname lead_creature) white);
  Ui.add_first_gameplay
    (draw_string_colored 20 220 1 20
       ("LVL: " ^ string_of_int (get_level lead_creature))
       white);
  let max, _, aft = get_hp_status lead_creature in
  Ui.add_first_gameplay (draw_health_bar max aft aft 56 192 180 6 true)

let draw_creature_status creature pos () =
  let x, y = (276, 577 - (112 * pos)) in
  let xx = 294 + 80 in
  let yy = 610 - (112 * pos) in
  Ui.add_first_background (draw_sprite chibi x (y + 10));
  Ui.add_first_background (draw_sprite active x y);
  Ui.add_first_gameplay
    (draw_string_colored xx (yy + 26) 1 30 (get_nickname creature) white);
  Ui.add_first_gameplay
    (draw_string_colored xx yy 1 20
       ("LVL: " ^ string_of_int (get_level creature))
       white);
  let max, _, aft = get_hp_status creature in
  Ui.add_first_gameplay
    (draw_health_bar max aft aft (xx + 130) (yy + 6) 250 6 true)

let draw_selector () =
  set_line_width 4;
  if menu_position.x = 1 then begin
    let x, y = (276, 577 - (112 * menu_position.y)) in
    set_color red;
    draw_rect x y 510 100
  end
  else
    let x, y = (9, 140) in
    set_color red;
    draw_rect x y 254 152

let refresh () =
  let length = List.length (Player.party (State.player ())) in
  if length > 1 then
    for i = 0 to length - 2 do
      draw_creature_status
        (Player.party_i (State.player ()) (i + 1))
        i ()
    done;

  Ui.add_first_foreground draw_selector;
  draw_menu (Player.party_i (State.player ()) 0) ()

let minimenu () =
  let x, y, dif, f = (630, 166, 44, 40) in
  Ui.add_first_foreground
    (draw_string_colored (x - 30)
       (y - 5 - (dif * minimenu_position.y))
       2 50 ">" text_color2);
  Ui.add_first_foreground
    (draw_string_colored x (y - (dif * 0)) 2 f "Summary" text_color2);
  Ui.add_first_foreground
    (draw_string_colored x (y - (dif * 1)) 2 f "Switch" text_color2);
  Ui.add_first_foreground
    (draw_string_colored x (y - (dif * 2)) 2 f "Item" text_color2);
  Ui.add_first_foreground
    (draw_string_colored x (y - (dif * 3)) 2 f "Back" text_color2);
  Ui.add_first_foreground (draw_sprite minimenu1 576 12)

let run_tick () =
  (* Input.poll (); *)
  let key =
    match Input.key_option () with
    | Some c -> c
    | None -> '#'
  in

  if key = 'w' then move_y (-1) ();
  if key = 's' then move_y 1 ();
  if key = 'a' then move_x (-1) ();
  if key = 'd' then move_x 1 ();

  if
    (key = 'w' || key = 's' || key = 'a' || key = 'd')
    && minimenu_position.x = -1
  then refresh ();
  if (key = 'w' || key = 's') && minimenu_position.x = 0 then
    minimenu ();

  if key = 'e' && minimenu_position.x = 0 && minimenu_position.y = 0
  then begin
    let i = if menu_position.x = 0 then 0 else menu_position.y + 1 in
    Creature_menu.set_creature i;
    Creature_menu.init ();
    Creature_menu.run_tick ();
    refresh ()
  end;

  if key = 'e' then begin
    minimenu_position.x <- 0;
    minimenu ()
  end;
  if key = 'q' then begin
    minimenu_position.y <- 0;
    minimenu_position.x <- -1;
    refresh ()
  end;
  Ui.update_all ()
(* if key <> 'q' then run_tick () else Draw.set_synced_mode true *)

let init () =
  minimenu_position.x <- -1;
  Draw.set_synced_mode false;
  refresh ();
  Ui.add_first_background clear_screen
(* run_tick () *)
