open Graphics
open CreatureGame
open CreatureGame.Draw
open CreatureGame.Creature

let init = Random.self_init ()
let white = rgb 55 255 255
let blue = rgb 200 200 240
let black = rgb 0 0 0
let red = rgb 200 50 50
let width = 800
let height = 720
let text_box_height = 212

type mode =
  | Adventure
  | Combat
  | Menu

let game = ref Adventure

let open_window =
  open_graph (" " ^ string_of_int width ^ "x" ^ string_of_int height);
  set_window_title "PokemonGame"

(* no way of setting background color; resizing shows white *)
let clear_window color =
  let fg = foreground in
  set_color color;
  fill_rect 0 0 (size_x ()) (size_y ());
  set_color fg

(* no function for converting color back to rgb in Graphics *)
let color_to_rgb color =
  let r = (color land 0xFF0000) asr 0x10
  and g = (color land 0x00FF00) asr 0x8
  and b = color land 0x0000FF in
  (r, g, b)

let get_move () : char option =
  if Graphics.key_pressed () then Some (Graphics.read_key ()) else None

let rayquaza = load_creature "rayquaza_front" ()
let clefairy_back = load_creature "clefairy_back" ()
let battle_bot_right = load_sprite "other_sprites/battle_bot_right" ()
let battle_bot_left = load_sprite "other_sprites/battle_bot_left" ()
let battle_right = load_sprite "other_sprites/battle_top" ()
let moves_window = load_sprite "other_sprites/moves_window" ()
let move_info1 = load_sprite "other_sprites/move_info1" ()
let combat_hud = load_sprite "other_sprites/opponent_hud" ()
let player_hud = load_sprite "other_sprites/player_hud" ()
let clefairy = create_creature "clefairy" 50

let clear () =
  set_color blue;
  fill_rect 0 0 width height;
  set_color text_color;
  moveto 100 200

let start_combat_hud () =
  set_text_char_cap 14;
  set_text_bg battle_bot_left battle_right;
  clear_text true ();
  set_text_bg battle_bot_left empty_sprite;
  set_sticky_text true;
  draw_text "What will     Clefairy do?   " ();
  set_text_bg empty_sprite battle_right;
  set_sticky_text false

(* let fit_words x y words = *)

let draw_moves creature c_b c_a re_draw () =
  let moves = get_moves creature in
  let size = List.length moves in
  if size > 0 then begin
    set_font_size 30 ();
    moveto 30 (2 * text_box_height / 3);
    draw_string (List.nth moves 0).move_name
  end;
  if size > 1 then begin
    set_font_size 30 ();
    moveto (30 + 278) (2 * text_box_height / 3);
    draw_string (List.nth moves 1).move_name
  end;
  if size > 2 then begin
    set_font_size 30 ();
    moveto 30 ((2 * text_box_height / 3) + 15 - 95);
    draw_string (List.nth moves 2).move_name
  end;
  if size > 3 then begin
    set_font_size 30 ();
    moveto (30 + 278) ((2 * text_box_height / 3) + 15 - 95);
    draw_string (List.nth moves 3).move_name
  end;
  if re_draw then begin
    set_line_width 5;
    set_color (rgb 255 255 255);
    draw_rect
      (27 - 5 + ((283 - 5) * if c_b = 1 || c_b = 3 then 1 else 0))
      ((2 * text_box_height / 3) - 30 - (80 * if c_b >= 2 then 1 else 0))
      275 90;
    set_color (rgb 0 0 0);
    draw_rect
      (27 - 5 + ((283 - 5) * if c_a = 1 || c_a = 3 then 1 else 0))
      ((2 * text_box_height / 3) - 30 - (80 * if c_a >= 2 then 1 else 0))
      275 90
  end

let start_up () =
  set_text_char_cap 28;
  set_text_bg battle_bot_left battle_bot_right;
  clear_text true ();
  set_current_hp clefairy (get_stats clefairy).max_hp;
  draw_combat_hud combat_hud "Rayquaza" 100 false (100, 100, 100) ();

  draw_combat_hud player_hud (get_nickname clefairy)
    (get_level clefairy) true
    ( (get_stats clefairy).max_hp,
      get_current_hp clefairy,
      get_current_hp clefairy )
    ();
  draw_creature rayquaza false ();
  draw_creature clefairy_back true ();
  draw_exp_bar 100 10 100 ();
  start_combat_hud ()
(* draw_health_bar 24 22 6 false () *)

(* draw_health_bar 356 356 0 false (); Unix.sleepf 1.5; animate_faint
   rayquaza ();

   draw_text "It was super-effective!" (); draw_text "Clefairy is the
   best!!" (); set_text_char_cap 14; set_text_bg battle_bot_left
   battle_right; set_sticky_text true; draw_text "What will Clefairy
   do?" (); set_sticky_text false; set_text_bg empty_sprite
   battle_right *)

let update_health creature before () =
  let curr, max =
    (get_current_hp creature, (get_stats creature).max_hp)
  in
  draw_health_bar max before curr true ()

type combat_m =
  | Commands
  | Moves

let combat_button = ref 0
let combat_mode = ref Commands

(*****************************************************************)
(***************      Game Loop      *****************************)
(*****************************************************************)
let run_game game () =
  match game.contents with
  (*****************************************************************)
  (***************      Adventure      *****************************)
  (*****************************************************************)
  | Adventure ->
      print_endline "Start of Adventure";
      set_text_bg battle_bot_left battle_right;
      clear_text true ();
      game.contents <- Combat
  (*****************************************************************)
  (***************        Combat       *****************************)
  (*****************************************************************)
  | Combat ->
      let b = !combat_button in
      if Input.d () && (b = 0 || b = 2) then
        combat_button.contents <- b + 1;
      if Input.a () && (b = 1 || b = 3) then
        combat_button.contents <- b - 1;
      if Input.w () && (b = 2 || b = 3) then
        combat_button.contents <- b - 2;
      if Input.s () && (b = 0 || b = 1) then
        combat_button.contents <- b + 2;
      let action =
        match combat_mode.contents with
        | Commands ->
            if b != combat_button.contents then
              draw_combat_commands combat_button.contents true ()
            else draw_combat_commands combat_button.contents false ();
            if Input.e () && combat_button.contents = 0 then begin
              set_text_bg moves_window move_info1;
              combat_mode.contents <- Moves;
              clear_text true ()
            end
        | Moves ->
            if b != combat_button.contents then
              draw_moves clefairy b combat_button.contents true ()
            else draw_moves clefairy b combat_button.contents false ();
            if Input.q () then begin
              set_text_bg battle_bot_left battle_right;
              combat_mode.contents <- Commands;
              clear_text true ()
            end
      in
      action
  (*****************************************************************)
  (***************         Menu        *****************************)
  (*****************************************************************)
  | Menu -> game.contents <- Adventure

let rec event_loop wx wy start game =
  (* there's no resize event so polling in required *)
  let _ = wait_next_event [ Poll ]
  and wx' = size_x ()
  and wy' = size_y () in
  if wx' <> wx || wy' <> wy then clear_window blue;

  if start then start_up ();

  let xx = get_move () in
  (match xx with
  | Some 'c' -> start_up ()
  | Some 'f' ->
      let before = get_current_hp clefairy in
      set_current_hp clefairy (before - Random.int 50);
      update_health clefairy before ()
  | Some 'm' ->
      set_text_char_cap 28;
      set_text_bg battle_bot_left battle_bot_right;
      draw_text
        "I can come in five minutes looking at Gardevoir  or Lopunny \
         ee a Serperior, for  instance, I have to think I can \n\
        \ come in five minutes looking at Gardevoir  or Lopunny ee a \
         Serperior, for  instance, I have to think"
        ();
      set_text_bg battle_bot_left battle_right
  | Some 'n' ->
      set_text_char_cap 14;
      set_text_bg battle_bot_left battle_right;
      clear_text true ();
      set_text_bg battle_bot_left empty_sprite;
      set_sticky_text true;
      draw_text "What will     Clefairy do?   " ();
      set_text_bg empty_sprite battle_right;
      set_sticky_text false
  | Some c ->
      Input.key_press c;

      run_game game ()
  | None -> run_game game ());

  Unix.sleepf 0.005;
  event_loop wx' wy' false game

let () =
  open_window;
  moveto 100 200;
  Input.keymap_init [ 'q'; 'e'; 'w'; 'a'; 's'; 'd' ];
  set_text_bg battle_bot_left battle_bot_right;
  set_font "-*-fixed-bold-r-semicondensed--40-*-*-*-*-*-iso8859-1";
  let r, g, b = color_to_rgb background in
  Printf.printf "Background color: %d %d %d\n" r g b;
  try event_loop 0 0 false game
  with Graphic_failure _ -> print_endline "Exiting..."
