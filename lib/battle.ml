open Graphics
open Draw
open Creature

let text_box_height = 212

type mode =
  | Adventure
  | Combat
  | Menu

let game = ref Adventure

(*****************************************************************)
(***************     Loading Some Assets     *********************)
(*****************************************************************)
let rayquaza_sprite = load_creature "rayquaza_front" ()
let clefairy_back = load_creature "clefairy_back" ()
let battle_bot_right = load_sprite "other_sprites/battle_bot_right" ()
let battle_bot_left = load_sprite "other_sprites/battle_bot_left" ()
let battle_right = load_sprite "other_sprites/battle_top" ()
let moves_window = load_sprite "other_sprites/moves_window" ()
let combat_hud = load_sprite "other_sprites/opponent_hud" ()
let player_hud = load_sprite "other_sprites/player_hud" ()
let clefairy = create_creature "clefairy" 100
let rayquaza = create_creature "rayquaza" 80

(*****************************************************************)
(***************     Combat Drawing Commands     *********************)
(*****************************************************************)
let start_combat_hud () =
  set_text_char_cap 14;
  set_font_size 40 ();
  set_text_bg battle_bot_left battle_right;
  clear_text ();
  set_text_bg battle_bot_left empty_sprite;
  set_sticky_text true;
  draw_text "What will     Clefairy do?   " ();
  set_text_bg empty_sprite battle_right;
  set_sticky_text false

let get_color_from_etype etype =
  match etype with
  | Normal -> rgb 166 166 166
  | Fire -> rgb 235 47 0
  | Water -> rgb 13 150 255
  | Grass -> rgb 0 168 3
  | Fairy -> rgb 255 122 244
  | _ -> rgb 0 0 0

let draw_moves creature c_b c_a () =
  let moves = get_moves creature in
  let size = List.length moves in
  let box_w, box_h = (378, 92) in
  let box_x, box_y = (20, 200 - box_h) in
  let text_x, text_y = (30, (4 * text_box_height / 5) - 6) in
  let text_xdif, text_ydif = (376, 95) in
  set_font_size 30 ();
  set_line_width 6;
  let rec draw_all_moves i j length =
    if length <= 0 then ()
    else
      let move = List.nth moves (i + (j * 2)) in
      let x, y = (text_x + (text_xdif * i), text_y - (text_ydif * j)) in
      draw_string_colored x y 2 move.move_name
        (get_color_from_etype move.etype)
        ();
      moveto x (y - 40);
      set_color (get_color_from_etype move.etype);
      draw_string (string_of_etype move.etype);
      set_color text_color;
      moveto (x + 120) (y - 40);
      draw_string
        ("PP:"
        ^ string_of_int move.curr_pp
        ^ "/"
        ^ string_of_int move.max_pp);

      if i = 1 then draw_all_moves 0 (j + 1) (length - 1)
      else draw_all_moves (i + 1) j (length - 1)
  in
  draw_all_moves 0 0 size;

  draw_rect
    (box_x + (box_w * if c_b = 1 || c_b = 3 then 1 else 0))
    (box_y - (box_h * if c_b >= 2 then 1 else 0))
    box_w box_h;
  set_color (rgb 255 0 0);
  draw_rect
    (box_x + (box_w * if c_a = 1 || c_a = 3 then 1 else 0))
    (box_y - (box_h * if c_a >= 2 then 1 else 0))
    box_w box_h;
  set_color text_color

let update_health creature before () =
  let curr, max =
    (get_current_hp creature, (get_stats creature).max_hp)
  in
  draw_health_bar max before curr false ()

type combat_m =
  | Commands
  | Moves

let combat_button = ref 0
let combat_mode = ref Commands

(*****************************************************************)
(***************     Test Demo Cmmands     *********************)
(*****************************************************************)
let start_up () =
  Input.keymap_init [ 'q'; 'e'; 'w'; 'a'; 's'; 'd' ];
  set_text_bg battle_bot_left battle_bot_right;
  combat_mode.contents <- Commands;
  set_text_char_cap 28;
  set_text_bg battle_bot_left battle_bot_right;
  clear_text ();
  set_current_hp clefairy (get_stats clefairy).max_hp;
  draw_combat_hud combat_hud "Rayquaza" 80 false
    ( (get_stats rayquaza).max_hp,
      get_current_hp rayquaza,
      get_current_hp rayquaza )
    ();

  draw_combat_hud player_hud (get_nickname clefairy)
    (get_level clefairy) true
    ( (get_stats clefairy).max_hp,
      get_current_hp clefairy,
      get_current_hp clefairy )
    ();
  draw_creature rayquaza_sprite false ();
  draw_creature clefairy_back true ();

  draw_exp_bar 100 100 100 ();

  (* draw_health_bar (get_stats rayquaza).max_hp (get_stats
     rayquaza).max_hp 0 false (); Unix.sleepf 1.5; animate_faint
     rayquaza_sprite (); *)
  start_combat_hud ()

(*****************************************************************)
(***************      Game Run      *****************************)
(*****************************************************************)
let run_game game () =
  match game.contents with
  (*****************************************************************)
  (***************      Adventure      *****************************)
  (*****************************************************************)
  | Adventure ->
      print_endline "Start of Adventure";
      set_text_bg battle_bot_left battle_right;
      clear_text ();
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
              set_text_bg moves_window empty_sprite;
              combat_mode.contents <- Moves;
              clear_text ();
              draw_moves clefairy b combat_button.contents ()
            end
        | Moves ->
            if b != combat_button.contents then
              draw_moves clefairy b combat_button.contents ();

            if Input.q () then begin
              start_combat_hud ();
              combat_mode.contents <- Commands;
              clear_text ()
            end
      in
      action
  (*************** Menu *****************************)
  | Menu -> game.contents <- Adventure

(*****************************************************************)
(***************        Game Loop        *****************************)
(*****************************************************************)

let yeet = ref true

let run_tick c =
  if !yeet then (
    start_up ();
    yeet := false;
    ())
  else ();
  match c with
  | Some 'g' ->
      damage_render rayquaza_sprite false ();
      damage_render clefairy_back true ()
  | Some 't' ->
      draw_creature rayquaza_sprite false ();
      wait ();
      animate_faint rayquaza_sprite ();
      Unix.sleepf 1.0;
      draw_creature rayquaza_sprite false ()
  | Some 'c' -> start_up ()
  | Some 'f' ->
      let before = get_current_hp clefairy in
      set_current_hp clefairy (before - Random.int 50);
      update_health clefairy before ();
      if get_current_hp clefairy <= 0 then begin
        Unix.sleepf 1.0;
        animate_faint rayquaza_sprite ()
      end
  | Some 'n' ->
      set_text_char_cap 14;
      set_text_bg battle_bot_left battle_right;
      clear_text ();
      set_text_bg battle_bot_left empty_sprite;
      set_sticky_text true;
      draw_text "What will     Clefairy do?   " ();
      set_text_bg empty_sprite battle_right;
      set_sticky_text false
  | Some '1' -> game := Menu
  | Some c ->
      Input.key_press c;

      run_game game ()
  | None -> run_game game ()
(*dummy function*)
