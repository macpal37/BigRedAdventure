open Draw
open Graphics
open Creature

(*****************************************************************)
(***************     Loading Some Assets     *********************)
(*****************************************************************)
let rayquaza_sprite = load_creature "rayquaza_front" ()
let clefairy_back = load_creature "clefairy_back" ()
let battle_bot_right = load_sprite "other_sprites/battle_bot_right" ()
let battle_bot_left = load_sprite "other_sprites/battle_bot_left" ()
let battle_right = load_sprite "other_sprites/battle_top" ()
let moves_window = load_sprite "other_sprites/moves_window" ()

(* let move_info1 = load_sprite "other_sprites/move_info1" () *)
let combat_hud = load_sprite "other_sprites/opponent_hud" ()
let player_hud = load_sprite "other_sprites/player_hud" ()

(** Test creatures Sprites **)
let clefairy = create_creature "clefairy" 100

let rayquaza = create_creature "rayquaza" 80
let init1 () = set_text_bg battle_bot_left battle_bot_right

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

(* let battle_sim = ref empty_battle battle_sim.contents<- wild *)
(*****************************************************************)
(***************     Test Demo Cmmands     *********************)
(*****************************************************************)
let start_up () =
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

let adhoc_test1 () =
  set_text_char_cap 14;
  set_text_bg battle_bot_left battle_right;
  clear_text ();
  set_text_bg battle_bot_left empty_sprite;
  set_sticky_text true;
  draw_text "What will     Clefairy do?   " ();
  set_text_bg empty_sprite battle_right;
  set_sticky_text false

let run_combat () =
  let b = !combat_button in
  if Input.d () && (b = 0 || b = 2) then combat_button.contents <- b + 1;
  if Input.a () && (b = 1 || b = 3) then combat_button.contents <- b - 1;
  if Input.w () && (b = 2 || b = 3) then combat_button.contents <- b - 2;
  if Input.s () && (b = 0 || b = 1) then combat_button.contents <- b + 2;
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
    (* | Attack -> () *)
  in
  action

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

let text_box_height = 212

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
    draw_sprite sprite (width - 368 + 30) (456 - 100 - sprite.height) ();
    moveto (width - 320) 316;
    draw_string (String.uppercase_ascii name);
    moveto (width - 100) 316;
    draw_string ("Lv" ^ string_of_int level);
    draw_health_bar max before after player ()
  end
  else begin
    set_font_size 30 ();
    draw_sprite sprite 42 (height - 45 - sprite.height) ();
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
  usync false ();
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
  usync true ();

  set_font_size 40 ()
