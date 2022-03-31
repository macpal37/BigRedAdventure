open Draw
open Graphics
open Creature
open Animation

let text_box_height = 212
(* let combat_button = ref 0 *)

let moves_position = Util.new_point ()
let commands_position = Util.new_point ()

type combat_m =
  | Commands
  | Moves
  | Attack
  | End_Battle

let combat_mode = ref Commands
let battle_sim = ref Combat.empty_battle

(*****************************************************************)
(***************     Loading Some Assets     *********************)
(*****************************************************************)

let battle_bot = load_sprite "battle_bot" GUI_Folder 3 ()
let battle_right = load_sprite "battle_top" GUI_Folder 3 ()
let moves_window = load_sprite "moves_window" GUI_Folder 3 ()
let combat_hud = load_sprite "opponent_hud" GUI_Folder 3 ()
let player_hud = load_sprite "player_hud2" GUI_Folder 3 ()
let battle_bg1 = load_sprite "battle-bg1" GUI_Folder 3 ()

(* WIll be improved next sprint *)
let enemy_active = ref true
let enemy_creature = ref (create_creature "stregoom" 1)

(*****************************************************************)
(***************     Combat Drawing Commands     *********************)
(*****************************************************************)

let draw_moves creature () =
  clear_text moves_window ();
  let moves = get_moves creature in
  let size = List.length moves in
  let box_w, box_h = (370, 92) in
  let box_x, box_y = (28, 200 - box_h) in
  let text_x, text_y = (38, (4 * text_box_height / 5) - 12) in
  let text_xdif, text_ydif = (376, 95) in
  set_font_size 30 ();
  set_line_width 6;
  let rec draw_all_moves i j length =
    if length <= 0 then ()
    else
      let move = List.nth moves (i + (j * 2)) in
      let x, y = (text_x + (text_xdif * i), text_y - (text_ydif * j)) in
      draw_string_colored x y 2 40 move.move_name
        (get_color_from_etype move.etype)
        text_color ();

      draw_string_colored x (y - 40) 1 30
        (string_of_etype move.etype)
        (get_color_from_etype move.etype)
        text_color ();

      draw_string_colored (x + 110) (y - 40) 1 30
        ("PP:"
        ^ string_of_int move.curr_pp
        ^ "/"
        ^ string_of_int move.max_pp)
        white text_color ();

      if i = 1 then draw_all_moves 0 (j + 1) (length - 1)
      else draw_all_moves (i + 1) j (length - 1)
  in
  draw_all_moves 0 0 size;

  (* draw_rect (box_x + (box_w * moves_position.x)) (box_y - (box_h *
     moves_position.y)) box_w box_h; *)
  set_color (rgb 255 0 0);
  draw_rect
    (box_x + (box_w * moves_position.x))
    (box_y - (box_h * moves_position.y))
    box_w box_h;
  set_color text_color

let draw_exp_bar_combat max before after () =
  let hwidth = 250 in
  let hheight = 6 in

  let xh, yh = (width - hwidth - 30, 240) in
  draw_exp_bar max before after xh yh hwidth hheight ()

let draw_health_bar_combat max before after player () =
  let hwidth = 210 in
  let hheight = 6 in
  let xh, yh =
    if player then (width - hwidth - 31 - 10, 296) else (130, 615)
  in

  draw_health_bar max before after xh yh hwidth hheight player ()

let draw_combat_hud sprite name level player (max, before, after) () =
  let sprite_width, sprite_height = get_dimension sprite in
  if player then begin
    set_font_size 30 ();
    draw_sprite sprite
      (width - sprite_width - 14)
      (360 - sprite_height) ();
    draw_string_colored (width - 320) 316 2 30
      (String.uppercase_ascii name)
      white text_color ();

    draw_string_colored (width - 100) 316 2 30
      ("Lv" ^ string_of_int level)
      white text_color ()
  end
  else begin
    set_font_size 30 ();
    draw_sprite sprite 42 (height - 45 - sprite_height) ();
    draw_string_colored 60 (height - 85) 2 30
      (String.uppercase_ascii name)
      white text_color ();

    draw_string_colored 280 (height - 85) 2 30
      ("Lv" ^ string_of_int level)
      white text_color ()
  end;

  draw_health_bar_combat max before after player ();
  set_font_size 40 ()

let draw_combat_commands () =
  set_font_size 50 ();
  let x, y = (475, 120) in
  set_color text_color;
  clear_text battle_right ();
  draw_string_colored x y 2 50 "FIGHT" white text_color ();
  draw_string_colored x (y - 75) 2 50 "PARTY" white text_color ();
  draw_string_colored (x + 200) y 2 50 "BAG" white text_color ();
  draw_string_colored (x + 200) (y - 75) 2 50 "BAG" white text_color ();
  draw_string_colored
    (x - 40 + (200 * commands_position.x))
    (y - (75 * commands_position.y))
    2 50 ">" white text_color ();
  set_font_size 40 ()

let start_combat_hud () =
  Ui.add_first_foreground (clear_text battle_right);
  Ui.add_first_foreground draw_combat_commands;
  Ui.add_first_foreground
    (draw_text_string_pos 35 132 40 14 "What will" white)

let rec faint base c sprite player () =
  set_synced_mode true;
  let sprite_width, sprite_height = get_dimension sprite in
  let xx, yy =
    if player then (50, 166)
    else (width - 50 - sprite_width, height - 50 - sprite_height)
  in
  if c = base - 2 then begin
    set_erase_mode true ();
    draw_creature sprite player ();
    set_erase_mode false ()
  end
  else begin
    draw_sprite_crop sprite xx
      (yy - (sprite_height - (sprite_height / c)))
      (0, sprite_width)
      (sprite_height - (sprite_height / c), sprite_height)
      ();

    clear_text battle_bot ();
    Input.sleep 0.075 ();
    set_erase_mode true ();
    draw_sprite_crop sprite xx
      (yy - (sprite_height - (sprite_height / c)))
      (0, sprite_width)
      (sprite_height - (sprite_height / c), sprite_height)
      ();
    set_erase_mode false ();

    set_color blue;

    faint base (c + 1) sprite player ()
  end;
  set_color text_color;
  set_synced_mode false

let animate_faint creature player () = faint 20 1 creature player ()

(*****************************************************************)
(***************     Test Demo Cmmands     *********************)
(*****************************************************************)
let refresh_hud () =
  let player, opponent =
    ( battle_sim.contents.player_battler.creature,
      battle_sim.contents.enemy_battler.creature )
  in

  Ui.add_first_gameplay
    (draw_combat_hud combat_hud (get_nickname opponent)
       (get_level opponent) false
       ( (get_stats opponent).max_hp,
         get_current_hp opponent,
         get_current_hp opponent ));

  Ui.add_first_gameplay
    (draw_combat_hud player_hud (get_nickname player) (get_level player)
       true
       ( (get_stats player).max_hp,
         get_current_hp player,
         get_current_hp player ))

let update_health creature before () =
  let curr, max =
    (get_current_hp creature, (get_stats creature).max_hp)
  in
  draw_health_bar_combat max before curr false ()

let draw_creature_exp creature added_exp render () =
  add_exp creature added_exp;
  let curr_exp, min_exp, max_exp = get_exp creature in
  if render then
    Ui.add_first_foreground
      (draw_exp_bar_combat (max_exp - min_exp) (curr_exp - min_exp)
         (curr_exp - min_exp))
  else
    (draw_exp_bar_combat (max_exp - min_exp) (curr_exp - min_exp)
       (curr_exp - min_exp))
      ()

let handle_exp player_creature enemy_creature () =
  let before_exp, _, _ = get_exp player_creature in
  add_exp player_creature (get_exp_gain enemy_creature);
  let curr_exp, min_exp, max_exp = get_exp player_creature in

  Ui.add_first_foreground
    (draw_exp_bar_combat (max_exp - min_exp) (before_exp - min_exp)
       (curr_exp - min_exp));
  Ui.add_first_foreground (set_sticky_text false);
  Ui.add_first_foreground
    (draw_text
       (get_nickname player_creature
       ^ " gained "
       ^ string_of_int (get_exp_gain player_creature)
       ^ " EXP. Points!")
       40 false);
  Ui.add_first_foreground (set_sticky_text true)

let handle_combat move =
  (* Ui.clear_ui Gameplay; Ui.clear_ui Foreground; *)
  if battle_sim.contents.battle_status = Ongoing then begin
    Combat.turn_builder battle_sim.contents move;
    set_text_char_cap 28;
    let player, enemy =
      ( battle_sim.contents.player_battler.creature,
        battle_sim.contents.enemy_battler.creature )
    in

    let p_maxhp, e_maxhp =
      ((get_stats player).max_hp, (get_stats enemy).max_hp)
    in

    let player_b, enemy_b =
      (get_current_hp player, get_current_hp enemy)
    in
    (***=============First Half =============***)
    Combat.battle_sim_fh battle_sim.contents;
    let player_a1, enemy_a1 =
      (get_current_hp player, get_current_hp enemy)
    in
    Ui.add_last_gameplay
      (draw_health_bar_combat p_maxhp player_b player_a1 true);
    Ui.add_last_gameplay
      (draw_health_bar_combat e_maxhp enemy_b enemy_a1 false);
    Ui.update_all ();
    (***=============Second Half =============***)
    Combat.battle_sim_sh battle_sim.contents;
    (* if battle_sim.contents.battle_status <> Combat.Victory *)
    let player_a2, enemy_a2 =
      (get_current_hp player, get_current_hp enemy)
    in
    if battle_sim.contents.battle_status <> Combat.Victory then
      Ui.add_last_gameplay
        (draw_health_bar_combat p_maxhp player_a1 player_a2 true);
    Ui.add_last_foreground
      (draw_health_bar_combat e_maxhp enemy_a1 enemy_a2 false);
    Ui.update_all ();
    (***============= Resolution =============***)
    if enemy_a2 <= 0 then (
      combat_mode.contents <- End_Battle;
      handle_exp player enemy ();
      Ui.add_first_foreground (set_sticky_text true);
      Ui.add_first_foreground
        (animate_faint (get_front_sprite enemy) false);
      enemy_active.contents <- false;
      Ui.add_first_foreground (Input.sleep 0.5);
      if player_a2 <= 0 then
        Ui.add_first_foreground
          (animate_faint (get_front_sprite player) true))
  end

let handle_item item () =
  match Item.get_type item with
  | Item.Misc -> print_endline "What?"
  | Item.Medicine -> ()
  | Item.Ball ->
      print_endline "Trying to catch hmmm.";

      Combat.capture battle_sim.contents;

      if battle_sim.contents.battle_status = Catch then begin
        print_endline "Success";
        Ui.add_first_foreground
          (handle_exp battle_sim.contents.player_battler.creature
             battle_sim.contents.enemy_battler.creature);
        Ui.add_first_foreground
          (draw_text
             ("You captured "
             ^ get_nickname enemy_creature.contents
             ^ "!")
             40 true);
        enemy_active.contents <- false;
        Player.add_creature enemy_creature.contents (State.player ());
        combat_mode.contents <- End_Battle
      end
      else begin
        Ui.add_last_foreground (draw_text "Aw... So close!" 40 true);
        print_endline "Failure"
        (* Ui.add_first_foreground (fun () -> Graphics.auto_synchronize
           true) *)
      end
  | Item.Key -> ()

let refresh_battle () =
  Ui.add_last_background (draw_sprite battle_bg1 0 0);
  if enemy_active.contents = true then
    Ui.add_first_gameplay
      (draw_creature
         (get_front_sprite battle_sim.contents.enemy_battler.creature)
         false);
  Ui.add_first_gameplay
    (draw_creature
       (get_back_sprite battle_sim.contents.player_battler.creature)
       true);

  refresh_hud ();
  draw_creature_exp battle_sim.contents.player_battler.creature 0 true
    ();
  start_combat_hud ()

let move_x x () =
  match combat_mode.contents with
  | Commands ->
      if commands_position.x + x >= 0 && commands_position.x + x < 2
      then commands_position.x <- commands_position.x + x
  | Moves ->
      if moves_position.x + x >= 0 && moves_position.x + x < 2 then
        moves_position.x <- moves_position.x + x
  | _ -> ()

let move_y y () =
  match combat_mode.contents with
  | Commands ->
      if commands_position.y + y >= 0 && commands_position.y + y < 2
      then commands_position.y <- commands_position.y + y
  | Moves ->
      if moves_position.y + y >= 0 && moves_position.y + y < 2 then
        moves_position.y <- moves_position.y + y
  | _ -> ()

let rec run_tick () =
  Input.poll ();
  let key =
    match Input.key_option () with
    | Some c -> c
    | None -> '#'
  in

  if key = 'd' then move_x 1 ();
  if key = 'a' then move_x (-1) ();
  if key = 'w' then move_y (-1) ();
  if key = 's' then move_y 1 ();
  let action =
    match combat_mode.contents with
    | Commands ->
        if key <> 'e' || key = 'a' || key = 'w' || key = 's' then
          Ui.add_first_foreground draw_combat_commands;
        if
          key = 'e' && commands_position.x = 0
          && commands_position.y = 0
          && battle_sim.contents.battle_status <> Combat.Victory
        then begin
          combat_mode.contents <- Moves;
          Ui.add_first_gameplay (clear_text moves_window);
          Ui.add_first_foreground
            (draw_moves battle_sim.contents.player_battler.creature)
        end
        else if
          key = 'e' && commands_position.x = 1
          && commands_position.y = 0
        then begin
          Inventory_menu.init ();

          match Inventory_menu.selected_item.contents with
          | Some i ->
              handle_item i ();
              refresh_battle ();
              Ui.update_all ();
              handle_combat Creature.empty_move
          | None ->
              refresh_battle ();
              Ui.update_all ()
        end
        else if
          key = 'e' && commands_position.x = 0
          && commands_position.y = 1
        then begin
          Party_menu.init true ();
          match Combat.switching_pending.contents with
          | Some c ->
              Ui.add_first_foreground (draw_text "Come back!" 40 true);
              Ui.add_first_foreground (clear_text battle_bot);

              Combat.switch_player battle_sim.contents c
                (Player.party (State.player ()));
              refresh_battle ();
              Ui.update_all ();
              combat_mode.contents <- Attack;
              Combat.switching_pending.contents <- None;
              handle_combat empty_move
          | Option.None -> refresh_battle ()
        end
        else if
          key = 'e' && commands_position.x = 1
          && commands_position.y = 1
        then begin
          Combat.run_away battle_sim.contents;
          if battle_sim.contents.battle_status = Combat.Flee then begin
            Ui.add_first_foreground (draw_text "You ran away!" 40 true);
            (* Ui.add_first_foreground (fun () ->
               Graphics.auto_synchronize true); *)
            combat_mode.contents <- End_Battle
          end
          else begin
            Ui.add_first_foreground
              (draw_text "You could not run away!" 40 true);
            Ui.update_all ();

            handle_combat Creature.empty_move
          end
        end
    | Moves ->
        if key <> 'e' || key = 'a' || key = 'w' || key = 's' then
          Ui.add_first_foreground
            (draw_moves battle_sim.contents.player_battler.creature);

        if key = 'e' then begin
          Ui.add_first_foreground (clear_text battle_bot);
          let move =
            List.nth
              (get_moves battle_sim.contents.player_battler.creature)
              (moves_position.x + (2 * moves_position.y))
          in
          Ui.update_all ();
          combat_mode.contents <- Attack;
          handle_combat move
        end;
        if key = 'q' then begin
          Ui.clear_ui Ui.Foreground;
          Ui.add_first_foreground start_combat_hud;
          combat_mode.contents <- Commands;
          Ui.add_last_background (clear_text battle_right)
        end
    | Attack ->
        Ui.add_first_gameplay (clear_text battle_right);
        if enemy_active.contents then combat_mode.contents <- Commands
        else combat_mode.contents <- End_Battle
    | End_Battle -> ()
  in

  action;

  Ui.update_all ();
  Unix.sleepf 0.016;
  if combat_mode.contents <> End_Battle then run_tick ()

let start_battle c =
  enemy_active.contents <- true;
  enemy_creature.contents <- c;
  Ui.add_last_background clear_screen;
  combat_mode.contents <- Commands;

  Ui.add_last_background (clear_text battle_right);
  (* ========= Start the Battle ========= *)
  battle_sim.contents <-
    Combat.wild_init
      (Player.party (State.player ()))
      [ enemy_creature.contents ];
  (* ========= Draw the Battle ========= *)
  refresh_battle ();
  Ui.update_all ();
  run_tick ()
