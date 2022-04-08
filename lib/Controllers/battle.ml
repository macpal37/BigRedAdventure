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
  | Exit

let combat_mode = ref Commands
let battle_sim = ref Combat.empty_battle
let captured_creature = ref Option.None

(*****************************************************************)
(***************     Loading Some Assets     *********************)
(*****************************************************************)

let battle_bot = load_sprite "battle_bot" GUI_Folder 3 ()
let battle_right = load_sprite "battle_top" GUI_Folder 3 ()
let moves_window = load_sprite "moves_window" GUI_Folder 3 ()
let combat_hud = load_sprite "opponent_hud" GUI_Folder 3 ()
let player_hud = load_sprite "player_hud" GUI_Folder 3 ()
let battle_bg1 = load_sprite "battle-bg1" GUI_Folder 3 ()
let level_up_screen = load_sprite "level_up" GUI_Folder 3 ()

(* WIll be improved next sprint *)

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

let draw_health_bar_combat max before after player animate () =
  let hwidth = 210 in
  let hheight = 6 in
  let xh, yh =
    if player then (width - hwidth - 31 - 10, 296) else (130, 615)
  in

  draw_health_bar max before after xh yh hwidth hheight player animate
    ()

let draw_combat_hud
    sprite
    name
    level
    player
    (max, before, after)
    still
    () =
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
  if still then draw_health_bar_combat max before before player false ()
  else draw_health_bar_combat max before after player true ();
  set_font_size 40 ()

let draw_combat_commands () =
  set_font_size 50 ();
  let x, y = (465, 120) in
  set_color text_color;
  clear_text battle_right ();
  draw_string_colored x y 2 50 "FIGHT" white text_color ();
  draw_string_colored x (y - 75) 2 50 "BAG" white text_color ();
  draw_string_colored (x + 175) y 2 50 "PARTY" white text_color ();
  draw_string_colored (x + 175) (y - 75) 2 50 "RUN" white text_color ();
  draw_string_colored
    (x - 35 + (175 * commands_position.x))
    (y - (75 * commands_position.y))
    2 50 ">" white text_color ();
  (draw_text_string_pos 35 132 40 14
     ("What will "
     ^ get_nickname battle_sim.contents.player_battler.creature)
     white)
    ()

(*****************************************************************)
(***************     Test Demo Cmmands     *********************)
(*****************************************************************)
let refresh_hud () =
  let player, opponent =
    ( battle_sim.contents.player_battler.creature,
      battle_sim.contents.enemy_battler.creature )
  in

  (* Ui.add_first_gameplay *)
  draw_combat_hud combat_hud (get_nickname opponent)
    (get_level opponent) false
    ( (get_stats opponent).max_hp,
      get_current_hp opponent,
      get_current_hp opponent )
    true ();

  (* Ui.add_first_gameplay *)
  draw_combat_hud player_hud (get_nickname player) (get_level player)
    true
    ( (get_stats player).max_hp,
      get_current_hp player,
      get_current_hp player )
    true ()

let update_health creature before () =
  let curr, max =
    (get_current_hp creature, (get_stats creature).max_hp)
  in
  draw_health_bar_combat max before curr false true ()

let draw_creature_exp creature render () =
  let curr_exp, min_exp, max_exp = get_exp creature in
  let curr_exp = if curr_exp >= max_exp then min_exp else curr_exp in
  if render then
    Ui.add_first_foreground
      (draw_exp_bar_combat (max_exp - min_exp) (curr_exp - min_exp)
         (curr_exp - min_exp))
  else
    (draw_exp_bar_combat (max_exp - min_exp) (curr_exp - min_exp)
       (curr_exp - min_exp))
      ()

let draw_level_up creature () =
  let old_stats = get_stats creature in
  level_up creature ();
  let new_stats = get_stats creature in
  let x, y, dif, x2 = (width - 300 + 30, 200 + 250, 35, width - 140) in
  auto_synchronize false;
  draw_sprite level_up_screen (width - 300) 210 ();
  draw_string_colored x y 1 40
    ("Level " ^ string_of_int (get_level creature))
    white text_color ();
  let stat_lst =
    [
      Creature.HP;
      Creature.Attack;
      Defense;
      Sp_Attack;
      Sp_Defense;
      Speed;
    ]
  in
  for i = 0 to 5 do
    let s = List.nth stat_lst i in

    draw_string_colored x
      (y - (dif * (i + 1)))
      1 30
      (string_of_stat_short s)
      white text_color ();

    draw_string_colored x2
      (y - (dif * (i + 1)))
      1 30
      (string_of_int (get_stat2 old_stats s))
      white text_color ();
    draw_string_colored (x2 + 80)
      (y - (dif * (i + 1)))
      1 30
      ("+"
      ^ string_of_int (get_stat2 new_stats s - get_stat2 old_stats s))
      white text_color ()
  done;
  auto_synchronize true;
  wait (-1) ();
  auto_synchronize false;
  draw_sprite level_up_screen (width - 300) 210 ();
  draw_string_colored x y 1 40
    ("Level " ^ string_of_int (get_level creature))
    white text_color ();
  for i = 0 to 5 do
    let s = List.nth stat_lst i in

    draw_string_colored x
      (y - (dif * (i + 1)))
      1 30
      (string_of_stat_short s)
      white text_color ();

    draw_string_colored x2
      (y - (dif * (i + 1)))
      1 30
      (string_of_int (get_stat2 new_stats s))
      white text_color ()
  done;
  auto_synchronize true;
  wait (-1) ();
  auto_synchronize false;
  Combat.refresh_battle.contents
    (get_current_hp battle_sim.contents.player_battler.creature)
    (get_current_hp battle_sim.contents.enemy_battler.creature)
    0 ();

  auto_synchronize true;
  auto_synchronize false

let handle_exp player_creature enemy_creature () =
  Ui.update_all ();
  let exp_gain =
    get_exp_gain enemy_creature
    / List.length battle_sim.contents.creatures_switched
  in

  let exp_event target player =
    let curr_level = get_level target in
    let exp_list = add_exp target exp_gain in
    (* Ui.add_last_foreground (set_sticky_text true); *)
    Ui.add_last_foreground
      (draw_text
         (get_nickname target ^ " gained " ^ string_of_int exp_gain
        ^ " EXP. Points!")
         40 true);
    let rec level_up_handler level = function
      | [] -> ()
      | h :: t ->
          let max, bef, aft, lvl = h in

          if player then
            Ui.add_last_foreground (draw_exp_bar_combat max bef aft);
          if level <> lvl then begin
            (* Ui.add_last_foreground (level_up target); *)
            Ui.add_last_foreground
              (draw_text
                 (get_nickname target ^ " grew to level "
                ^ string_of_int lvl ^ "!")
                 40 true);
            Ui.add_last_foreground (draw_level_up target)
          end;
          level_up_handler lvl t
    in

    level_up_handler curr_level (List.rev exp_list)
  in

  exp_event player_creature true;
  let rec rest_exp_events = function
    | [] -> ()
    | h :: t ->
        if h <> player_creature then exp_event h false else ();

        rest_exp_events t
  in
  rest_exp_events battle_sim.contents.creatures_switched

let rec faint base c sprite player () =
  let sprite_width, sprite_height = get_dimension sprite in
  let xx, yy =
    if player then (50, 166)
    else (width - 50 - sprite_width, height - 50 - sprite_height)
  in
  if c = base - 2 then begin
    auto_synchronize false;
    Combat.refresh_battle.contents
      (get_current_hp battle_sim.contents.player_battler.creature)
      (get_current_hp battle_sim.contents.enemy_battler.creature)
      (if player then 1 else 0)
      ();
    auto_synchronize true
  end
  else begin
    auto_synchronize false;
    Combat.refresh_battle.contents
      (get_current_hp battle_sim.contents.player_battler.creature)
      (get_current_hp battle_sim.contents.enemy_battler.creature)
      (if player then 1 else 0)
      ();
    draw_sprite_crop sprite xx
      (yy - (sprite_height - (sprite_height / c)))
      (0, sprite_width)
      (sprite_height - (sprite_height / c), sprite_height)
      ();
    clear_text battle_bot ();
    auto_synchronize true;
    Input.sleep 0.075 ();
    faint base (c + 1) sprite player ()
  end;
  set_color text_color;
  set_synced_mode false

let animate_faint creature player () =
  set_synced_mode false;
  faint 20 1 creature player ()

let handle_combat move =
  Ui.update_all ();
  set_synced_mode false;
  auto_synchronize false;
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
      (draw_health_bar_combat p_maxhp player_b player_a1 true true);
    Ui.add_last_gameplay
      (draw_health_bar_combat e_maxhp enemy_b enemy_a1 false true);
    Ui.update_all ();
    (***=============Second Half =============***)
    Combat.battle_sim_sh battle_sim.contents;
    (* if battle_sim.contents.battle_status <> Combat.Victory *)
    let player_a2, enemy_a2 =
      (get_current_hp player, get_current_hp enemy)
    in
    if battle_sim.contents.battle_status <> Combat.Victory then begin
      Ui.add_last_gameplay
        (draw_health_bar_combat p_maxhp player_a1 player_a2 true true);
      Ui.add_last_foreground
        (draw_health_bar_combat e_maxhp enemy_a1 enemy_a2 false true)
    end;
    Ui.update_all ();
    (***============= Resolution =============***)
    if battle_sim.contents.enemy_battler.active = false then (
      combat_mode.contents <- End_Battle;

      (* Ui.add_last_gameplay (set_sticky_text true); *)
      Ui.add_last_gameplay
        (animate_faint (get_front_sprite enemy) false);
      battle_sim.contents.enemy_battler.active <- false;
      Ui.add_last_gameplay (Input.sleep 0.5);

      handle_exp player enemy ();
      Ui.update_all ());
    if battle_sim.contents.player_battler.active = false then
      Ui.add_last_gameplay (animate_faint (get_back_sprite player) true)
  end

let pokeball_spritesheet =
  Spritesheet.init_spritesheet
    "assets/item_sprites/pokeball_capture.png" 64 64 3

let handle_item item () =
  match Item.get_type item with
  | Item.Misc -> print_endline "What?"
  | Item.Medicine -> ()
  | Item.Ball ->
      let ball_type = Item.get_id item mod 50 in
      let modifier =
        match ball_type with
        | 1 -> 1.5
        | 2 -> 2.0
        | _ -> 1.0
      in

      let catch_results = Combat.capture battle_sim.contents modifier in
      Ui.update_all ();
      Ui.add_last_gameplay
        (Animation.capture_animation pokeball_spritesheet
           (get_front_sprite battle_sim.contents.enemy_battler.creature)
           catch_results ball_type
           (Combat.refresh_battle.contents
              (get_current_hp
                 battle_sim.contents.player_battler.creature)
              (get_current_hp battle_sim.contents.enemy_battler.creature)));
      Ui.update_all ();
      if battle_sim.contents.battle_status = Catch then begin
        print_endline "Success";
        Ui.add_last_foreground
          (draw_text
             ("You captured "
             ^ get_nickname battle_sim.contents.enemy_battler.creature
             ^ "!")
             40 true);
        handle_exp battle_sim.contents.player_battler.creature
          battle_sim.contents.enemy_battler.creature ();
        battle_sim.contents.enemy_battler.active <- false;
        captured_creature.contents <-
          Some battle_sim.contents.enemy_battler.creature;

        (* Player.add_creature enemy_creature.contents (State.player
           ()); *)
        combat_mode.contents <- End_Battle
      end
      else begin
        Ui.add_last_foreground (draw_text "Aw... So close!" 40 true);
        print_endline "Failure"
      end;
      Ui.update_all ()
  | Item.Key -> ()

let refresh_battle () =
  Ui.add_last_background (draw_sprite battle_bg1 0 0);
  if battle_sim.contents.enemy_battler.active = true then
    Ui.add_first_gameplay
      (draw_creature
         (get_front_sprite battle_sim.contents.enemy_battler.creature)
         false);
  if battle_sim.contents.player_battler.active = true then
    Ui.add_first_gameplay
      (draw_creature
         (get_back_sprite battle_sim.contents.player_battler.creature)
         true);

  Ui.add_first_gameplay refresh_hud;
  Ui.add_first_foreground
    (draw_creature_exp battle_sim.contents.player_battler.creature false);
  Ui.add_first_foreground draw_combat_commands;
  auto_synchronize false

let clear_battle p_hp e_hp state () =
  (draw_sprite battle_bg1 0 0) ();
  let player, opponent =
    ( battle_sim.contents.player_battler.creature,
      battle_sim.contents.enemy_battler.creature )
  in

  (* Ui.add_first_gameplay *)
  draw_combat_hud combat_hud (get_nickname opponent)
    (get_level opponent) false
    ((get_stats opponent).max_hp, e_hp, e_hp)
    true ();

  (* Ui.add_first_gameplay *)
  draw_combat_hud player_hud (get_nickname player) (get_level player)
    true
    ((get_stats player).max_hp, p_hp, p_hp)
    true ();
  (draw_creature_exp battle_sim.contents.player_battler.creature false)
    ();
  (match state with
  | 0 ->
      if battle_sim.contents.player_battler.active then
        draw_creature
          (get_back_sprite battle_sim.contents.player_battler.creature)
          true ()
  | 1 ->
      if battle_sim.contents.enemy_battler.active then
        draw_creature
          (get_front_sprite battle_sim.contents.enemy_battler.creature)
          false ()
  | 2 ->
      if battle_sim.contents.player_battler.active then
        draw_creature
          (get_back_sprite battle_sim.contents.player_battler.creature)
          true ();
      if battle_sim.contents.enemy_battler.active then
        draw_creature
          (get_front_sprite battle_sim.contents.enemy_battler.creature)
          false ()
  | _ -> ());

  (clear_text battle_bot) ()

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
  (if key = 't' then
   let creature = battle_sim.contents.player_battler.creature in

   draw_level_up creature ());
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
          key = 'e' && commands_position.x = 0
          && commands_position.y = 1
        then begin
          Inventory_menu.init ();

          match Inventory_menu.selected_item.contents with
          | Some i ->
              handle_item i ();
              Ui.update_all ();
              Ui.add_first_background
                (Combat.refresh_battle.contents
                   (get_current_hp
                      battle_sim.contents.player_battler.creature)
                   (get_current_hp
                      battle_sim.contents.enemy_battler.creature)
                   2);

              handle_combat Creature.empty_move
          | None ->
              refresh_battle ();
              Ui.update_all ()
        end
        else if
          key = 'e' && commands_position.x = 1
          && commands_position.y = 0
        then begin
          Party_menu.init true ();
          match Combat.switching_pending.contents with
          | Some c ->
              (* Ui.add_first_foreground (draw_text "Come back!" 40
                 true); *)
              Ui.add_first_foreground (clear_text battle_bot);

              Combat.switch_player battle_sim.contents c
                (Player.party (State.player ()));
              refresh_battle ();
              Ui.update_all ();
              combat_mode.contents <- Attack;
              Combat.switching_pending.contents <- None;
              handle_combat empty_move;
              commands_position.x <- 0;
              commands_position.y <- 0
          | Option.None -> refresh_battle ()
        end
        else if
          key = 'e' && commands_position.x = 1
          && commands_position.y = 1
        then begin
          Combat.run_away battle_sim.contents;
          if battle_sim.contents.battle_status = Combat.Flee then begin
            Ui.add_first_foreground (draw_text "You ran away!" 40 true);

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
          Ui.add_first_foreground draw_combat_commands;
          combat_mode.contents <- Commands
        end
    | Attack ->
        Ui.add_first_gameplay (clear_text battle_right);
        if battle_sim.contents.enemy_battler.active then
          combat_mode.contents <- Commands
        else combat_mode.contents <- End_Battle
    | End_Battle ->
        let new_party =
          match captured_creature.contents with
          | Some c -> battle_sim.contents.player_creatures @ [ c ]
          | None -> battle_sim.contents.player_creatures
        in
        Player.set_party new_party (State.player ());
        combat_mode.contents <- Exit;
        wait (175000 * 2) ()
    | Exit -> ()
  in

  action;

  Ui.update_all ();
  Unix.sleepf 0.016;
  if combat_mode.contents <> Exit then run_tick ()

let start_battle c =
  (* enemy_active.contents <- true; *)
  captured_creature.contents <- None;
  Combat.refresh_battle.contents <- clear_battle;
  Ui.add_last_background clear_screen;
  combat_mode.contents <- Commands;

  Ui.add_last_background (clear_text battle_right);
  (* ========= Start the Battle ========= *)
  battle_sim.contents <-
    Combat.wild_init (Player.party (State.player ())) [ c ];
  (* ========= Draw the Battle ========= *)
  refresh_battle ();
  Ui.update_all ();
  run_tick ()
