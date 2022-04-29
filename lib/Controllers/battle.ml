open Draw
open Creature
open Animation
open Util
open DrawText
open Sdlkeycode

let text_box_height = 212
let moves_position = new_point ()
let commands_position = new_point ()

type combat_m =
  | Commands
  | Moves
  | Attack
  | End_Battle
  | Exit

let combat_mode = ref Commands
let battle_sim : Combat.battle_record pointer = null ()
let captured_creature : creature pointer = null ()

(*****************************************************************)
(***************     Loading Some Assets     *********************)
(*****************************************************************)

(* let battle_bot = load_sprite "battle_bot" GUI_Folder 3 () *)
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
  let size = Array.length moves in
  let box_w, box_h = (370, 92) in
  let box_x, box_y = (28, 200 - box_h) in
  let text_x, text_y = (38, (4 * text_box_height / 5) - 12) in
  let text_xdif, text_ydif = (376, 95) in
  set_line_width 6;
  let rec draw_all_moves i j length =
    if length <= 0 then ()
    else
      let move = get_move_i creature (i + (j * 2)) in
      let x, y = (text_x + (text_xdif * i), text_y - (text_ydif * j)) in
      (match move with
      | None -> ()
      | Some m ->
          draw_string_colored x y 40 m.move_name
            (get_color_from_etype m.etype)
            text_color ();
          draw_string_colored x (y - 40) 30
            (string_of_etype m.etype)
            (get_color_from_etype m.etype)
            text_color ();

          draw_string_colored (x + 110) (y - 40) 30
            ("PP:" ^ string_of_int m.curr_pp ^ "/"
           ^ string_of_int m.max_pp)
            white text_color ());

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
    draw_sprite sprite
      (width - sprite_width - 14)
      (360 - sprite_height) ();
    draw_string_colored (width - 320) 316 1
      (String.uppercase_ascii name)
      white text_color ();

    draw_string_colored (width - 100) 316 1
      ("Lv" ^ string_of_int level)
      white text_color ()
  end
  else begin
    draw_sprite sprite 42 (height - 45 - sprite_height) ();
    draw_string_colored 60 (height - 85) 1
      (String.uppercase_ascii name)
      white text_color ();

    draw_string_colored 280 (height - 85) 0
      ("Lv" ^ string_of_int level)
      white text_color ()
  end;
  if still then draw_health_bar_combat max before before player false ()
  else draw_health_bar_combat max before after player true ()

let draw_combat_commands () =
  let x, y = (465, 120) in
  set_color text_color;
  clear_text battle_right ();
  draw_string_colored x y 1 "FIGHT" white text_color ();
  draw_string_colored x (y - 75) 1 "BAG" white text_color ();
  draw_string_colored (x + 175) y 1 "PARTY" white text_color ();
  draw_string_colored (x + 175) (y - 75) 1 "RUN" white text_color ();
  draw_string_colored
    (x - 35 + (175 * commands_position.x))
    (y - (75 * commands_position.y))
    1 ">" white text_color ();

  (draw_text_string_pos 35 132 40 14
     ("What will " ^ get_nickname ~!battle_sim.player_battler.creature)
     white)
    ()

(*****************************************************************)
(***************     Test Demo Cmmands     *********************)
(*****************************************************************)
let refresh_hud () =
  let player, opponent =
    ( ~!battle_sim.player_battler.creature,
      ~!battle_sim.enemy_battler.creature )
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
  draw_sprite level_up_screen (width - 300) 210 ();
  draw_string_colored x y 0
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
      0
      (string_of_stat_short s)
      white text_color ();

    draw_string_colored x2
      (y - (dif * (i + 1)))
      0
      (string_of_int (get_stat2 old_stats s))
      white text_color ();
    draw_string_colored (x2 + 80)
      (y - (dif * (i + 1)))
      0
      ("+"
      ^ string_of_int (get_stat2 new_stats s - get_stat2 old_stats s))
      white text_color ()
  done;
  present ();
  wait (-1) ();
  draw_sprite level_up_screen (width - 300) 210 ();
  draw_string_colored x y 1
    ("Level " ^ string_of_int (get_level creature))
    white text_color ();
  for i = 0 to 5 do
    let s = List.nth stat_lst i in

    draw_string_colored x
      (y - (dif * (i + 1)))
      0
      (string_of_stat_short s)
      white text_color ();

    draw_string_colored x2
      (y - (dif * (i + 1)))
      0
      (string_of_int (get_stat2 new_stats s))
      white text_color ()
  done;
  present ();
  wait (-1) ();
  !Combat.refresh_battle
    (get_current_hp ~!battle_sim.player_battler.creature)
    (get_current_hp ~!battle_sim.enemy_battler.creature)
    0 ();

  present ()

let handle_exp player_creature enemy_creature () =
  Ui.update_all ();
  let exp_gain =
    get_exp_gain enemy_creature
    / List.length
        (List.filter
           (fun c -> get_status c <> Fainted)
           ~!battle_sim.creatures_switched)
  in

  let exp_event target player =
    if get_status target <> Fainted then begin
      let curr_level = get_level target in
      let exp_list = add_exp target exp_gain in
      add_ev_gain target (get_ev_gain enemy_creature);
      Ui.add_last_foreground
        (draw_text
           (get_nickname target ^ " gained " ^ string_of_int exp_gain
          ^ " EXP. Points!")
           40 true false);
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
                   40 true false);
              Ui.add_last_foreground (draw_level_up target)
            end;
            level_up_handler lvl t
      in

      level_up_handler curr_level (List.rev exp_list)
    end
  in

  exp_event player_creature true;
  let rec rest_exp_events = function
    | [] -> ()
    | h :: t ->
        if h <> player_creature then exp_event h false else ();

        rest_exp_events t
  in
  rest_exp_events ~!battle_sim.creatures_switched

let rec faint base c sprite player () =
  let sprite_width, sprite_height = get_dimension sprite in
  let xx, yy =
    if player then (50, 166)
    else (width - 50 - sprite_width, height - 50 - sprite_height)
  in
  if c = base - 2 then begin
    !Combat.refresh_battle
      (get_current_hp ~!battle_sim.player_battler.creature)
      (get_current_hp ~!battle_sim.enemy_battler.creature)
      (if player then 1 else 0)
      ();
    present ()
  end
  else begin
    !Combat.refresh_battle
      (get_current_hp ~!battle_sim.player_battler.creature)
      (get_current_hp ~!battle_sim.enemy_battler.creature)
      (if player then 1 else 0)
      ();
    draw_sprite_crop sprite xx
      (yy - (sprite_height - (sprite_height / c)))
      (0, sprite_width)
      (sprite_height - (sprite_height / c), sprite_height)
      ();
    clear_text DrawText.battle_bot ();
    present ();
    Input.sleep 0.075 ();
    faint base (c + 1) sprite player ()
  end;
  set_color text_color

let animate_faint creature player () = faint 20 1 creature player ()

let handle_combat move =
  Ui.update_all ();
  if ~!battle_sim.battle_status = Ongoing then begin
    Combat.turn_builder ~!battle_sim move;
    let player, enemy =
      ( ~!battle_sim.player_battler.creature,
        ~!battle_sim.enemy_battler.creature )
    in

    (***=============First Half =============***)
    Combat.battle_sim_fh ~!battle_sim;
    Ui.update_all ();
    (***=============Second Half =============***)
    Combat.battle_sim_sh ~!battle_sim;
    Ui.update_all ();
    (***============= Resolution =============***)
    if ~!battle_sim.enemy_battler.active = false then (
      combat_mode := End_Battle;
      Ui.add_last_gameplay
        (animate_faint (get_front_sprite enemy) false);
      ~!battle_sim.enemy_battler.active <- false;
      Ui.add_last_gameplay (Input.sleep 0.5);

      handle_exp player enemy ();
      Ui.update_all ());
    if ~!battle_sim.player_battler.active = false then begin
      combat_mode := Attack;
      Ui.add_last_gameplay (animate_faint (get_back_sprite player) true)
    end
  end

(* let pokeball_spritesheet = Spritesheet.init_spritesheet
   "assets/item_sprites/pokeball_capture.png" 64 64 3 *)

let handle_item item () =
  combat_mode := Attack;
  match Item.get_type item with
  | Item.Misc -> false
  | Item.Medicine -> (
      print_endline "HI!";
      Party_menu.set_current_item item;
      Party_menu.init Party_menu.InventoryMode ();
      match Party_menu.get_current_item with
      | Some _ -> false
      | None -> true)
  | Item.Ball ->
      let ball_type = Item.get_id item mod 50 in
      let modifier =
        match ball_type with
        | 1 -> 1.5
        | 2 -> 2.0
        | _ -> 1.0
      in

      let _ = Combat.capture ~!battle_sim modifier in
      Ui.update_all ();
      (* Ui.add_last_gameplay (Animation.capture_animation
         pokeball_spritesheet (get_front_sprite
         ~!battle_sim.enemy_battler.creature) catch_results ball_type
         (!Combat.refresh_battle (get_current_hp
         ~!battle_sim.player_battler.creature) (get_current_hp
         ~!battle_sim.enemy_battler.creature))); *)
      Ui.update_all ();
      if ~!battle_sim.battle_status = Catch then begin
        print_endline "Success";
        Ui.add_last_foreground
          (draw_text
             ("You captured "
             ^ get_nickname ~!battle_sim.enemy_battler.creature
             ^ "!")
             40 true false);
        handle_exp ~!battle_sim.player_battler.creature
          ~!battle_sim.enemy_battler.creature ();
        ~!battle_sim.enemy_battler.active <- false;
        captured_creature *= ~!battle_sim.enemy_battler.creature;

        combat_mode := End_Battle
      end
      else begin
        Ui.add_last_foreground
          (draw_text "Aw... So close!" 40 true false);
        print_endline "Failure"
      end;
      Ui.update_all ();
      true
  | Item.Key -> false

let refresh_battle () =
  Ui.add_first_foreground (clear_text battle_bot);
  Ui.add_last_background (draw_sprite battle_bg1 0 0);
  if ~!battle_sim.enemy_battler.active = true then
    Ui.add_first_gameplay
      (draw_creature
         (get_front_sprite ~!battle_sim.enemy_battler.creature)
         false);
  if ~!battle_sim.player_battler.active = true then
    Ui.add_first_gameplay
      (draw_creature
         (get_back_sprite ~!battle_sim.player_battler.creature)
         true);

  Ui.add_first_gameplay refresh_hud;
  Ui.add_first_foreground
    (draw_creature_exp ~!battle_sim.player_battler.creature false);
  Ui.add_first_foreground draw_combat_commands;
  present ()

let clear_battle p_hp e_hp state () =
  let player, opponent =
    ( ~!battle_sim.player_battler.creature,
      ~!battle_sim.enemy_battler.creature )
  in
  (draw_sprite battle_bg1 0 0) ();

  draw_combat_hud combat_hud (get_nickname opponent)
    (get_level opponent) false
    ((get_stats opponent).max_hp, e_hp, e_hp)
    true ();

  (* Ui.add_first_gameplay *)
  draw_combat_hud player_hud (get_nickname player) (get_level player)
    true
    ((get_stats player).max_hp, p_hp, p_hp)
    true ();
  (draw_creature_exp ~!battle_sim.player_battler.creature false) ();
  match state with
  | 0 ->
      if ~!battle_sim.player_battler.active then
        draw_creature
          (get_back_sprite ~!battle_sim.player_battler.creature)
          true ()
  | 1 ->
      if ~!battle_sim.enemy_battler.active then
        draw_creature
          (get_front_sprite ~!battle_sim.enemy_battler.creature)
          false ()
  | 2 ->
      if ~!battle_sim.player_battler.active then
        draw_creature
          (get_back_sprite ~!battle_sim.player_battler.creature)
          true ();
      if ~!battle_sim.enemy_battler.active then
        draw_creature
          (get_front_sprite ~!battle_sim.enemy_battler.creature)
          false ()
  | _ -> ()
(* (clear_text Draw.battle_bot) () *)

let rec handle_inventory () =
  Inventory_menu.init ();
  Ui.add_first_gameplay (clear_text DrawText.battle_bot);
  match !Inventory_menu.selected_item with
  | Some i ->
      let valid_item = handle_item i () in
      if valid_item then begin
        Ui.update_all ();
        Ui.add_first_background
          (!Combat.refresh_battle
             (get_current_hp ~!battle_sim.player_battler.creature)
             (get_current_hp ~!battle_sim.enemy_battler.creature)
             2);
        Inventory.consume_item (Player.inventory (State.player ())) i;
        handle_combat None
      end
      else handle_inventory ()
  | None ->
      refresh_battle ();
      Ui.update_all ()

let move_x x () =
  match !combat_mode with
  | Commands ->
      if commands_position.x + x >= 0 && commands_position.x + x < 2
      then commands_position.x <- commands_position.x + x
  | Moves ->
      if moves_position.x + x >= 0 && moves_position.x + x < 2 then
        moves_position.x <- moves_position.x + x
  | _ -> ()

let move_y y () =
  match !combat_mode with
  | Commands ->
      if commands_position.y + y >= 0 && commands_position.y + y < 2
      then commands_position.y <- commands_position.y + y
  | Moves ->
      if moves_position.y + y >= 0 && moves_position.y + y < 2 then
        moves_position.y <- moves_position.y + y
  | _ -> ()

let rec run_tick () =
  Input.sleep Draw.tick_rate ();
  let key =
    match Input.poll_key_option () with
    | Some c -> c
    | None -> Unknown
  in

  if key = D || key = Right then move_x 1 ();
  if key = A || key = Left then move_x (-1) ();
  if key = W || key = Up then move_y (-1) ();
  if key = S || key = Down then move_y 1 ();
  let action =
    match !combat_mode with
    | Commands ->
        (* ===================================== *)
        (* =================MOVES=============== *)
        (* ===================================== *)
        if
          (key <> E && key <> X)
          || key = A || key = Left || key = W || key = Up || key = S
          || key = Down
        then Ui.add_first_foreground draw_combat_commands;
        if
          (key = E || key = Z)
          && commands_position.x = 0 && commands_position.y = 0
          && ~!battle_sim.battle_status <> Combat.Victory
        then begin
          combat_mode := Moves;
          Ui.add_first_gameplay (clear_text moves_window);
          Ui.add_first_foreground
            (draw_moves ~!battle_sim.player_battler.creature)
          (* ===================================== *)
          (* =================BAG/INVENTORY=============== *)
          (* ===================================== *)
        end
        else if
          (key = E || key = Z)
          && commands_position.x = 0 && commands_position.y = 1
        then handle_inventory ()
          (* ===================================== *)
          (* =================PARTY=============== *)
          (* ===================================== *)
        else if
          (key = E || key = Z)
          && commands_position.x = 1 && commands_position.y = 0
        then begin
          Party_menu.init BattleSwitch ();
          match !Combat.switching_pending with
          | Some c ->
              Ui.add_first_foreground (clear_text DrawText.battle_bot);

              Combat.switch_player ~!battle_sim c
                (Player.party (State.player ()));
              refresh_battle ();
              Ui.update_all ();
              combat_mode := Attack;
              Combat.switching_pending := None;
              handle_combat None;
              commands_position.x <- 0;
              commands_position.y <- 0
          | None -> refresh_battle ()
          (* ===================================== *)
          (* =================RUNAWAY=============== *)
          (* ===================================== *)
        end
        else if
          (key = E || key = Z)
          && commands_position.x = 1 && commands_position.y = 1
        then begin
          Combat.run_away ~!battle_sim;
          if ~!battle_sim.battle_status = Combat.Flee then begin
            Ui.add_first_foreground
              (draw_text "You ran away!" 40 true true);

            combat_mode := End_Battle
          end
          else begin
            Ui.add_first_foreground
              (draw_text "You could not run away!" 40 true false);
            Ui.update_all ();

            handle_combat None
          end
        end
    | Moves ->
        if
          (key <> E && key <> X)
          || key = A || key = Left || key = W || key = Up || key = S
          || key = Down
        then
          Ui.add_first_foreground
            (draw_moves ~!battle_sim.player_battler.creature);

        if key = E || key = Z then begin
          Ui.add_first_foreground (clear_text DrawText.battle_bot);
          let move =
            (get_move_i ~!battle_sim.player_battler.creature)
              (moves_position.x + (2 * moves_position.y))
          in
          Ui.update_all ();
          combat_mode := Attack;
          handle_combat move
        end;
        if key = Q || key = X then begin
          Ui.clear_ui Ui.Foreground;
          Ui.add_first_foreground draw_combat_commands;
          combat_mode := Commands
        end
    | Attack ->
        Ui.add_first_gameplay (clear_text battle_right);
        print_endline "Did this work?????";
        if get_status ~!battle_sim.player_battler.creature = Fainted
        then begin
          Ui.update_all ();
          print_endline "Did this work?";
          Party_menu.init FaintedSwitch ();
          match !Combat.switching_pending with
          | Some c ->
              Ui.add_first_foreground (clear_text DrawText.battle_bot);
              Combat.switch_player ~!battle_sim c
                (Player.party (State.player ()));
              refresh_battle ();
              Ui.update_all ();
              combat_mode := Commands;
              Combat.switching_pending := None;
              commands_position.x <- 0;
              commands_position.y <- 0
          | None -> refresh_battle ()
        end;

        if ~!battle_sim.enemy_battler.active then
          combat_mode := Commands
        else combat_mode := End_Battle
    | End_Battle ->
        Player.set_party ~!battle_sim.player_creatures (State.player ());
        (match !captured_creature with
        | None -> ()
        | Some c ->
            Event_menu.init_capture c ();
            Player.add_creature c (State.player ()));

        (* in *)
        combat_mode := Exit;
        wait 175000 ()
    | Exit -> ()
  in

  action;

  Ui.update_all ();
  if !combat_mode <> Exit then run_tick ()

let start_wild_battle c =
  Combat.refresh_battle := clear_battle;
  Combat.health_bar := draw_health_bar_combat;
  Ui.add_last_background clear_screen;
  combat_mode := Commands;

  Ui.add_last_background (clear_text battle_right);

  (* ========= Start the Battle ========= *)
  let rec battle_party fainted = function
    | [] -> fainted
    | h :: t ->
        if get_status h = Fainted then battle_party (h :: fainted) t
        else (h :: t) @ fainted
  in

  battle_sim *= Combat.wild_init (Player.party (State.player ())) [ c ];

  Player.set_party
    (battle_party [] (Player.party (State.player ())))
    (State.player ());

  (* ========= Draw the Battle ========= *)
  refresh_battle ();
  Ui.update_all ();
  run_tick ()
