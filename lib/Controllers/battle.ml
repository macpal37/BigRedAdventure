open Draw
open Creature
open Creature.Move
open Animation
open Util
open Input
open DrawText

let moves_position = new_point ()
let cmd_pos = new_point ()

type combat_m =
  | Commands
  | Moves
  | Attack
  | End_Battle
  | Exit

let combat_mode = ref Commands
let bs : Combat.battle_record pointer = null ()
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

let draw_moves () =
  clear_text moves_window ();
  let moves = get_moves ~!bs.player_battler.creature in
  let size = Array.length moves in
  let box_w, box_h = (370, 92) in
  let box_x, box_y = (28, 200 - box_h) in
  (* Height of Box 212 *)
  let text_x, text_y = (38, (4 * 212 / 5) - 12) in
  let text_xdif, text_ydif = (376, 95) in
  set_line_width 6;
  let rec draw_all_moves i j length =
    if length <= 0 then ()
    else
      let move =
        get_move_i ~!bs.player_battler.creature (i + (j * 2))
      in
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

  set_color (rgb 255 0 0);
  draw_rect
    (box_x + (box_w * moves_position.x))
    (box_y - (box_h * moves_position.y))
    box_w box_h;
  set_color text_color

(* let draw_exp_bar_combat max curr () = let hwidth = 250 in let hheight
   = 6 in

   let xh, yh = (width - hwidth - 30, 240) in draw_exp_bar max curr xh
   yh hwidth hheight () *)

let draw_health_bar_combat (max : float) (curr : float) player () =
  let hwidth = 210 in
  let hheight = 6 in
  let xh, yh =
    if player then (width - hwidth - 31 - 10, 296) else (130, 615)
  in
  draw_health_bar max curr xh yh hwidth hheight player ()

let draw_combat_hud
    sprite
    name
    level
    player
    ((max, curr) : float * float)
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
  draw_health_bar_combat max curr player ()

let draw_combat_commands () =
  let x, y = (465, 120) in
  set_color text_color;
  clear_text battle_right ();
  draw_string_colored x y 1 "FIGHT" white text_color ();
  draw_string_colored x (y - 75) 1 "BAG" white text_color ();
  draw_string_colored (x + 175) y 1 "PARTY" white text_color ();
  draw_string_colored (x + 175) (y - 75) 1 "RUN" white text_color ();
  draw_string_colored
    (x - 35 + (175 * cmd_pos.x))
    (y - (75 * cmd_pos.y))
    1 ">" white text_color ();

  (draw_text_string_pos 35 132 40 14
     ("What will " ^ get_nickname ~!bs.player_battler.creature)
     white)
    ()

(* Refreshes *)
let refresh_hud () =
  let player, opponent =
    (~!bs.player_battler.creature, ~!bs.enemy_battler.creature)
  in

  (* Ui.add_first_gameplay *)
  draw_combat_hud combat_hud (get_nickname opponent)
    (get_level opponent) false
    ((get_stats opponent).max_hp, get_current_hp opponent)
    ();

  (* Ui.add_first_gameplay *)
  draw_combat_hud player_hud (get_nickname player) (get_level player)
    true
    ((get_stats player).max_hp, get_current_hp player)
    ()

(* let update_health creature before () = let curr, max =
   (get_current_hp creature, (get_stats creature).max_hp) in
   draw_health_bar_combat max before curr false true () *)

(* let draw_creature_exp creature () = let curr_exp, min_exp, max_exp =
   get_exp creature in let curr_exp = if curr_exp >= max_exp then
   min_exp else curr_exp in Ui.add_first_foreground (draw_exp_bar_combat
   (max_exp -. min_exp) (curr_exp -. min_exp)) *)

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
      (string_of_intf (get_stat2 old_stats s))
      white text_color ();
    draw_string_colored (x2 + 80)
      (y - (dif * (i + 1)))
      0
      ("+"
      ^ string_of_intf (get_stat2 new_stats s -. get_stat2 old_stats s)
      )
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
      (string_of_intf (get_stat2 new_stats s))
      white text_color ()
  done;

  wait (-1) ()

let handle_exp player_creature enemy_creature () =
  Ui.update_all ();
  let exp_gain =
    get_exp_gain enemy_creature
    / List.length
        (List.filter
           (fun c -> get_status c <> Fainted)
           ~!bs.creatures_switched)
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
            (* let max, bef, aft, lvl = h in *)
            let _, _, _, lvl = h in

            if player then ();
            (* TODO: Animate EXP! *)
            (* Ui.add_last_foreground (draw_exp_bar_combat max bef
               aft); *)
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
  rest_exp_events ~!bs.creatures_switched

let handle_combat move =
  Ui.update_all ();
  if ~!bs.battle_status = Ongoing then begin
    Combat.turn_builder ~!bs move;
    let player, enemy =
      (~!bs.player_battler.creature, ~!bs.enemy_battler.creature)
    in
    (***=============First Half =============***)
    Combat.battle_sim_fh ~!bs;
    Ui.update_all ();
    (***=============Second Half =============***)
    Combat.battle_sim_sh ~!bs;
    Ui.update_all ();
    (***============= Resolution =============***)
    if ~!bs.enemy_battler.active = false then (
      combat_mode := End_Battle;

      (* (TODO: Faint Animation) *)
      (* Ui.add_last_gameplay (animate_faint (get_front_sprite enemy)
         false); *)
      ~!bs.enemy_battler.active <- false;
      Ui.add_last_gameplay (Input.sleep 0.5);

      handle_exp player enemy ();
      Ui.update_all ());
    if ~!bs.player_battler.active = false then combat_mode := Attack
    (* (TODO: Faint Animation) *)
    (* Ui.add_last_gameplay (animate_faint (get_back_sprite player)
       true) *)
  end

let handle_item item () =
  combat_mode := Attack;
  match Item.get_type item with
  | Item.Misc -> false
  | Item.Medicine -> (
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

      let _ = Combat.capture ~!bs modifier in
      Ui.update_all ();
      (* Ui.add_last_gameplay (Animation.capture_animation
         pokeball_spritesheet (get_front_sprite
         ~!bs.enemy_battler.creature) catch_results ball_type
         (!Combat.refresh_battle (get_current_hp
         ~!bs.player_battler.creature) (get_current_hp
         ~!bs.enemy_battler.creature))); *)
      Ui.update_all ();
      if ~!bs.battle_status = Catch then begin
        print_endline "Success";
        Ui.add_last_foreground
          (draw_text
             ("You captured "
             ^ get_nickname ~!bs.enemy_battler.creature
             ^ "!")
             0 true false);
        handle_exp ~!bs.player_battler.creature
          ~!bs.enemy_battler.creature ();
        ~!bs.enemy_battler.active <- false;
        captured_creature *= ~!bs.enemy_battler.creature;

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
  Ui.add_last_background (draw_sprite battle_bg1 0 0);
  (* Draws the ally and enemy creature *)
  if ~!bs.enemy_battler.active = true then
    Ui.add_first_gameplay
      (draw_creature
         (get_front_sprite ~!bs.enemy_battler.creature)
         false);
  if ~!bs.player_battler.active = true then
    Ui.add_first_gameplay
      (draw_creature
         (get_back_sprite ~!bs.player_battler.creature)
         true);

  Ui.add_last_foreground refresh_hud;
  Ui.add_first_foreground
    (match !combat_mode with
    | Commands -> draw_combat_commands
    | Moves -> draw_moves
    | _ -> clear_text battle_bot)

(* let refresh_battle1 () = Ui.add_first_foreground (clear_text
   battle_bot); Ui.add_last_background (draw_sprite battle_bg1 0 0); if
   ~!bs.enemy_battler.active = true then Ui.add_first_gameplay
   (draw_creature (get_front_sprite ~!bs.enemy_battler.creature) false);
   if ~!bs.player_battler.active = true then Ui.add_first_gameplay
   (draw_creature (get_back_sprite ~!bs.player_battler.creature) true);

   Ui.add_first_gameplay refresh_hud; Ui.add_first_foreground
   (draw_creature_exp ~!bs.player_battler.creature false);
   Ui.add_first_foreground draw_combat_commands *)

(* let clear_battle p_hp e_hp state () = let player, opponent =
   (~!bs.player_battler.creature, ~!bs.enemy_battler.creature) in
   (draw_sprite battle_bg1 0 0) ();

   draw_combat_hud combat_hud (get_nickname opponent) (get_level
   opponent) false ((get_stats opponent).max_hp, e_hp) ();

   (* Ui.add_first_gameplay *) draw_combat_hud player_hud (get_nickname
   player) (get_level player) true ((get_stats player).max_hp, p_hp) ();
   (draw_creature_exp ~!bs.player_battler.creature) (); match state with
   | 0 -> if ~!bs.player_battler.active then draw_creature
   (get_back_sprite ~!bs.player_battler.creature) true () | 1 -> if
   ~!bs.enemy_battler.active then draw_creature (get_front_sprite
   ~!bs.enemy_battler.creature) false () | 2 -> if
   ~!bs.player_battler.active then draw_creature (get_back_sprite
   ~!bs.player_battler.creature) true (); if ~!bs.enemy_battler.active
   then draw_creature (get_front_sprite ~!bs.enemy_battler.creature)
   false () | _ -> () *)

let handle_party () =
  Party_menu.init BattleSwitch ();
  match !Combat.switching_pending with
  | Some c ->
      Ui.add_first_foreground (clear_text DrawText.battle_bot);

      Combat.switch_player ~!bs c (Player.party (State.player ()));
      refresh_battle ();
      Ui.update_all ();
      combat_mode := Attack;
      Combat.switching_pending := None;
      handle_combat None;
      cmd_pos.x <- 0;
      cmd_pos.y <- 0
  | None -> refresh_battle ()

let rec handle_inventory () =
  Inventory_menu.init ();
  Ui.add_first_gameplay (clear_text DrawText.battle_bot);
  match !Inventory_menu.selected_item with
  | Some i ->
      let valid_item = handle_item i () in
      if valid_item then begin
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
      if cmd_pos.x + x >= 0 && cmd_pos.x + x < 2 then
        cmd_pos.x <- cmd_pos.x + x
  | Moves ->
      if moves_position.x + x >= 0 && moves_position.x + x < 2 then
        moves_position.x <- moves_position.x + x
  | _ -> ()

let move_y y () =
  match !combat_mode with
  | Commands ->
      if cmd_pos.y + y >= 0 && cmd_pos.y + y < 2 then
        cmd_pos.y <- cmd_pos.y + y
  | Moves ->
      if moves_position.y + y >= 0 && moves_position.y + y < 2 then
        moves_position.y <- moves_position.y + y
  | _ -> ()

type commands =
  | Fight
  | Bag
  | Party
  | Run

let selected_command () =
  if cmd_pos.x = 0 then if cmd_pos.y = 0 then Fight else Bag
  else if cmd_pos.y = 0 then Party
  else Run

let rec run_tick () =
  Input.sleep Draw.tick_rate ();
  let key =
    match Input.poll_key_option () with
    | Some c -> get_ctrl_key c
    | None -> NoKey
  in
  (*===== Selecting a command ===*)
  (match key with
  | Right -> move_x 1 ()
  | Left -> move_x (-1) ()
  | Up -> move_y (-1) ()
  | Down -> move_y 1 ()
  | _ -> ());

  (match !combat_mode with
  | Commands -> (
      if key = Action then
        match selected_command () with
        | Fight -> combat_mode := Moves
        | Bag -> handle_inventory ()
        | Party -> handle_party ()
        | Run ->
            Combat.run_away ~!bs;
            if ~!bs.battle_status = Combat.Flee then begin
              Ui.add_first_foreground
                (draw_text "You ran away!" 40 true true);

              combat_mode := End_Battle
            end
            else begin
              Ui.add_first_foreground
                (draw_text "You could not run away!" 40 true false);
              Ui.update_all ();
              handle_combat None
            end)
  | Moves ->
      if key = Action then begin
        Ui.add_first_foreground (clear_text DrawText.battle_bot);
        let move =
          (get_move_i ~!bs.player_battler.creature)
            (moves_position.x + (2 * moves_position.y))
        in
        Ui.update_all ();
        combat_mode := Attack;
        handle_combat move
      end;
      if key = Back then begin
        Ui.clear_ui Ui.Foreground;
        Ui.add_first_foreground draw_combat_commands;
        combat_mode := Commands
      end
  | Attack ->
      Ui.add_first_gameplay (clear_text battle_right);
      if get_status ~!bs.player_battler.creature = Fainted then begin
        Ui.update_all ();
        Party_menu.init FaintedSwitch ();
        match !Combat.switching_pending with
        | Some c ->
            Ui.add_first_foreground (clear_text DrawText.battle_bot);
            Combat.switch_player ~!bs c (Player.party (State.player ()));
            refresh_battle ();
            Ui.update_all ();
            combat_mode := Commands;
            Combat.switching_pending := None;
            cmd_pos.x <- 0;
            cmd_pos.y <- 0
        | None -> refresh_battle ()
      end;

      if ~!bs.enemy_battler.active then combat_mode := Commands
      else combat_mode := End_Battle
  | End_Battle ->
      Player.set_party ~!bs.player_creatures (State.player ());
      (match !captured_creature with
      | None -> ()
      | Some c ->
          Event_menu.init_capture c ();
          Player.add_creature c (State.player ()));
      combat_mode := Exit;
      (* wait 175000 () *)
      wait (175000 / 4) ()
  | Exit -> ());

  refresh_battle ();
  Ui.update_all ();
  if !combat_mode <> Exit then run_tick ()

let start_wild_battle c =
  combat_mode := Commands;
  (* ========= Start the Battle ========= *)
  (* ========= Filter out the fainted creatures ========= *)
  let rec battle_party fainted = function
    | [] -> fainted
    | h :: t ->
        if get_status h = Fainted then battle_party (h :: fainted) t
        else (h :: t) @ fainted
  in

  bs *= Combat.wild_init (Player.party (State.player ())) [ c ];

  Player.set_party
    (battle_party [] (Player.party (State.player ())))
    (State.player ());

  (* ========= Draw the Battle ========= *)
  run_tick ()
