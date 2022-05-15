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

type hud_stats = {
  mutable max_hp : float;
  mutable curr_hp : float;
  mutable status : status;
  mutable max_exp : float;
  mutable curr_exp : float;
  mutable level : int;
}

let update_player stats c =
  let a, b = get_hp_status c in
  let x, y, z = get_exp c in
  stats
  *= {
       max_hp = a;
       curr_hp = b;
       status = get_status c;
       max_exp = z -. y;
       curr_exp = x -. y;
       level = get_level c;
     }

let p_hud_stats : hud_stats pointer = null ()
let e_hud_stats : hud_stats pointer = null ()

(*****************************************************************)
(***************     Loading Some Assets     *********************)
(*****************************************************************)

(* let battle_bot = load_sprite "battle_bot" GUI_Folder 3 () *)
let battle_right = Util.null ()
let moves_window = Util.null ()
let combat_hud = Util.null ()
let player_hud = Util.null ()
let battle_bg1 = Util.null ()
let level_up_screen = Util.null ()

let load_assets _ =
  battle_right *= Sprite_assets.get_sprite2 "battle_top" GUI_Folder;
  moves_window *= Sprite_assets.get_sprite2 "moves_window" GUI_Folder;
  combat_hud *= Sprite_assets.get_sprite2 "opponent_hud" GUI_Folder;
  player_hud *= Sprite_assets.get_sprite2 "player_hud" GUI_Folder;
  battle_bg1 *= Sprite_assets.get_sprite2 "battle-bg1" GUI_Folder;
  level_up_screen *= Sprite_assets.get_sprite2 "level_up" GUI_Folder

(* WIll be improved next sprint *)

(*****************************************************************)
(***************     Combat Drawing Commands     *********************)
(*****************************************************************)

let draw_moves creature () =
  clear_text ~!moves_window ();
  let moves = get_moves creature in
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

let draw_exp_bar_combat () =
  let hwidth = 250 in
  let hheight = 6 in
  let xh, yh = (width - hwidth - 30, 240) in
  draw_exp_bar ~!p_hud_stats.max_exp ~!p_hud_stats.curr_exp xh yh hwidth
    hheight ()

let draw_health_bar_combat (max : float) (curr : float) player () =
  let hwidth = 210 in
  let hheight = 6 in
  let xh, yh =
    if player then (width - hwidth - 31 - 10, 296) else (130, 615)
  in
  draw_health_bar max curr xh yh hwidth hheight player ()

let draw_combat_hud sprite name level player () =
  let sprite_width, sprite_height = get_dimension sprite in
  if player then begin
    draw_sprite sprite
      (width - sprite_width - 14)
      (360 - sprite_height) ();
    draw_string_colored (width - 320) 312 0
      (String.uppercase_ascii name)
      white text_color ();
    draw_string_colored (width - 100) 312 0
      ("Lv" ^ string_of_int level)
      white text_color ();
    draw_health_bar_combat ~!p_hud_stats.max_hp ~!p_hud_stats.curr_hp
      player ();
    draw_exp_bar_combat ()
  end
  else begin
    draw_sprite sprite 42 (height - 49 - sprite_height) ();
    draw_string_colored 60 (height - 85) 0
      (String.uppercase_ascii name)
      white text_color ();
    draw_string_colored 280 (height - 89) 0
      ("Lv" ^ string_of_int level)
      white text_color ();
    draw_health_bar_combat ~!e_hud_stats.max_hp ~!e_hud_stats.curr_hp
      player ()
  end

let draw_combat_commands () =
  let x, y = (465, 120) in
  set_color text_color;
  clear_text ~!battle_right ();
  draw_string_colored x y 1 "FIGHT" white text_color ();
  draw_string_colored x (y - 75) 1 "BAG" white text_color ();
  draw_string_colored (x + 175) y 1 "PARTY" white text_color ();
  draw_string_colored (x + 175) (y - 75) 1 "RUN" white text_color ();
  draw_string_colored
    (x - 35 + (175 * cmd_pos.x))
    (y - (75 * cmd_pos.y))
    1 ">" white text_color ();

  (draw_text_string_pos 35 132 40 14
     ("What will " ^ get_nickname ~!bs.player_battler.creature))
    ()

(* Refreshes *)
let draw_hud () =
  let player, opponent =
    (~!bs.player_battler.creature, ~!bs.enemy_battler.creature)
  in

  draw_combat_hud ~!combat_hud (get_nickname opponent)
    (get_level opponent) false ();
  draw_combat_hud ~!player_hud (get_nickname player) (get_level player)
    true ()

(* let update_health creature before () = let curr, max =
   (get_current_hp creature, (get_stats creature).max_hp) in
   draw_health_bar_combat max before curr false true () *)

(* let draw_creature_exp creature () = let curr_exp, min_exp, max_exp =
   get_exp creature in let curr_exp = if curr_exp >= max_exp then
   min_exp else curr_exp in Ui.add_first_foreground (draw_exp_bar_combat
   (max_exp -. min_exp) (curr_exp -. min_exp)) *)

let draw_level_up creature frame () =
  let x, y, dif, x2 = (width - 300 + 30, 200 + 250, 35, width - 140) in
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
  if frame = 0 then begin
    let old_stats = get_stats creature in
    level_up creature ();
    let new_stats = get_stats creature in

    draw_sprite ~!level_up_screen (width - 300) 210 ();
    draw_string_colored x y 0
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
        (string_of_intf (get_stat2 old_stats s))
        white text_color ();
      draw_string_colored (x2 + 80)
        (y - (dif * (i + 1)))
        0
        ("+"
        ^ string_of_intf (get_stat2 new_stats s -. get_stat2 old_stats s)
        )
        white text_color ()
    done
  end
  else if frame = 1 then begin
    let new_stats = get_stats creature in
    draw_sprite ~!level_up_screen (width - 300) 210 ();
    draw_string_colored x y 0
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
    done
  end

let refresh_battle state () =
  Ui.add_last_background (draw_sprite ~!battle_bg1 0 0);

  (* Draws the ally and enemy creature *)
  (match state with
  | 0 ->
      if ~!bs.player_battler.active then
        Ui.add_last_gameplay
          (draw_creature
             (get_back_sprite ~!bs.player_battler.creature)
             true)
  | 1 ->
      if ~!bs.enemy_battler.active then
        Ui.add_last_gameplay
          (draw_creature
             (get_front_sprite ~!bs.enemy_battler.creature)
             false)
  | 2 ->
      if ~!bs.player_battler.active then
        Ui.add_last_gameplay
          (draw_creature
             (get_back_sprite ~!bs.player_battler.creature)
             true);
      if ~!bs.enemy_battler.active then
        Ui.add_last_gameplay
          (draw_creature
             (get_front_sprite ~!bs.enemy_battler.creature)
             false)
  | _ -> ());
  Ui.add_last_foreground draw_hud;

  Ui.add_last_foreground
    (match !combat_mode with
    | Commands -> draw_combat_commands
    | Moves -> draw_moves ~!bs.player_battler.creature
    | Attack -> clear_text battle_bot
    | _ -> clear_text battle_bot)

let animate_exp_bar_combat max bef aft () =
  let hwidth = 250 in
  let hheight = 6 in
  let xh, yh = (width - hwidth - 30, 240) in
  animate_exp_bar max bef aft xh yh hwidth hheight (refresh_battle 2)

let animate_health_bar_combat max bef aft player () =
  let hwidth = 210 in
  let hheight = 6 in
  let xh, yh =
    if player then (width - hwidth - 31 - 10, 296) else (130, 615)
  in
  animate_health_bar max bef aft xh yh hwidth hheight player
    (refresh_battle 2)

let handle_exp player_creature enemy_creature () =
  let exp_gain =
    get_exp_gain enemy_creature
    /. float_of_int
         (List.length
            (List.filter
               (fun c -> get_status c <> Fainted)
               ~!bs.creatures_switched))
  in

  let exp_event target player =
    if get_status target <> Fainted then begin
      let curr_level = get_level target in
      let exp_list = add_exp target exp_gain in
      add_ev_gain target (get_ev_gain enemy_creature);
      Animation.display_text_box
        (get_nickname target ^ " gained " ^ string_of_intf exp_gain
       ^ " EXP. Points!")
        true (refresh_battle 2) ();
      let rec level_up_handler level = function
        | [] -> ()
        | h :: t ->
            (* let max, bef, aft, lvl = h in *)
            let max, bef, aft, lvl = h in

            if player then ();
            animate_exp_bar_combat max bef aft ();
            ~!p_hud_stats.curr_exp <- aft;

            if level <> lvl then begin
              level_up target ();
              Animation.display_text_box
                (get_nickname target ^ " grew to level "
               ^ string_of_int lvl ^ "!")
                true (refresh_battle 2) ();

              refresh_battle 2 ();
              Ui.add_last_foreground (draw_level_up target 0);
              Ui.update_all ();
              wait (-1) ();
              refresh_battle 2 ();
              Ui.add_last_foreground (draw_level_up target 1);
              Ui.update_all ();
              wait (-1) ()
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
  if ~!bs.battle_status = Ongoing then begin
    let player, enemy =
      (~!bs.player_battler.creature, ~!bs.enemy_battler.creature)
    in
    (***=============First Half =============***)
    Combat.battle_sim ~!bs move;
    let all_actions = List.rev !Combat.battle_actions in

    for i = 0 to List.length all_actions - 1 do
      let affected, action, text = List.nth all_actions i in
      let sprite, is_player, hud =
        if affected.is_player then
          (get_back_sprite affected.creature, true, p_hud_stats)
        else (get_front_sprite affected.creature, false, e_hud_stats)
      in
      let state = if is_player then 1 else 0 in
      match action with
      | ChooseMove _ -> display_text_box text true (refresh_battle 2) ()
      | Damage (dmg, max, curr, _, crit) ->
          animate_damage_render sprite is_player (refresh_battle state);
          display_text_box text true (refresh_battle 2) ();
          animate_health_bar_combat max curr (curr -. dmg) is_player ();
          ~!hud.curr_hp <- curr -. dmg;
          if crit then
            display_text_box "It was a critical hit!" true
              (refresh_battle 2) ()
      | Heal _ -> ()
      | StatusGain (_, _) -> ()
      | StatusEffect (_, _, _, _) -> ()
      | MaxStat -> display_text_box text true (refresh_battle 2) ()
      | StatGain stages ->
          if stages > 0 then
            animate_raise_stat_effect sprite is_player
              (refresh_battle 2)
          else if stages < 0 then
            animate_lower_stat_effect sprite is_player
              (refresh_battle 2)
      | Switch _ -> ()
      | Fainted ->
          affected.active <- false;
          animate_faint sprite is_player (refresh_battle 2);
          display_text_box text true (refresh_battle 2) ()
    done;
    Combat.battle_actions := [];
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
        display_text_box
          ("You captured "
          ^ get_nickname ~!bs.enemy_battler.creature
          ^ "!")
          false (refresh_battle 0) ();
        handle_exp ~!bs.player_battler.creature
          ~!bs.enemy_battler.creature ();
        ~!bs.enemy_battler.active <- false;
        captured_creature *= ~!bs.enemy_battler.creature;

        combat_mode := End_Battle
      end
      else
        display_text_box "Aw... So close!" false (refresh_battle 2) ();
      Ui.update_all ();
      true
  | Item.Key -> false

(* let refresh_battle1 () = Ui.add_first_foreground (clear_text
   battle_bot); Ui.add_last_background (draw_sprite battle_bg1 0 0); if
   ~!bs.enemy_battler.active = true then Ui.add_first_gameplay
   (draw_creature (get_front_sprite ~!bs.enemy_battler.creature) false);
   if ~!bs.player_battler.active = true then Ui.add_first_gameplay
   (draw_creature (get_back_sprite ~!bs.player_battler.creature) true);

   Ui.add_first_gameplay draw_hud; Ui.add_first_foreground
   (draw_creature_exp ~!bs.player_battler.creature false);
   Ui.add_first_foreground draw_combat_commands *)

let handle_party () =
  Party_menu.init BattleSwitch ();
  match !Combat.switching_pending with
  | Some c ->
      combat_mode := Attack;
      let b = ~!bs.player_battler.creature in
      animate_switch (get_back_sprite b) (get_back_sprite c) true
        (get_nickname b) (get_nickname c) (refresh_battle 1);
      Combat.switch_player ~!bs c (Player.party (State.player ()));
      Combat.switching_pending := None;
      handle_combat None;
      cmd_pos.x <- 0;
      cmd_pos.y <- 0;
      update_player p_hud_stats ~!bs.player_battler.creature
  | None -> refresh_battle 1 ()

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
      refresh_battle 2 ();
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
    match Input.pop_key_option () with
    | Some Sdlkeycode.F ->
        display_text_box
          "Hey guys, did you know that in terms of male human and \
           female Pokémon breeding, Vaporeon is the most compatible \
           Pokémon for humans? Not only are they in the field egg \
           group, which is mostly comprised of mammals, Vaporeon are \
           an average of 3”03’ tall and 63.9 pounds, this means \
           they’re large enough to be able handle human dicks, and \
           with their impressive Base Stats for HP and access to Acid \
           Armor, you can be rough with one. Due to their mostly water \
           based biology, there’s no doubt in my mind that an aroused \
           Vaporeon would be incredibly wet, so wet that you could \
           easily have sex with one for hours without getting sore. \
           They can also learn the moves Attract, Baby-Doll Eyes, \
           Captivate, Charm, and Tail Whip, along with not having fur \
           to hide nipples, so it’d be incredibly easy for one to get \
           you in the mood. With their abilities Water Absorb and \
           Hydration, they can easily recover from fatigue with enough \
           water. No other Pokémon comes close to this level of \
           compatibility. Also, fun fact, if you pull out enough, you \
           can make your Vaporeon turn white. Vaporeon is literally \
           built for human dick. Ungodly defense stat+high HP \
           pool+Acid Armor means it can take cock all day, all shapes \
           and sizes and still come for more"
          false (refresh_battle 2) ();
        NoKey
    | Some c -> get_ctrl_key c
    | None -> NoKey
  in
  (*===== Selecting a command ===*)
  (match key with
  | Right -> move_x 1 ()
  | Left -> move_x (-1) ()
  | Up -> move_y (-1) ()
  | Down -> move_y 1 ()
  | Debug ->
      Ui.add_last_foreground (fun () ->
          Draw.draw_sprite_crop
            (get_front_sprite ~!bs.player_battler.creature)
            300 300 (0, 0) (50, 50) ();
          wait (-1) ());

      Ui.add_last_foreground (fun () ->
          Draw.draw_sprite_crop
            (get_front_sprite ~!bs.player_battler.creature)
            300 300 (0, 0) (100, 100) ();
          wait (-1) ());
      Ui.add_last_foreground (fun () ->
          Draw.draw_sprite_crop
            (get_front_sprite ~!bs.player_battler.creature)
            300 300 (0, 0) (200, 200) ();
          wait (-1) ())
  | _ -> ());

  (match !combat_mode with
  | Commands -> (
      DrawText.set_text_display "";
      if key = Action then
        match selected_command () with
        | Fight ->
            combat_mode := Moves;
            ignore (Input.poll_key_option ())
        | Bag -> handle_inventory ()
        | Party -> handle_party ()
        | Run ->
            Combat.run_away ~!bs;
            if ~!bs.battle_status = Combat.Flee then begin
              Animation.display_text_box "You ran away!" false
                (refresh_battle 2) ();

              combat_mode := End_Battle
            end
            else begin
              Animation.display_text_box "You could not run away!" false
                (refresh_battle 2) ();
              Ui.update_all ();
              handle_combat None
            end)
  | Moves ->
      if key = Action then begin
        let move =
          (get_move_i ~!bs.player_battler.creature)
            (moves_position.x + (2 * moves_position.y))
        in
        combat_mode := Attack;
        handle_combat move
      end;
      if key = Back then combat_mode := Commands
  | Attack ->
      let b = ~!bs.player_battler.creature in
      if get_status ~!bs.player_battler.creature = Fainted then begin
        Party_menu.init FaintedSwitch ();
        match !Combat.switching_pending with
        | Some c ->
            animate_switch (get_back_sprite b) (get_back_sprite c) true
              (get_nickname b) (get_nickname c) (refresh_battle 1);
            Combat.switch_player ~!bs c (Player.party (State.player ()));
            combat_mode := Commands;
            Combat.switching_pending := None;
            cmd_pos.x <- 0;
            cmd_pos.y <- 0;
            update_player p_hud_stats ~!bs.player_battler.creature
        | None -> refresh_battle 2 ()
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
      wait 240 ()
  | Exit -> ());

  refresh_battle 2 ();
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
  update_player p_hud_stats ~!bs.player_battler.creature;
  update_player e_hud_stats ~!bs.enemy_battler.creature;

  Player.set_party
    (battle_party [] (Player.party (State.player ())))
    (State.player ());

  (* ========= Draw the Battle ========= *)
  run_tick ()
