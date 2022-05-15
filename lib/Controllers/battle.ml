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

let battle_right = Util.null ()
let moves_window = Util.null ()
let combat_hud = Util.null ()
let player_hud = Util.null ()
let battle_bg1 = Util.null ()
let level_up_screen = Util.null ()
let ball_anim = Util.null ()

let load_assets _ =
  battle_right *= Sprite_assets.get_sprite2 "battle_top" GUI_Folder;
  moves_window *= Sprite_assets.get_sprite2 "moves_window" GUI_Folder;
  combat_hud *= Sprite_assets.get_sprite2 "opponent_hud" GUI_Folder;
  player_hud *= Sprite_assets.get_sprite2 "player_hud" GUI_Folder;
  battle_bg1 *= Sprite_assets.get_sprite2 "battle-bg1" GUI_Folder;
  level_up_screen *= Sprite_assets.get_sprite2 "level_up" GUI_Folder;
  ball_anim
  *= Sprite_assets.get_spritesheet
       "assets/item_sprites/pokeball_capture.png"

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
          draw_string_colored x y Medium m.move_name
            (get_color_from_etype m.etype)
            text_color ();
          draw_string_colored x (y - 40) Small
            (string_of_etype m.etype)
            (get_color_from_etype m.etype)
            text_color ();

          draw_string_colored (x + 110) (y - 40) Small
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
  let hwidth = 256 in
  let hheight = 6 in
  let xh, yh = (width - hwidth - 30, 244) in
  draw_exp_bar ~!p_hud_stats.max_exp ~!p_hud_stats.curr_exp xh yh hwidth
    hheight ()

let draw_health_bar_combat (max : float) (curr : float) player () =
  let hwidth = 210 in
  let hheight = 6 in
  let xh, yh =
    if player then (width - hwidth - 38, 302) else (130, 612)
  in
  draw_health_bar max curr xh yh hwidth hheight player ()

let draw_combat_hud sprite name level player () =
  let sprite_width, sprite_height = get_dimension sprite in
  if player then begin
    draw_sprite sprite
      (width - sprite_width - 14)
      (360 - sprite_height) ();
    draw_string_colored (width - 320) 312 Medium
      (String.uppercase_ascii name)
      white text_color ();
    draw_string_colored (width - 100) 312 Medium
      ("Lv" ^ string_of_int level)
      white text_color ();
    draw_health_bar_combat ~!p_hud_stats.max_hp ~!p_hud_stats.curr_hp
      player ();
    draw_exp_bar_combat ()
  end
  else begin
    draw_sprite sprite 42 (height - 49 - sprite_height) ();
    draw_string_colored 60 (height - 97) Medium
      (String.uppercase_ascii name)
      white text_color ();
    draw_string_colored 280 (height - 101) Medium
      ("Lv" ^ string_of_int level)
      white text_color ();
    draw_health_bar_combat ~!e_hud_stats.max_hp ~!e_hud_stats.curr_hp
      player ()
  end

let draw_combat_commands () =
  let x, y = (465, 108) in
  let hdif = 80 in
  set_color text_color;
  clear_text ~!battle_right ();
  draw_string_colored x y Large "FIGHT" white text_color ();
  draw_string_colored x (y - hdif) Large "BAG" white text_color ();
  draw_string_colored (x + 175) y Large "PARTY" white text_color ();
  draw_string_colored (x + 175) (y - hdif) Large "RUN" white text_color
    ();
  draw_string_colored
    (x - 35 + (175 * cmd_pos.x))
    (y - (hdif * cmd_pos.y))
    Huge ">" white text_color ();

  draw_text_string_pos 35 132 Medium 20
    ("What will " ^ get_nickname ~!bs.player_battler.creature ^ " do?")
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

let draw_level_up creature frame () =
  let x, y, dif, x2 =
    (width - 300 + 30, 200 + 250 - 8, 35, width - 140)
  in
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
    let s = get_stats creature in
    let old_stats =
      {
        max_hp = s.max_hp;
        attack = s.attack;
        defense = s.defense;
        sp_attack = s.sp_attack;
        sp_defense = s.sp_defense;
        speed = s.speed;
      }
    in
    level_up creature ();
    let new_stats = get_stats creature in

    draw_sprite ~!level_up_screen (width - 300 + 8) 210 ();
    draw_string_colored x y Medium
      ("Level " ^ string_of_int (get_level creature))
      white text_color ();
    for i = 0 to 5 do
      let s = List.nth stat_lst i in

      draw_string_colored x
        (y - (dif * (i + 1)))
        Medium
        (string_of_stat_short s)
        white text_color ();

      draw_string_colored x2
        (y - (dif * (i + 1)))
        Medium
        (string_of_intf (get_stat2 old_stats s))
        white text_color ();
      draw_string_colored (x2 + 80)
        (y - (dif * (i + 1)))
        Medium
        ("+"
        ^ string_of_int
            (int_of_float (get_stat2 new_stats s)
            - int_of_float (get_stat2 old_stats s)))
        white text_color ()
    done
  end
  else if frame = 1 then begin
    let new_stats = get_stats creature in
    draw_sprite ~!level_up_screen (width - 300 + 8) 210 ();
    draw_string_colored x y Medium
      ("Level " ^ string_of_int (get_level creature))
      white text_color ();
    for i = 0 to 5 do
      let s = List.nth stat_lst i in

      draw_string_colored x
        (y - (dif * (i + 1)))
        Medium
        (string_of_stat_short s)
        white text_color ();

      draw_string_colored x2
        (y - (dif * (i + 1)))
        Medium
        (string_of_intf (get_stat2 new_stats s))
        white text_color ()
    done
  end

let refresh_battle state () =
  Ui.add_last_background (draw_sprite ~!battle_bg1 0 (-3));

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
  let hwidth = 256 in
  let hheight = 6 in
  let xh, yh = (width - hwidth - 30, 244) in
  animate_exp_bar max bef aft xh yh hwidth hheight (refresh_battle 2)

let animate_health_bar_combat max bef aft player () =
  let hwidth = 210 in
  let hheight = 6 in
  let xh, yh =
    if player then (width - hwidth - 38, 302) else (130, 612)
  in
  animate_health_bar max bef aft xh yh hwidth hheight player
    (refresh_battle 2)

let moves_ptr = Util.new_point ()

let draw_replace_moves creature () =
  let x, y, dif = (width - 300 + 30, 200 + 250, 40) in
  draw_sprite ~!level_up_screen (width - 300) 210 ();
  draw_string_colored x y Medium "Moves " white text_color ();

  draw_string_colored x
    (y - (dif * (moves_ptr.y + 1)))
    Medium ">" white text_color ();
  for i = 0 to 3 do
    let m = !!(get_move_i creature i) in

    draw_string_colored (x + 30)
      (y - (dif * (i + 1)))
      Medium m.move_name white text_color ()
  done

let handle_learn_move creature =
  let x = try Some (level_up_move creature) with Not_found -> None in

  for i = 0 to 3 do
    match get_move_i creature i with
    | Some m -> print_endline ("Move: " ^ m.move_name)
    | None -> print_endline "-NONE-"
  done;

  match x with
  | Some m ->
      if num_moves creature < 4 then begin
        Animation.display_text_box
          (get_nickname creature ^ " learned " ^ m.move_name ^ "!")
          false (refresh_battle 2) ();
        add_move creature m
      end
      else begin
        Animation.display_text_box
          (get_nickname creature ^ " wants to learn " ^ m.move_name
         ^ ". But " ^ get_nickname creature ^ " already knows 4 moves!"
          )
          false (refresh_battle 2) ();

        Animation.display_text_box
          ("Which move should " ^ get_nickname creature ^ " forget?")
          true (refresh_battle 2) ();

        let rec learn_move_event () =
          Input.sleep Draw.tick_rate ();
          let key =
            match Input.pop_key_option () with
            | Some c -> get_ctrl_key c
            | None -> NoKey
          in

          if key = Up then
            if moves_ptr.y > 0 then moves_ptr.y <- moves_ptr.y - 1;
          if key = Down then
            if moves_ptr.y < 3 then moves_ptr.y <- moves_ptr.y + 1;

          refresh_battle 2 ();
          Ui.add_last_foreground (draw_replace_moves creature);
          Ui.update_all ();

          if key = Action || key = Back then
            match key with
            | Action ->
                Animation.display_text_box "1, 2, and ... Poof!" false
                  (refresh_battle 2) ();
                Animation.display_text_box
                  (get_nickname creature ^ " forgot "
                 ^ !!(get_move_i creature moves_ptr.y).move_name
                 ^ " and learned " ^ m.move_name ^ "!")
                  false (refresh_battle 2) ();
                add_move_i creature m moves_ptr.y
            | Back ->
                Animation.display_text_box
                  (get_nickname creature ^ " did not learn "
                 ^ m.move_name ^ ".")
                  false (refresh_battle 2) ()
            | _ -> ()
          else learn_move_event ()
        in
        learn_move_event ()
      end
  | None -> print_endline "Sad:("

let handle_exp player_creature enemy_creature () =
  let exp_gain =
    get_exp_gain enemy_creature
    /. float_of_int
         (List.length
            (List.filter
               (fun c -> get_status c <> Fainted)
               ~!bs.creatures_switched))
  in

  let exp_event target mc =
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

            if mc then begin
              animate_exp_bar_combat max bef aft ();
              ~!p_hud_stats.curr_exp <- aft;
              ~!p_hud_stats.max_exp <- max
            end;

            if level <> lvl then begin
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
              wait (-1) ();
              handle_learn_move target
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
            display_text_box "It was a critical hit!" false
              (refresh_battle 2) ()
      | Heal _ -> ()
      | StatusGain (gained, cstatus) ->
          if affected.active then begin
            (if gained then
             match cstatus with
             | Status s ->
                 animate_status sprite is_player s (refresh_battle 2)
             | _ -> ());
            display_text_box text false (refresh_battle 2) ()
          end
      | StatusEffect (cstatus, max, bef, aft) ->
          if affected.active then begin
            (match cstatus with
            | Status s ->
                animate_status sprite is_player s (refresh_battle 2)
            | _ -> ());
            display_text_box text true (refresh_battle 2) ();
            animate_health_bar_combat max bef aft is_player ();
            ~!hud.curr_hp <- aft
          end
      | MaxStat -> display_text_box text false (refresh_battle 2) ()
      | StatGain stages ->
          if stages > 0 then
            animate_raise_stat_effect sprite is_player
              (refresh_battle 2)
          else if stages < 0 then
            animate_lower_stat_effect sprite is_player
              (refresh_battle 2);
          display_text_box text false (refresh_battle 2) ()
      | Switch _ -> ()
      | Fainted ->
          affected.active <- false;
          animate_faint sprite is_player (refresh_battle 2);
          display_text_box text false (refresh_battle 2) ()
    done;
    Combat.battle_actions := [];
    (***============= Resolution =============***)
    if ~!bs.enemy_battler.active = false then (
      combat_mode := End_Battle;
      ~!bs.enemy_battler.active <- false;
      Ui.add_last_gameplay (Input.sleep 0.5);

      handle_exp player enemy ());
    if ~!bs.player_battler.active = false then combat_mode := Attack
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

      let catch_results = Combat.capture ~!bs modifier in

      Animation.animate_capture ~!ball_anim
        (get_front_sprite ~!bs.enemy_battler.creature)
        catch_results ball_type (refresh_battle 0);

      if ~!bs.battle_status = Catch then begin
        display_text_box
          ("You captured "
          ^ get_nickname ~!bs.enemy_battler.creature
          ^ "!")
          false
          (fun () ->
            refresh_battle 0 ();
            Ui.add_last_gameplay
              (draw_sprite
                 (Spritesheet.get_sprite ~!ball_anim
                    (((5 + 27) * (~!ball_anim.columns - 1)) + ball_type))
                 540 396))
          ();
        ~!bs.enemy_battler.active <- false;

        handle_exp ~!bs.player_battler.creature
          ~!bs.enemy_battler.creature ();

        captured_creature *= ~!bs.enemy_battler.creature;

        combat_mode := End_Battle
      end
      else
        display_text_box "Aw... So close!" false (refresh_battle 2) ();
      true
  | Item.Key -> false

let switch_creatures a b player =
  let sprite_a, sprite_b =
    if player then (get_back_sprite a, get_back_sprite b)
    else (get_front_sprite a, get_front_sprite b)
  in
  let name_a, name_b = (get_nickname a, get_nickname b) in

  display_text_box
    ("Come back " ^ name_a ^ "!")
    true (refresh_battle 2) ();
  animate_switch_out sprite_a player (refresh_battle 1);

  display_text_box ("Go " ^ name_b ^ "!") true (refresh_battle 1) ();
  update_player p_hud_stats b;
  animate_switch_in sprite_b player (refresh_battle 1)

let handle_party () =
  Party_menu.init BattleSwitch ();
  match !Combat.switching_pending with
  | Some c ->
      combat_mode := Attack;
      let b = ~!bs.player_battler.creature in
      switch_creatures b c true;
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
        update_player p_hud_stats ~!bs.player_battler.creature;
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
            combat_mode := End_Battle;
            if ~!bs.battle_status = Combat.Flee then
              Animation.display_text_box "You ran away!" false
                (refresh_battle 2) ()
            else begin
              combat_mode := Attack;
              Animation.display_text_box "You could not run away!" false
                (refresh_battle 2) ();

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
            switch_creatures b c true;
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
          Player.add_creature c (State.player ());
          captured_creature := None);
      combat_mode := Exit;

      Combat.reset_battler ~!bs.player_battler;
      Combat.reset_battler ~!bs.enemy_battler;

      wait 240 ();
      for i = 0 to List.length (Player.party (State.player ())) - 1 do
        let c = Player.party_i (State.player ()) i in
        if can_evolve c then Event_menu.init_evolution c ()
      done
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
