open Draw
open Graphics
open Creature
open Animation

let text_box_height = 212
let combat_button = ref 0

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

let battle_bot_right = load_sprite "battle_bot_right" GUI_Folder 3 ()
let battle_bot_left = load_sprite "battle_bot_left" GUI_Folder 3 ()
let battle_right = load_sprite "battle_top" GUI_Folder 3 ()
let moves_window = load_sprite "moves_window" GUI_Folder 3 ()
let combat_hud = load_sprite "opponent_hud" GUI_Folder 3 ()
let player_hud = load_sprite "player_hud2" GUI_Folder 3 ()
let battle_bg1 = load_sprite "battle-bg1" GUI_Folder 3 ()

(* WIll be improved next sprint *)
let enemy_active = ref true
let enemy_creature = ref (create_creature "stregoom" 40)

(*****************************************************************)
(***************     Combat Drawing Commands     *********************)
(*****************************************************************)
let start_combat_hud () =
  set_text_bg battle_bot_left battle_right;
  clear_text ();
  set_text_bg battle_bot_left empty_sprite;
  Ui.add_first_foreground
    (draw_text_string_pos 35 132 40 14 "What will" white);
  set_text_bg empty_sprite battle_right

let draw_moves creature c_b c_a () =
  let moves = get_moves creature in
  let size = List.length moves in
  let box_w, box_h = (378, 92) in
  let box_x, box_y = (20, 200 - box_h) in
  let text_x, text_y = (30, (4 * text_box_height / 5) - 12) in
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
      white ();

    draw_string_colored (width - 100) 316 2 30
      ("Lv" ^ string_of_int level)
      white ()
  end
  else begin
    set_font_size 30 ();
    draw_sprite sprite 42 (height - 45 - sprite_height) ();
    draw_string_colored 60 (height - 85) 2 30
      (String.uppercase_ascii name)
      white ();

    draw_string_colored 280 (height - 85) 2 30
      ("Lv" ^ string_of_int level)
      white ()
  end;

  draw_health_bar_combat max before after player ();
  set_font_size 40 ()

let draw_combat_commands c redraw () =
  set_font_size 50 ();
  let x, y = (475, 120) in
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

  set_font_size 40 ()

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

    clear_text ();
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
    ( List.nth battle_sim.contents.player_creatures 0,
      List.nth battle_sim.contents.enemy_creatures 0 )
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
  Ui.add_last_foreground (set_sticky_text false);

  Ui.add_first_foreground
    (draw_text
       (get_nickname player_creature
       ^ " gained "
       ^ string_of_int (get_exp_gain player_creature)
       ^ " EXP. Points!")
       40 true)

let handle_combat move =
  (* Ui.clear_ui Gameplay; Ui.clear_ui Foreground; *)
  if battle_sim.contents.battle_status = Ongoing then begin
    Combat.turn_builder battle_sim.contents move;
    set_text_char_cap 28;
    let player, enemy =
      ( List.nth battle_sim.contents.player_creatures 0,
        List.nth battle_sim.contents.enemy_creatures 0 )
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

      set_text_char_cap 28;
      set_text_bg battle_bot_left battle_bot_right;
      if battle_sim.contents.battle_status = Catch then begin
        print_endline "Success";
        handle_exp battle_sim.contents.player_battler.creature
          enemy_creature.contents ();
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
      end;
      (* Ui.add_first_foreground (fun () -> Graphics.auto_synchronize
         false); *)
      Ui.add_first_foreground (fun () -> Graphics.auto_synchronize true)
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
  Ui.update_all ();
  draw_creature_exp battle_sim.contents.player_battler.creature 0 true
    ();
  Ui.add_first_foreground start_combat_hud

let rec run_tick () =
  Input.poll ();
  let key =
    match Input.key_option () with
    | Some c -> c
    | None -> '#'
  in

  let b = !combat_button in
  if key = 'd' && (b = 0 || b = 2) then combat_button.contents <- b + 1;
  if key = 'a' && (b = 1 || b = 3) then combat_button.contents <- b - 1;
  if key = 'w' && (b = 2 || b = 3) then combat_button.contents <- b - 2;
  if key = 's' && (b = 0 || b = 1) then combat_button.contents <- b + 2;
  let action =
    match combat_mode.contents with
    | Commands ->
        if key <> 'e' then
          if b != combat_button.contents then
            Ui.add_first_foreground
              (draw_combat_commands combat_button.contents true)
          else
            Ui.add_first_foreground
              (draw_combat_commands combat_button.contents false);
        if
          key = 'e'
          && combat_button.contents = 0
          && battle_sim.contents.battle_status <> Combat.Victory
        then begin
          set_text_bg moves_window empty_sprite;
          combat_mode.contents <- Moves;
          Ui.add_first_gameplay clear_text;
          Ui.add_first_foreground
            (draw_moves battle_sim.contents.player_battler.creature b
               combat_button.contents)
        end
        else if key = 'e' && combat_button.contents = 1 then begin
          Inventory_menu.init ();

          (match Inventory_menu.selected_item.contents with
          | Some i -> handle_item i ()
          | None -> print_endline "No Item :(");
          refresh_battle ();
          Ui.update_all ();
          handle_combat Creature.empty_move
        end
        else if key = 'e' && combat_button.contents = 2 then begin
          Party_menu.init ();
          refresh_battle ()
        end
        else if key = 'e' && combat_button.contents = 3 then begin
          Combat.run_away battle_sim.contents;
          if battle_sim.contents.battle_status = Combat.Flee then begin
            Ui.add_first_foreground (draw_text "You ran away!" 40 true);
            Ui.add_first_foreground (fun () ->
                Graphics.auto_synchronize true);
            combat_mode.contents <- End_Battle
          end
          else begin
            Ui.add_first_foreground
              (draw_text "You could not run away!" 40 true);
            Ui.add_first_foreground (fun () ->
                Graphics.auto_synchronize true);
            handle_combat Creature.empty_move
          end
        end;
        if key = 'q' then combat_mode.contents <- End_Battle
    | Moves ->
        if b != combat_button.contents then
          Ui.add_first_foreground
            (draw_moves battle_sim.contents.player_battler.creature b
               combat_button.contents);

        if key = 'e' then begin
          (* Ui.clear_ui Ui.Foreground; *)
          set_text_bg battle_bot_left battle_bot_right;
          Ui.add_first_foreground clear_text;
          let move =
            List.nth
              (get_moves battle_sim.contents.player_battler.creature)
              combat_button.contents
          in
          Ui.update_all ();
          handle_combat move;
          combat_mode.contents <- Attack
        end;
        if key = 'q' then begin
          Ui.clear_ui Ui.Foreground;
          Ui.add_first_foreground start_combat_hud;
          combat_mode.contents <- Commands;
          Ui.add_last_background clear_text
        end
    | Attack ->
        set_text_bg battle_bot_left battle_right;
        Ui.add_first_gameplay clear_text;
        combat_mode.contents <- Commands;
        combat_button.contents <- 0
    | End_Battle -> ()
  in

  action;

  Ui.update_all ();
  Unix.sleepf 0.016;
  if key = 'q' || combat_mode.contents <> End_Battle then run_tick ()

let start_battle () =
  enemy_active.contents <- true;
  enemy_creature.contents <-
    (fun () -> create_creature "rafu" (Random.int 10 + 35)) ();
  set_synced_mode false;
  clear_screen ();
  combat_mode.contents <- Commands;
  set_text_bg battle_bot_left battle_right;
  (* ========= Start the Battle ========= *)
  battle_sim.contents <-
    Combat.wild_init
      (Player.party (State.player ()))
      [ enemy_creature.contents ];
  (* ========= Draw the Battle ========= *)
  refresh_battle ();
  run_tick ()
