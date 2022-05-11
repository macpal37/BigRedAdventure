open Draw
open Creature
open Animation
open Util
open DrawText
open Sdlkeycode

(* let max_creatures = 5 let max_creatures = ref 0 *)
let party_menu_bg = load_sprite "party_menu" GUI_Folder 3 ()
let active = load_sprite "active_party_creature" GUI_Folder 3 ()
let minimenu1 = load_sprite "party_minimenu" GUI_Folder 3 ()
let faint = load_sprite "faint" GUI_Folder 3 ()
let menu_position = Util.new_point ()
let switch_position = Util.new_point ()
let minimenu_position = Util.new_point ()
let current_item = null ()
let set_current_item i = current_item := Some i
let get_current_item = !current_item

type combat_mode =
  | OverworldMode
  | BattleSwitch
  | InventoryMode
  | FaintedSwitch

let battle_mode = ref OverworldMode

(* let itemmenu_position = Util.new_point () *)
type menu =
  | MainMenu
  | MiniMenu
  | SwitchMode
  | Exit

let menu_mode = ref MainMenu

let move_x x () =
  match !menu_mode with
  | MainMenu ->
      if
        menu_position.x + x >= 0
        && menu_position.x + x < 2
        && List.length (Player.party (State.player ())) > 1
      then menu_position.x <- menu_position.x + x
  | SwitchMode ->
      if
        switch_position.x + x >= 0
        && switch_position.x + x < 2
        && List.length (Player.party (State.player ())) > 1
      then switch_position.x <- switch_position.x + x
  | _ -> ()

let move_y y () =
  match !menu_mode with
  | MainMenu ->
      if
        menu_position.y + y >= 0
        && menu_position.y + y
           < List.length (Player.party (State.player ())) - 1
      then menu_position.y <- menu_position.y + y
  | MiniMenu ->
      if minimenu_position.y + y >= 0 && minimenu_position.y + y < 4
      then minimenu_position.y <- minimenu_position.y + y
  | SwitchMode ->
      if
        switch_position.y + y >= 0
        && switch_position.y + y
           < List.length (Player.party (State.player ())) - 1
      then switch_position.y <- switch_position.y + y
  | _ -> ()

let draw_status x y status () =
  match status with
  | Fainted -> draw_sprite faint x y ()
  | _ -> ()

let draw_menu lead_creature () =
  Ui.add_first_background (draw_sprite party_menu_bg 0 0);

  Ui.add_first_foreground
    (draw_string_colored 24 605 1 "PARTY" (rgb 255 170 40) white);
  Ui.add_first_gameplay
    (draw_sprite (get_front_sprite lead_creature) 9 318);
  Ui.add_first_gameplay
    (draw_string_colored 20 246 0
       (get_nickname lead_creature)
       white text_color);
  Ui.add_first_gameplay
    (draw_string_colored 20 220 0
       ("LVL: " ^ string_of_int (get_level lead_creature))
       white text_color);
  let max, curr = get_hp_status lead_creature in
  Ui.add_first_gameplay (draw_health_bar max curr 56 192 180 6 true);
  Ui.add_first_gameplay
    (draw_status (56 - 20) (192 - 30) (get_status lead_creature))

let draw_creature_status creature pos () =
  let x, y = (276, 577 - (112 * pos)) in
  let xx = 294 + 80 in
  let yy = 610 - (112 * pos) in
  Ui.add_first_background
    (draw_sprite
       (change_dpi (get_front_sprite creature) 1)
       (x + 11) (y + 11));
  Ui.add_first_background (draw_sprite active x y);
  Ui.add_first_gameplay
    (draw_string_colored xx (yy + 26) 0 (get_nickname creature) white
       text_color);
  Ui.add_first_gameplay
    (draw_string_colored xx yy 0
       ("LVL: " ^ string_of_int (get_level creature))
       white text_color);
  let max, curr = get_hp_status creature in
  Ui.add_first_gameplay
    (draw_health_bar max curr (xx + 130) (yy + 6) 250 6 true);
  Ui.add_first_gameplay
    (draw_status (xx + 120) (yy - 24) (get_status creature))

let draw_selector () =
  set_line_width 6;

  (if menu_position.x = 1 then begin
   let x, y = (276, 577 - (112 * menu_position.y)) in
   set_color red;
   draw_rect x y 510 100
 end
  else
    let x, y = (9, 140) in
    set_color red;
    draw_rect x y 254 152);
  if !menu_mode = SwitchMode then begin
    if switch_position.x = 1 then begin
      let x, y = (276, 577 - (112 * switch_position.y)) in
      set_color blue;
      draw_rect x y 510 100
    end
    else
      let x, y = (9, 140) in
      set_color blue;
      draw_rect x y 254 152
  end

let refresh () =
  let length = List.length (Player.party (State.player ())) in
  if length > 1 then
    for i = 0 to length - 2 do
      draw_creature_status
        (Player.party_i (State.player ()) (i + 1))
        i ()
    done;
  Ui.add_first_foreground draw_selector;
  draw_menu (Player.party_i (State.player ()) 0) ();
  if !battle_mode <> InventoryMode then
    Ui.add_first_foreground
      (draw_string_colored 40 36 1 "Choose a CREATURE" text_color2
         text_color)

let use_item creature item =
  print_endline "Hello?";
  let id = Item.get_id item in
  try
    (match id with
    | 1 -> Creature.add_hp creature 20.
    | 2 -> Creature.add_hp creature 50.
    | 3 -> Creature.add_hp creature 120.
    | 4 -> Creature.add_hp creature (get_stat creature HP)
    | 7 ->
        Creature.remove_status creature Fainted;
        Creature.add_hp creature (get_stat creature HP /. 2.)
    | 8 ->
        Creature.remove_status creature Fainted;
        Creature.add_hp creature (get_stat creature HP)
    | _ -> print_endline "NICE ITEM BRO :)");
    current_item := None;
    menu_mode := Exit
  with NoEffect ->
    refresh ();
    Ui.add_first_foreground
      (draw_string_colored 40 36 1 "It has no effect" text_color2
         text_color);
    ();
    menu_mode := MainMenu

let minimenu () =
  let x, y, dif, f = (630, 166, 44, 40) in
  Ui.add_first_foreground
    (draw_string_colored (x - 30)
       (y - 5 - (dif * minimenu_position.y))
       50 ">" text_color2 text_color);
  Ui.add_first_foreground
    (draw_string_colored x
       (y - (dif * 0))
       f "Summary" text_color2 text_color);
  Ui.add_first_foreground
    (draw_string_colored x
       (y - (dif * 1))
       f "Switch" text_color2 text_color);
  Ui.add_first_foreground
    (draw_string_colored x
       (y - (dif * 2))
       f "Item" text_color2 text_color);
  Ui.add_first_foreground
    (draw_string_colored x
       (y - (dif * 3))
       f "Back" text_color2 text_color);
  Ui.add_first_foreground (draw_sprite minimenu1 576 12)

let get_party_index () =
  if menu_position.x = 0 then 0 else menu_position.y + 1

let switch () =
  let a, b =
    ( Player.party_i (State.player ())
        (if menu_position.x = 0 then 0 else menu_position.y + 1),
      Player.party_i (State.player ())
        (if switch_position.x = 0 then 0 else switch_position.y + 1) )
  in
  let party = Player.party (State.player ()) in
  let rec switch_creature new_lst = function
    | [] -> new_lst
    | h :: t ->
        if a = h then switch_creature (new_lst @ [ b ]) t
        else if b = h then switch_creature (new_lst @ [ a ]) t
        else switch_creature (new_lst @ [ h ]) t
  in
  let new_party = switch_creature [] party in
  Player.set_party new_party (State.player ())

let rec run_tick () =
  Input.sleep Draw.tick_rate ();
  let key =
    match Input.pop_key_option () with
    | Some c -> c
    | None -> Unknown
  in

  (*====== Move Selector ====== *)
  if key = W || key = Up then move_y (-1) ();
  if key = S || key = Down then move_y 1 ();
  if key = A || key = Left then move_x (-1) ();
  if key = D || key = Right then move_x 1 ();

  (match !menu_mode with
  | MainMenu -> (
      match !battle_mode with
      | BattleSwitch | OverworldMode ->
          if key = E || key = Z then begin
            minimenu_position.x <- 0;
            minimenu ();
            menu_mode := MiniMenu
          end;
          if key = Q || key = X then menu_mode := Exit
      | InventoryMode ->
          (if key = E || key = Z then
           let target_creature =
             Player.party_i (State.player ())
               (if menu_position.x = 0 then 0 else menu_position.y + 1)
           in
           match !current_item with
           | Some i -> use_item target_creature i
           | None -> ());
          if key = Q || key = X then menu_mode := Exit
      | FaintedSwitch ->
          if key = E || key = Z then begin
            minimenu_position.x <- 0;
            minimenu ();
            menu_mode := MiniMenu
          end)
  | MiniMenu ->
      if
        key = A || key = S || key = A || key = D || key = Up
        || key = Down || key = Right || key = Left
      then minimenu ();
      if key = Q || key = X then begin
        minimenu_position.y <- 0;
        menu_mode := MainMenu;
        refresh ()
      end;
      if (key = E || key = Z) && minimenu_position.y = 0 then begin
        Creature_menu.set_creature (get_party_index ());
        Creature_menu.init ();

        refresh ()
      end;
      minimenu ();
      (if (key = E || key = Z) && minimenu_position.y = 1 then
       match !battle_mode with
       | OverworldMode ->
           switch_position.x <- menu_position.x + 0;
           switch_position.y <- menu_position.y + 0;
           refresh ();
           menu_mode := SwitchMode
       | BattleSwitch | FaintedSwitch ->
           if menu_position.x <> 0 then
             let a, b =
               ( Player.party_i (State.player ()) 0,
                 Player.party_i (State.player ())
                   (if menu_position.x = 0 then 0
                   else menu_position.y + 1) )
             in
             if get_status b <> Fainted then begin
               let party = Player.party (State.player ()) in
               let rec switch_creature new_lst = function
                 | [] -> new_lst
                 | h :: t ->
                     if a = h then switch_creature (new_lst @ [ b ]) t
                     else if b = h then
                       switch_creature (new_lst @ [ a ]) t
                     else switch_creature (new_lst @ [ h ]) t
               in
               let new_party = switch_creature [] party in
               Player.set_party new_party (State.player ());
               Combat.switching_pending := Some b;

               menu_mode := Exit
             end
       | _ -> ());
      if (key = E || key = Z) && minimenu_position.y = 3 then
        menu_mode := MainMenu
  | SwitchMode ->
      if key = E || key = Z then begin
        switch ();
        menu_position.x <- switch_position.x + 0;
        menu_position.y <- switch_position.y + 0;
        minimenu_position.x <- 1;
        switch_position.x <- 0;
        switch_position.y <- 0;
        minimenu ();

        menu_mode := MiniMenu
      end;
      if key = Q || key = X then begin
        menu_mode := MiniMenu;
        menu_position.x <- switch_position.x + 0;
        menu_position.y <- switch_position.y + 0;
        minimenu_position.x <- 1;
        switch_position.x <- 0;
        switch_position.y <- 0;
        minimenu ()
      end
  | Exit -> ());

  refresh ();
  (*====== MiniMenu ====== *)
  Ui.update_all ();
  if !menu_mode <> Exit then run_tick ()

let init bm () =
  Ui.clear_all ();
  menu_mode := MainMenu;
  Combat.switching_pending := None;
  battle_mode := bm;
  minimenu_position.x <- 0;
  minimenu_position.y <- 0;
  switch_position.x <- 0;
  switch_position.y <- 0;
  refresh ();
  run_tick ()
