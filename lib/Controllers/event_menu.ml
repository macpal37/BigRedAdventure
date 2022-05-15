open Draw
open Creature
open Util
open DrawText
open Input

let event_menu = Util.null ()
let decision_menu = Util.null ()
let nameholder = Util.null ()
let minimenu_position = Util.new_point ()
let target_creature = null ()
let nickname = ref ""

let load_assets _ =
  event_menu *= Sprite_assets.get_sprite2 "event_menu" GUI_Folder;
  decision_menu *= Sprite_assets.get_sprite2 "decision_menu" GUI_Folder;
  nameholder *= Sprite_assets.get_sprite2 "nameholder" GUI_Folder

type menu =
  | Captured_Creature
  | Nicknaming
  | Evolution
  | Exit

let menu_mode = ref Captured_Creature

let refresh () =
  Ui.add_first_background (draw_sprite ~!event_menu 0 0);

  match !target_creature with
  | Some creature ->
      Ui.add_first_gameplay
        (draw_sprite (get_front_sprite creature) 240 280)
  | None -> ()

let draw_decision () =
  let x, y = (800 - 105, 216) in

  draw_sprite ~!decision_menu (x - 30) (y + 3) ();
  draw_string_colored x
    (y + 60 - (40 * minimenu_position.y))
    0 ">" white text_color ();
  draw_string_colored (x + 12) (y + 60) 0 "Yes" white text_color ();
  draw_string_colored (x + 12) (y + 20) 0 "No" white text_color ()

let draw_nickname () =
  draw_sprite ~!nameholder 240 (280 - 70) ();
  draw_string_colored (240 + 12) (280 - 62) 0 !nickname white text_color
    ()

let rec run_tick () =
  Input.sleep Draw.tick_rate ();
  let key =
    match Input.pop_key_option () with
    | Some c -> c
    | None -> Sdlkeycode.Unknown
  in

  (match !menu_mode with
  | Captured_Creature ->
      if key => Up then minimenu_position.y <- minimenu_position.y - 1;
      if key => Down then minimenu_position.y <- minimenu_position.y + 1;
      minimenu_position.y <- Util.bound minimenu_position.y 0 1;
      Ui.add_first_foreground draw_decision;

      if key => Action && minimenu_position.y = 0 then
        menu_mode := Nicknaming;

      if key => Back || (key => Action && minimenu_position.y = 1) then begin
        (if List.length (Player.party (State.player ())) = 6 then
         match !target_creature with
         | Some creature ->
             Animation.display_text_box
               (get_nickname creature ^ " was sent to your PC!")
               false refresh ()
         | None -> ());
        menu_mode := Exit
      end
  | Nicknaming -> (
      Ui.add_first_gameplay draw_nickname;
      match key with
      | Sdlkeycode.Unknown -> ()
      | c when c = Backspace ->
          nickname :=
            String.sub !nickname 0
              (Util.bound (String.length !nickname - 1) 0 14)
      | c when c = Return -> (
          match !target_creature with
          | Some creature ->
              set_nickname creature !nickname;
              menu_mode := Exit;
              Animation.display_text_box
                ("Welcome " ^ get_nickname creature ^ "!")
                false refresh ();
              if List.length (Player.party (State.player ())) = 6 then
                Animation.display_text_box
                  (get_nickname creature ^ " was sent to your PC!")
                  false refresh ()
          | None -> menu_mode := Exit)
      | c ->
          let c_to_string = Sdlkeycode.to_string c in
          if String.length c_to_string = 1 then
            (*Attempts to ignore weird characters by only accepting
              keycode strings of length 1*)
            if String.length !nickname <= 14 then
              nickname := !nickname ^ c_to_string)
  | Evolution -> ()
  | Exit -> ());

  (*====== MiniMenu ====== *)
  refresh ();
  Ui.update_all ();
  if !menu_mode <> Exit then run_tick ()

let init_capture creature () =
  menu_mode := Captured_Creature;
  target_creature := Some creature;
  nickname := "";
  ignore (Input.poll_key_option ());
  Animation.display_text_box
    ("Do you want to give " ^ get_nickname creature ^ " a name?")
    false refresh ();

  Ui.add_last_foreground draw_decision;

  run_tick ()

let init_evolution creature () =
  menu_mode := Evolution;
  target_creature := Some creature;
  ignore (Input.poll_key_option ());
  run_tick ()
