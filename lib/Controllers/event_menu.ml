open Draw
open Creature
open Util
open Draw_text
open Input

let event_menu = Util.null ()
let decision_menu = Util.null ()
let nameholder = Util.null ()
let minimenu_position = Util.new_point ()
let target_creature = null ()
let nickname = ref ""

let load_assets _ =
  event_menu *= Draw.get_sprite2 "event_menu" GUI_Folder;
  decision_menu *= Draw.get_sprite2 "decision_menu" GUI_Folder;
  nameholder *= Draw.get_sprite2 "nameholder" GUI_Folder

type menu =
  | Captured_Creature
  | Nicknaming
  | Evolution
  | Exit

let menu_mode = ref Captured_Creature

let refresh () =
  Ui.add_first_background (draw_sprite ~!event_menu 0 (-3));

  (match !target_creature with
  | Some creature ->
      if !menu_mode <> Evolution then
        Ui.add_first_gameplay
          (draw_sprite (get_front_sprite creature) 280 240)
  | None -> ());
  Ui.add_first_foreground (clear_text empty_sprite)

let draw_decision () =
  let x, y = (800 - 105, 216) in

  draw_sprite ~!decision_menu (x - 30) (y + 3) ();
  draw_string_colored x
    (y + 60 - (40 * minimenu_position.y))
    Medium ">" white text_color ();
  draw_string_colored (x + 12) (y + 60) Medium "Yes" white text_color ();
  draw_string_colored (x + 12) (y + 20) Medium "No" white text_color ()

let tick = ref 0

let draw_nickname () =
  draw_sprite ~!nameholder 240 (280 - 70) ();

  if !tick mod 20 <= 10 then
    draw_string_colored (240 + 12) (280 - 62) Medium !nickname white
      text_color ()
  else
    draw_string_colored (240 + 12) (280 - 62) Medium (!nickname ^ "_")
      white text_color ()

let rec run_tick () =
  if !menu_mode = Captured_Creature then Input.sleep Draw.tick_rate ()
  else Input.sleep (Draw.tick_rate /. 10.) ();
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

      if key => Action && minimenu_position.y = 0 then begin
        menu_mode := Nicknaming;
        set_text_display "Press ENTER when you're done!"
      end;

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
      tick := !tick + 1;
      Ui.add_first_gameplay draw_nickname;
      match key with
      | Sdlkeycode.Unknown -> ()
      | c when c = Sdlkeycode.Space -> nickname := !nickname ^ " "
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
  | Evolution ->
      let old_name, new_name =
        ( get_nickname ~!target_creature,
          get_evolution_name ~!target_creature )
      in
      let old_sprite, new_sprite =
        ( get_front_sprite ~!target_creature,
          Spritesheet.get_sprite
            (Spritesheet.get_spritesheet
               ("assets/creature_sprites/"
               ^ String.lowercase_ascii
                   (get_evolution_name ~!target_creature)
               ^ ".png"))
            (if is_shiny ~!target_creature then 1 else 0) )
      in
      Animation.animate_evolution old_sprite new_sprite refresh;

      Animation.display_text_box
        ("Your " ^ old_name ^ " has evolved into a " ^ new_name)
        false
        (fun () ->
          refresh ();
          Ui.add_first_gameplay (reset_rgb new_sprite);
          Ui.add_last_gameplay (draw_sprite new_sprite 280 240))
        ();
      evolve ~!target_creature;

      wait 240 ();
      menu_mode := Exit
  | Exit -> ());

  (*====== MiniMenu ====== *)
  refresh ();
  Ui.update_all ();
  if !menu_mode <> Exit then run_tick ()

let init_capture creature () =
  menu_mode := Captured_Creature;
  target_creature := Some creature;
  nickname := "";
  tick := 0;
  Ui.clear_all ();
  Animation.display_text_box
    ("Do you want to give " ^ get_nickname creature ^ " a name?")
    true refresh ();
  Ui.add_last_foreground draw_decision;

  print_endline !Draw_text.get_text_display;
  run_tick ()

let init_evolution creature () =
  menu_mode := Evolution;
  target_creature := Some creature;
  ignore (Input.poll_key_option ());
  Animation.display_text_box
    ("What? " ^ get_nickname creature ^ " is evolving!")
    true
    (fun () ->
      refresh ();

      Ui.add_last_gameplay
        (draw_sprite (get_front_sprite ~!target_creature) 280 240))
    ();
  wait 60 ();
  run_tick ()
