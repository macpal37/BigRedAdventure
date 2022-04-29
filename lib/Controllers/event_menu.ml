open Draw
open Creature
open Graphics
open Util
open DrawText

let event_menu = load_sprite "event_menu" GUI_Folder 3 ()
let decision_menu = load_sprite "decision_menu" GUI_Folder 3 ()
let nameholder = load_sprite "nameholder" GUI_Folder 3 ()
let minimenu_position = Util.new_point ()
let target_creature = null ()
let nickname = ref ""

type menu =
  | Captured_Creature
  | Nicknaming
  | Evolution
  | Exit

let menu_mode = ref Captured_Creature

let refresh () =
  let text_box = Graphics.get_image 3 0 797 216 in
  Ui.add_first_gameplay (fun () -> draw_image text_box 3 0);
  Ui.add_first_background (draw_sprite event_menu 0 0);

  match !target_creature with
  | Some creature ->
      Ui.add_first_gameplay
        (draw_sprite (get_front_sprite creature) 240 280)
  | None -> ()

let draw_decision () =
  let x, y = (800 - 105, 216) in

  draw_sprite decision_menu (x - 30) (y + 3) ();
  draw_string_colored x
    (y + 60 - (40 * minimenu_position.y))
    0 ">" white text_color ();
  draw_string_colored (x + 12) (y + 60) 0 "Yes" white text_color ();
  draw_string_colored (x + 12) (y + 20) 0 "No" white text_color ()

let draw_nickname () =
  draw_sprite nameholder 240 (280 - 70) ();
  draw_string_colored (240 + 12) (280 - 62) 0 !nickname white text_color
    ()

let rec run_tick () =
  Input.poll ();
  let key =
    match Input.key_option () with
    | Some c -> c
    | None -> '#'
  in

  (match !menu_mode with
  | Captured_Creature ->
      if key = 'w' then minimenu_position.y <- minimenu_position.y - 1;
      if key = 's' then minimenu_position.y <- minimenu_position.y + 1;
      if key = 'w' || key = 's' then begin
        minimenu_position.y <- Util.bound minimenu_position.y 0 1;
        Ui.add_first_foreground draw_decision
      end;
      if key = 'e' && minimenu_position.y = 0 then begin
        refresh ();
        menu_mode := Nicknaming
      end;

      if key = 'q' || (key = 'e' && minimenu_position.y = 1) then begin
        (if List.length (Player.party (State.player ())) = 6 then
         match !target_creature with
         | Some creature ->
             Ui.add_last_foreground
               (draw_text
                  (get_nickname creature ^ " was sent to your PC!")
                  40 true true)
         | None -> ());
        menu_mode := Exit
      end
  | Nicknaming -> (
      Ui.add_first_gameplay draw_nickname;

      match key with
      | '#' -> ()
      | c when c = Char.chr 8 ->
          nickname :=
            String.sub !nickname 0
              (Util.bound (String.length !nickname - 1) 0 14)
      | c when c = Char.chr 13 -> (
          match !target_creature with
          | Some creature ->
              set_nickname creature !nickname;
              menu_mode := Exit;
              Ui.add_last_foreground
                (draw_text
                   ("Welcome " ^ get_nickname creature ^ "!")
                   40 true false);
              if List.length (Player.party (State.player ())) = 6 then
                Ui.add_last_foreground
                  (draw_text
                     (get_nickname creature ^ " was sent to your PC!")
                     40 true true)
          | None -> menu_mode := Exit)
      | c ->
          if String.length !nickname <= 14 then
            nickname := !nickname ^ String.make 1 c)
  | Evolution -> ()
  | Exit -> ());

  (*====== MiniMenu ====== *)
  Ui.update_all ();
  if !menu_mode <> Exit then run_tick ()

let init_capture creature () =
  menu_mode := Captured_Creature;
  target_creature := Some creature;
  nickname := "";
  refresh ();
  Ui.add_last_foreground
    (draw_text
       ("Do you want to give " ^ get_nickname creature ^ " a name?")
       40 true true);

  Ui.add_last_foreground draw_decision;

  run_tick ()

let init_evolution creature () =
  menu_mode := Evolution;
  target_creature := Some creature;
  refresh ();
  run_tick ()
