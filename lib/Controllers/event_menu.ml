open Draw
open Creature
open Graphics

let event_menu = load_sprite "event_menu" GUI_Folder 3 ()
let decision_menu = load_sprite "decision_menu" GUI_Folder 3 ()
let minimenu_position = Util.new_point ()
let target_creature = ref Option.None

type menu =
  | Captured_Creature
  | Evolution
  | Exit

let menu_mode = ref Captured_Creature
let refresh () = Ui.add_first_background (draw_sprite event_menu 0 0)

let draw_decision () =
  let x, y = (800 - 105, 216) in

  draw_sprite decision_menu (x - 30) (y + 3) ();
  draw_string_colored (x + 12) (y + 60) 1 30 "Yes" white text_color ();
  draw_string_colored (x + 12) (y + 20) 1 30 "No" white text_color ()

let rec run_tick () =
  Input.poll ();
  let key =
    match Input.key_option () with
    | Some c -> c
    | None -> '#'
  in
  (match menu_mode.contents with
  | Captured_Creature ->
      if key = 'w' then minimenu_position.y <- minimenu_position.y + 1;
      if key = 's' then minimenu_position.y <- minimenu_position.y - 1;
      if key = 'w' || key = 's' then
        minimenu_position.y <- Util.bound minimenu_position.y 0 1;

      if key = 'q' then menu_mode.contents <- Exit
  | Evolution -> ()
  | Exit -> ());

  (*====== MiniMenu ====== *)
  Ui.update_all ();
  if menu_mode.contents <> Exit then run_tick ()

let init_capture creature () =
  menu_mode.contents <- Captured_Creature;
  target_creature.contents <- Some creature;
  refresh ();

  Ui.add_first_gameplay
    (draw_sprite (get_front_sprite creature) 240 280);

  Ui.add_last_foreground
    (draw_text
       ("Do you want to give " ^ get_nickname creature ^ " a name?")
       40 true false);

  Ui.add_last_foreground draw_decision;

  run_tick ()

let init_evolution creature () =
  menu_mode.contents <- Evolution;
  target_creature.contents <- Some creature;
  refresh ();
  run_tick ()
