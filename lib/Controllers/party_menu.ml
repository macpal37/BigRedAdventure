open Draw
open Graphics
open Creature
open Animation

(* let max_creatures = 5 let max_creatures = ref 0 *)
let party_menu_bg = load_sprite "party_menu" GUI_Folder 3 ()
let active = load_sprite "active_party_creature" GUI_Folder 3 ()
let chibi = load_sprite "chibi_clefairy" Creature_Folder 1 ()
let party_position = Util.new_point ()

let draw_menu lead_creature () =
  Ui.add_first_background (draw_sprite party_menu_bg 0 0);
  Ui.add_first_foreground
    (draw_string_colored 40 36 3 50 "Choose a CREATURE"
       (rgb 215 215 255));
  Ui.add_first_foreground
    (draw_string_colored 24 605 4 60 "PARTY" (rgb 255 170 40));
  Ui.add_first_gameplay
    (draw_sprite (get_front_sprite lead_creature) 3 318);
  Ui.add_first_gameplay
    (draw_string_colored 20 246 1 30 (get_nickname lead_creature) white);
  Ui.add_first_gameplay
    (draw_string_colored 20 220 1 20
       ("LVL: " ^ string_of_int (get_level lead_creature))
       white);
  let max, bef, aft = get_hp_status lead_creature in

  Ui.add_first_gameplay (draw_health_bar max bef aft 56 192 180 6 true)

let draw_creature_status creature () =
  let x, y = (276, 577) in
  let xx = 294 + 80 in
  Ui.add_first_background (draw_sprite active x y);
  Ui.add_first_background (draw_sprite chibi x y);
  Ui.add_first_gameplay
    (draw_string_colored xx 636 1 30 (get_nickname creature) white);
  Ui.add_first_gameplay
    (draw_string_colored xx 610 1 20
       ("LVL: " ^ string_of_int (get_level creature))
       white)

let open_party () =
  let clefairy = create_creature "clefairy" 10 in
  draw_creature_status clefairy ();
  draw_menu clefairy ()

let run_tick () =
  let key =
    match Input.key_option () with
    | Some c -> c
    | None -> '#'
  in
  let x, _ = (party_position.x, party_position.y) in
  if key = 'd' then party_position.x <- x + 1;
  if key = 'a' then party_position.x <- x - 1;

  Ui.update_all ()
