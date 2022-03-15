open Graphics
open CreatureGame

let init = Random.self_init ()
let white = rgb 55 255 255
let blue = rgb 200 200 240
let black = rgb 0 0 0
let red = rgb 200 50 50
let width = 800
let height = 720

type mode =
  | Adventure
  | Combat
  | Menu

let game = ref Adventure

let open_window =
  open_graph (" " ^ string_of_int width ^ "x" ^ string_of_int height);
  set_window_title "BigRedAdventures"

(* no way of setting background color; resizing shows white *)
let clear_window color =
  let fg = foreground in
  set_color color;
  fill_rect 0 0 (size_x ()) (size_y ());
  set_color fg

(* no function for converting color back to rgb in Graphics *)
let color_to_rgb color =
  let r = (color land 0xFF0000) asr 0x10
  and g = (color land 0x00FF00) asr 0x8
  and b = color land 0x0000FF in
  (r, g, b)

let clear () =
  set_color blue;
  fill_rect 0 0 width height;
  set_color Draw.text_color;
  moveto 100 200

(*****************************************************************)
(***************      Game Run      *****************************)
(*****************************************************************)
let run_game game () =
  match game.contents with
  (*****************************************************************)
  (***************      Adventure      *****************************)
  (*****************************************************************)
  | Adventure ->
      print_endline "Start of Adventure";

      game.contents <- Combat
  (*****************************************************************)
  (***************        Combat       *****************************)
  (*****************************************************************)
  | Combat -> Battle.run_combat ()
  (*************** Menu *****************************)
  | Menu -> game.contents <- Adventure

(*****************************************************************)
(***************        Game Loop        *****************************)
(*****************************************************************)
let rec event_loop wx wy start game =
  (* there's no resize event so polling in required *)
  let _ = wait_next_event [ Poll ]
  and wx' = size_x ()
  and wy' = size_y () in
  if wx' <> wx || wy' <> wy then clear_window blue;

  if start then Battle.start_up ();
  (match
     (fun () ->
       if Graphics.key_pressed () then Some (Graphics.read_key ())
       else None)
       ()
   with
  | Some 'n' -> Battle.adhoc_test1 ()
  | Some c ->
      Input.key_press c;
      run_game game ()
  | None -> run_game game ());

  Ui.update_all ();
  Unix.sleepf 0.0017;
  event_loop wx' wy' false game

let () =
  Controller.main ();
  open_window;
  moveto 100 200;
  Input.keymap_init [ 'q'; 'e'; 'w'; 'a'; 's'; 'd' ];

  Battle.init1 ();
  set_font "-*-fixed-bold-r-semicondensed--40-*-*-*-*-*-iso8859-1";
  let r, g, b = color_to_rgb background in
  Printf.printf "Background color: %d %d %d\n" r g b;
  try event_loop 0 0 true game
  with Graphic_failure _ -> print_endline "Exiting..."
