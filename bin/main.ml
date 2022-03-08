open Graphics
open CreatureGame.Draw

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

let game_mode = ref Adventure

let open_window =
  open_graph (" " ^ string_of_int width ^ "x" ^ string_of_int height);
  set_window_title "PokemonGame"

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

let get_move () : char option =
  if Graphics.key_pressed () then Some (Graphics.read_key ()) else None

let rayquaza = load_creature "rayquaza" ()
let clefairy_back = load_creature "clefairy_back" ()

let clear () =
  set_color blue;
  fill_rect 0 0 width height;
  set_color black;
  moveto 100 200

let start_up () =
  set_color red;
  fill_rect 0 0 width 212;
  set_color black;
  moveto 80 (height - 50 - 40);
  draw_string "RAYQUAZA :L80";
  draw_health_bar 100 100 100 true ();
  draw_health_bar 100 100 100 false ();
  draw_creature rayquaza false ();
  draw_creature clefairy_back true ();
  Unix.sleep 1;
  damage_render rayquaza false ();
  draw_health_bar 356 356 0 false ();
  Unix.sleepf 1.5;
  faint 20 2 rayquaza ();
  draw_text "It was super-effective!" ();
  draw_text "Clefairy is the best!!" ()

let rec event_loop wx wy =
  (* there's no resize event so polling in required *)
  let _ = wait_next_event [ Poll ]
  and wx' = size_x ()
  and wy' = size_y () in
  if wx' <> wx || wy' <> wy then clear_window blue;
  Unix.sleepf 0.005;

  let xx = get_move () in
  (match xx with
  | Some '.' -> clear ()
  | Some c -> (fun () -> draw_char c) ()
  | None -> ());

  event_loop wx' wy'

let () =
  open_window;

  moveto 100 200;
  set_font "-*-fixed-bold-r-semicondensed--40-*-*-*-*-*-iso8859-1";
  let r, g, b = color_to_rgb background in
  Printf.printf "Background color: %d %d %d\n" r g b;
  try event_loop 0 0
  with Graphic_failure _ -> print_endline "Exiting..."
