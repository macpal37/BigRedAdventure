open Graphics

let width = 800
let height = 720
let tick_rate = 1. /. 60.

type modes =
  | ModeOverworld
  | ModeBattle
  | ModeMenu

let open_window =
  open_graph (" " ^ string_of_int width ^ "x" ^ string_of_int height);
  set_window_title "Big Red Adventure"

let rec event_loop mode =
  let _ = wait_next_event [ Poll ]
  (* Counteract any potential resizing *)
  and wx' = size_x ()
  and wy' = size_y () in
  if wx' <> width || wy' <> height then resize_window width height;

  (*if start then start_up ();*)
  let key_input =
    (fun _ ->
      if Graphics.key_pressed () then Some (Graphics.read_key ())
      else None)
      ()
  in
  let key_mapping =
    match key_input with
    | Some c -> Input.key_press c
    | None -> Input.key_press '#'
  in
  key_mapping;
  (match mode with
  | ModeOverworld -> Overworld.run_tick key_input
  | ModeBattle -> Battle.run_tick key_input
  | ModeMenu -> Menu.run_tick key_input);
  let new_mode = mode in
  (* placeholder*)
  Unix.sleepf tick_rate;
  event_loop new_mode

let main _ =
  Input.keymap_init [ 'q'; 'e'; 'w'; 'a'; 's'; 'd'; '#' ];
  open_window;
  Battle.init ();
  moveto 100 200;
  set_font "-*-fixed-bold-r-semicondensed--40-*-*-*-*-*-iso8859-1";
  try event_loop ModeBattle
  with Graphic_failure _ -> print_endline "Exiting..."
