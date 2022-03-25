open Graphics

let width = 800
let height = 720
let tick_rate = 0.016

type modes =
  | ModeOverworld
  | ModeBattle
  | ModeMenu

type state = { player : Player.player }

let current_state = ref { player = Player.new_player "Red" }
let get_state _ = !current_state
let player _ = !current_state.player

let debug_draw () =
  let x, y = mouse_pos () in
  Draw.sync false ();
  moveto 0 0;
  set_color white;
  fill_rect 0 0 300 50;
  set_color black;
  draw_string ("X: " ^ string_of_int x ^ " Y: " ^ string_of_int y);

  Draw.sync true ()

let open_window =
  open_graph (" " ^ string_of_int width ^ "x" ^ string_of_int height);
  set_window_title "Big Red Adventure"

let rec event_loop mode =
  let _ = wait_next_event [ Poll ]
  (* Counteract any potential resizing *)
  and wx' = size_x ()
  and wy' = size_y () in
  if wx' <> width || wy' <> height then resize_window width height;

  Input.poll ();

  (match mode with
  | ModeOverworld -> Overworld.run_tick ()
  | ModeBattle -> Battle.run_tick ()
  | ModeMenu -> Inventory_menu.run_tick ());

  let key =
    match Input.key_option () with
    | Some c -> c
    | None -> '#'
  in
  if key = 'r' then debug_draw ();

  let new_mode = mode in
  (* placeholder*)
  Unix.sleepf tick_rate;
  event_loop new_mode

let setup _ = ()

let main _ =
  open_window;
  (* Battle.start_battle (); *)
  let red = Player.new_player "Red" in
  Inventory_menu.open_inventory red ();
  moveto 100 200;
  set_font "-*-fixed-bold-r-semicondensed--40-*-*-*-*-*-iso8859-1";
  setup ();
  try event_loop ModeMenu
  with Graphic_failure _ -> print_endline "Exiting..."
