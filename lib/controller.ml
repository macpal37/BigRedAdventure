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
let get_player s = s.player

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
  | ModeMenu -> Menu.run_tick ());
  let new_mode = mode in
  (* placeholder*)
  Unix.sleepf tick_rate;
  event_loop new_mode

let setup _ = ()

let main _ =
  open_window;
  Battle.init ();
  moveto 100 200;
  set_font "-*-fixed-bold-r-semicondensed--40-*-*-*-*-*-iso8859-1";
  setup ();
  try event_loop ModeBattle
  with Graphic_failure _ -> print_endline "Exiting..."
