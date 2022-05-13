(* let tick_rate = 0.0 *)

type modes =
  | ModeOverworld
  | ModeBattle
  | ModeMenu
(* let debug_draw () = let x, y = mouse_pos () in Draw.sync false ();
   moveto 0 0; set_color white; fill_rect 0 0 300 50; set_color black;
   draw_string ("X: " ^ string_of_int x ^ " Y: " ^ string_of_int y);

   Draw.sync true () *)
(* let open_window = open_graph (" " ^ string_of_int width ^ "x" ^
   string_of_int height); set_window_title "Big Red Adventure" *)
(* let rec event_loop mode = let _ = wait_next_event [ Poll ] (*
   Counteract any potential resizing *) and wx' = size_x () and wy' =
   size_y () in if wx' <> width || wy' <> height then resize_window
   width height;

   Input.poll ();

   (match mode with | ModeOverworld -> Overworld.run_tick () |
   ModeBattle -> Battle.run_tick () | ModeMenu -> Party_menu.run_tick
   ());

   let key = match Input.key_option () with | Some c -> c | None -> '#'
   in if key = 'r' then debug_draw ();

   let new_mode = mode in (* placeholder*) Unix.sleepf Draw.tick_rate;
   event_loop new_mode *)

let load_assets _ =
  Play_assets.load ();
  Sprite_assets.load ();
  Creature_menu.load_assets ();
  Overworld.load_assets ();
  Battle.load_assets ();
  Event_menu.load_assets ();
  Inventory_menu.load_assets ();
  Party_menu.load_assets ()

let main _ =
  Sdl.init [ `VIDEO; `JOYSTICK ];
  load_assets ();
  State.adhoc_init ();

  Draw.open_window ();

  Overworld.run_overworld ()
(* Draw.set_synced_mode false; let mode = ModeOverworld in *)
(* Battle.start_wild_battle (Creature.create_creature "rafu" 10) *)

(* moveto 100 200; *)
(* set_font "-*-fixed-bold-r-semicondensed--40-*-*-*-*-*-iso8859-1";
   setup (); try event_loop mode with Graphic_failure _ -> print_endline
   "Exiting..."*)
