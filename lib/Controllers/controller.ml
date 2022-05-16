(* let load_assets _ = Sprite_assets.load (); Item.load_items ();
   Map.load_maps (); Creature_menu.load_assets (); Overworld.load_assets
   (); Battle.load_assets (); Event_menu.load_assets ();
   Inventory_menu.load_assets (); Party_menu.load_assets () *)

let main _ =
  Loading_screen.load ();
  Sdl.init [ `VIDEO; `JOYSTICK ];
  Main_menu.load_assets ();
  Saves_menu.load_assets ();
  (* State.adhoc_init (); *)
  Draw.open_window ();

  if false then Main_menu.init ()
  else (
    Loading_screen.await ();
    Saves_menu.launch_new_game 0 "Red")
(* Battle.start_wild_battle (Creature.create_creature "rafu" 4) *)
