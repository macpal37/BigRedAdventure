type modes =
  | ModeOverworld
  | ModeBattle
  | ModeMenu

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
(* Battle.start_wild_battle (Creature.create_creature "rafu" 4) *)
