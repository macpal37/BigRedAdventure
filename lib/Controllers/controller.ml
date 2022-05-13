type modes =
  | ModeOverworld
  | ModeBattle
  | ModeMenu

let setup _ = ()

let main _ =
  Sdl.init [ `VIDEO; `JOYSTICK ];
  State.adhoc_init ();
  setup ();
  Draw.open_window ();

  Overworld.run_overworld ()
(* Battle.start_wild_battle (Creature.create_creature "rafu" 4) *)
