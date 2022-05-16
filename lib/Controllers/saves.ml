type save_preview = {
  name : string;
  money : int;
  time : int;
  id : int;
}

let get_previews _ =
  Util.list_index_fun
    [
      Some { name = "RedRedRedRed"; money = 420; time = 69; id = 0 };
      Some { name = "Blue"; money = 1337; time = 3110; id = 1 };
      None;
    ]

let new_game _ _ =
  State.adhoc_init ();
  Overworld.run_overworld ()

let load_game _ =
  State.adhoc_init ();
  Overworld.run_overworld ()
