open Draw

let menu_position = Util.new_point ()
let refresh () = ()

let init () =
  Draw.set_synced_mode false;

  refresh ();
  Ui.add_first_background clear_screen

let rec run_tick () =
  Input.poll ();
  let key =
    match Input.key_option () with
    | Some c -> c
    | None -> '#'
  in
  let _, y = (menu_position.x, menu_position.y) in
  if key = 'w' && menu_position.y > 0 then menu_position.y <- y - 1;
  if key = 's' && menu_position.y < 4 then menu_position.y <- y + 1;

  if key = 'w' || key = 's' then refresh ();

  Ui.update_all ();
  if key <> 'q' then run_tick () else Draw.set_synced_mode true
