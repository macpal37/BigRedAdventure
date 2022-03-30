let tile_size = 64
let tile_resolution = 16
let tile_dpi = tile_size / tile_resolution

let draw_tiles offset_x offset_y =
  for i = 0 to Map.get_width (State.map ()) - 1 do
    for j = 0 to Map.get_height (State.map ()) - 1 do
      let s = Map.get_sprite (State.map ()) (i, j) in
      Draw.draw_sprite s
        ((i * tile_size) - offset_x)
        ((j * tile_size) - offset_y)
        ()
    done
  done;
  Graphics.fill_rect 368 328 64 64

let draw _ =
  draw_tiles
    ((Player.x (State.player ()) * tile_size) - ((800 - tile_size) / 2))
    ((Player.y (State.player ()) * tile_size) - ((720 - tile_size) / 2))

let attempt_move dx dy orie =
  let new_x, new_y = (State.player_x () + dx, State.player_y () + dy) in
  (match Map.get_type (State.map ()) (new_x, new_y) with
  | Path -> Player.set_coord new_x new_y (State.player ())
  | Obstacle -> ()
  | Grass e ->
      ignore e;
      Player.set_coord new_x new_y (State.player ()));
  Player.set_orie orie (State.player ())
(*let move_scroll _ = () *)

let ui_refresh _ =
  Ui.clear_ui Ui.Background;
  Ui.clear_ui Gameplay;
  Ui.clear_ui Foreground;
  Ui.add_first_background draw;
  Ui.update_all ()

let run_tick _ =
  (match Input.key_option () with
  | Some 'w' -> attempt_move 0 1 Player.N
  | Some 'a' -> attempt_move (-1) 0 Player.W
  | Some 's' -> attempt_move 0 (-1) Player.S
  | Some 'd' -> attempt_move 1 0 Player.E
  | Some 'e' ->
      Battle.start_battle ();
      print_endline "End!!!!"
  | Some 'q' -> ()
  | Some k -> ignore k
  | None -> ());
  ui_refresh ()
