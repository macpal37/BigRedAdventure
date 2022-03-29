let tile_size = 64
let tile_resolution = 16
let tile_dpi = tile_size / tile_resolution
(* let grid_width = 800 / tile_size let grid_height = 720 / tile_size *)

let draw_tiles offset_x offset_y =
  for i = 0 to Map.get_width (State.map ()) - 1 do
    for j = 0 to Map.get_height (State.map ()) - 1 do
      let s = Map.get_sprite (State.map ()) (i, j) in
      Draw.draw_sprite s
        ((i * tile_size) - offset_x)
        ((j * tile_size) - offset_y)
        ()
    done
  done

let draw _ =
  draw_tiles
    ((Player.x (State.player ()) * tile_size) + (tile_size / 2))
    ((Player.y (State.player ()) * tile_size) + (tile_size / 2))
(* let attempt_move x y = false let move_scroll _ = () *)

let ui_refresh _ =
  Ui.clear_ui Ui.Background;
  Ui.clear_ui Gameplay;
  Ui.clear_ui Foreground;
  Ui.add_first_background draw;
  Ui.update_all ()

let run_tick _ =
  (match Input.key_option () with
  | Some 'w' -> ()
  | Some 'a' -> ()
  | Some 's' -> ()
  | Some 'd' -> ()
  | Some 'e' -> ()
  | Some 'q' -> ()
  | Some k -> ignore k
  | None -> ());
  ui_refresh ()
