let tile_size = 64
let tile_resolution = 16
let tile_dpi = tile_size / tile_resolution

let player_spritesheet =
  Spritesheet.init_spritesheet
    "assets/gui_sprites/player_sprite_overworld.png" 64 64 1

let player_sprite_n = Spritesheet.get_sprite player_spritesheet 6
let player_sprite_s = Spritesheet.get_sprite player_spritesheet 0
let player_sprite_e = Spritesheet.get_sprite player_spritesheet 11
let player_sprite_w = Spritesheet.get_sprite player_spritesheet 3

let draw_tiles offset_x offset_y orie =
  for i = 0 to Map.get_width (State.map ()) - 1 do
    for j = 0 to Map.get_height (State.map ()) - 1 do
      let s = Map.get_sprite (State.map ()) (i, j) in
      Draw.draw_sprite s
        ((i * tile_size) - offset_x)
        ((j * tile_size) - offset_y)
        ()
    done
  done;
  match orie with
  | Player.N -> Draw.draw_sprite player_sprite_n 368 328 ()
  | Player.E -> Draw.draw_sprite player_sprite_e 368 328 ()
  | Player.S -> Draw.draw_sprite player_sprite_s 368 328 ()
  | Player.W -> Draw.draw_sprite player_sprite_w 368 328 ()

let draw _ =
  draw_tiles
    ((Player.x (State.player ()) * tile_size) - ((800 - tile_size) / 2))
    ((Player.y (State.player ()) * tile_size) - ((720 - tile_size) / 2))
    (Player.orie (State.player ()))

let attempt_move dx dy orie =
  let new_x, new_y = (State.player_x () + dx, State.player_y () + dy) in
  (match Map.get_type (State.map ()) (new_x, new_y) with
  | Path -> Player.set_coord new_x new_y (State.player ())
  | Obstacle -> ()
  | Grass e ->
      if Random.float 1. < 0.2 then
        let c = Map.encounter_creature e in
        match c with
        | Some c -> Battle.start_battle c
        | None -> failwith "no creature encountered"
      else Player.set_coord new_x new_y (State.player ()));
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
      Party_menu.init ()
      (* Battle.start_battle (Creature.create_creature "rafu"
         (Random.int 10 + 35)) *)
  | Some 'q' -> ()
  | Some k -> ignore k
  | None -> ());
  ui_refresh ()
