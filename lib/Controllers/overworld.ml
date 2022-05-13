open Util

let tile_size = 64
let tile_resolution = 16
let tile_dpi = tile_size / tile_resolution
let player_spritesheet = Util.null ()

let load_assets _ =
  player_spritesheet
  *= Sprite_assets.get_spritesheet
       "assets/entity_sprites/player_sprite_overworld.png"

let player_sprite_n_walk i =
  match i with
  | 0 -> Spritesheet.get_sprite ~!player_spritesheet 7
  | 1 -> Spritesheet.get_sprite ~!player_spritesheet 8
  | 2 -> Spritesheet.get_sprite ~!player_spritesheet 6
  | _ -> failwith ("Out of bounds: " ^ string_of_int i)

let player_sprite_s_walk i =
  match i with
  | 0 -> Spritesheet.get_sprite ~!player_spritesheet 1
  | 1 -> Spritesheet.get_sprite ~!player_spritesheet 2
  | 2 -> Spritesheet.get_sprite ~!player_spritesheet 0
  | _ -> failwith ("Out of bounds: " ^ string_of_int i)

let player_sprite_e_walk i =
  match i with
  | 0 -> Spritesheet.get_sprite ~!player_spritesheet 10
  | 1 -> Spritesheet.get_sprite ~!player_spritesheet 9
  | 2 -> Spritesheet.get_sprite ~!player_spritesheet 11
  | _ -> failwith ("Out of bounds: " ^ string_of_int i)

let player_sprite_w_walk i =
  match i with
  | 0 -> Spritesheet.get_sprite ~!player_spritesheet 4
  | 1 -> Spritesheet.get_sprite ~!player_spritesheet 5
  | 2 -> Spritesheet.get_sprite ~!player_spritesheet 3
  | _ -> failwith ("Out of bounds: " ^ string_of_int i)

(** draw_tiles tx ty gx gy draws the tiles centered at (tx,ty) within
    [Draw.width / (tile_size * 2) + 1] tiles with graphical offset of
    (gx,gy)*)
let draw_tiles tile_x tile_y graphic_x graphic_y =
  Draw.set_color 0;
  Draw.fill_rect 0 0 Draw.width Draw.height;
  let range = (Draw.width / (tile_size * 2)) + 1 in
  for i = -range + tile_x to range + tile_x do
    for j = -range + tile_y to range + tile_y do
      if
        i >= 0
        && i < Map.get_width (State.map ())
        && j >= 0
        && j < Map.get_height (State.map ())
      then
        let s = Map.get_sprite (State.map ()) (i, j) in
        Draw.draw_sprite_centered s
          (((i - tile_x) * tile_size) + (Draw.width / 2) - graphic_x)
          (((j - tile_y) * tile_size) + (Draw.height / 2) - graphic_y)
          ()
    done
  done

let draw_player ?(i = 2) orie =
  match orie with
  | Player.N ->
      Draw.draw_sprite_centered
        (player_sprite_n_walk i)
        (Draw.width / 2) (Draw.height / 2) ()
  | Player.E ->
      Draw.draw_sprite_centered
        (player_sprite_e_walk i)
        (Draw.width / 2) (Draw.height / 2) ()
  | Player.S ->
      Draw.draw_sprite_centered
        (player_sprite_s_walk i)
        (Draw.width / 2) (Draw.height / 2) ()
  | Player.W ->
      Draw.draw_sprite_centered
        (player_sprite_w_walk i)
        (Draw.width / 2) (Draw.height / 2) ()

let draw _ =
  draw_tiles
    (Player.x (State.player ()))
    (Player.y (State.player ()))
    0 0;
  draw_player (Player.orie (State.player ()))

let encounter_anim _ =
  for i = 1 to 60 do
    Draw.set_color 0;
    Draw.fill_rect (400 - (7 * i)) (360 - (6 * i)) (14 * i) (12 * i);
    draw ();
    Draw.present ();
    Input.sleep Draw.tick_rate ()
  done;
  Input.sleep 0.3 ()

let move_scroll dx dy =
  let speed = 12 in
  for i = 1 to speed - 1 do
    draw_tiles
      (Player.x (State.player ()))
      (Player.y (State.player ()))
      (i * dx * tile_size / speed)
      (i * dy * tile_size / speed);
    draw_player ~i:(i / 5 mod 3) (Player.orie (State.player ()));
    Draw.present ();
    Input.sleep Draw.tick_rate ()
  done

let attempt_move dx dy orie =
  Player.set_orie orie (State.player ());
  let new_x, new_y = (State.player_x () + dx, State.player_y () + dy) in
  match Map.get_type (State.map ()) (new_x, new_y) with
  | Path ->
      move_scroll dx dy;
      Player.set_coord new_x new_y (State.player ())
  | Obstacle -> ()
  | Grass e ->
      move_scroll dx dy;
      Player.set_coord new_x new_y (State.player ());
      if Random.float 1. < 0.2 then (
        encounter_anim ();
        let c = Map.encounter_creature e in
        match c with
        | Some c -> Battle.start_wild_battle c
        | None -> failwith "no creature encountered")

let redraw _ =
  draw ();
  Draw.present ()

let rec run_tick _ =
  (match Input.get_ctrl_option (Input.poll_key_option ()) with
  | Some Up -> attempt_move 0 1 Player.N
  | Some Left -> attempt_move (-1) 0 Player.W
  | Some Down -> attempt_move 0 (-1) Player.S
  | Some Right -> attempt_move 1 0 Player.E
  | Some Action -> Party_menu.init OverworldMode ()
  | Some Back -> ()
  | Some k -> ignore k
  | None -> ());
  redraw ();
  Input.sleep Draw.tick_rate ();
  run_tick ()

let run_overworld _ = run_tick ()
