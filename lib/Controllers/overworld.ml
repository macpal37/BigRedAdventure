let tile_size = 64
let tile_resolution = 16
let tile_dpi = tile_size / tile_resolution

let player_spritesheet =
  Spritesheet.init_spritesheet "assets/gui_sprites/player_sprite.png" 64
    64 1

let player_sprite_n = Spritesheet.get_sprite player_spritesheet 6

let player_sprite_n_walk =
  Util.list_index_fun
    [
      Spritesheet.get_sprite player_spritesheet 7;
      Spritesheet.get_sprite player_spritesheet 8;
      Spritesheet.get_sprite player_spritesheet 6;
    ]

let player_sprite_s = Spritesheet.get_sprite player_spritesheet 0

let player_sprite_s_walk =
  Util.list_index_fun
    [
      Spritesheet.get_sprite player_spritesheet 1;
      Spritesheet.get_sprite player_spritesheet 2;
      Spritesheet.get_sprite player_spritesheet 0;
    ]

let player_sprite_e = Spritesheet.get_sprite player_spritesheet 11

let player_sprite_e_walk =
  Util.list_index_fun
    [
      Spritesheet.get_sprite player_spritesheet 10;
      Spritesheet.get_sprite player_spritesheet 9;
      Spritesheet.get_sprite player_spritesheet 11;
    ]

let player_sprite_w = Spritesheet.get_sprite player_spritesheet 3

let player_sprite_w_walk =
  Util.list_index_fun
    [
      Spritesheet.get_sprite player_spritesheet 4;
      Spritesheet.get_sprite player_spritesheet 5;
      Spritesheet.get_sprite player_spritesheet 3;
    ]

let draw_tiles offset_x offset_y =
  Draw.set_color 0;
  Draw.fill_rect 0 0 800 720;
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
    ((Player.x (State.player ()) * tile_size) - ((800 - tile_size) / 2))
    ((Player.y (State.player ()) * tile_size) - ((720 - tile_size) / 2));
  match Player.orie (State.player ()) with
  | Player.N -> Draw.draw_sprite player_sprite_n 368 328 ()
  | Player.E -> Draw.draw_sprite player_sprite_e 368 328 ()
  | Player.S -> Draw.draw_sprite player_sprite_s 368 328 ()
  | Player.W -> Draw.draw_sprite player_sprite_w 368 328 ()

let encounter_anim _ =
  for i = 1 to 60 do
    Draw.set_color 0;
    Draw.fill_rect (400 - (7 * i)) (360 - (6 * i)) (14 * i) (12 * i);
    (match Player.orie (State.player ()) with
    | Player.N -> Draw.draw_sprite player_sprite_n 368 328 ()
    | Player.E -> Draw.draw_sprite player_sprite_e 368 328 ()
    | Player.S -> Draw.draw_sprite player_sprite_s 368 328 ()
    | Player.W -> Draw.draw_sprite player_sprite_w 368 328 ());
    Draw.present ();
    Input.sleep 0.016 ()
  done;
  Input.sleep 0.3 ()

let move_scroll dx dy =
  let speed = 9 in
  for i = 1 to speed - 1 do
    draw_tiles
      ((Player.x (State.player ()) * tile_size)
      - ((800 - tile_size) / 2)
      + (i * dx * tile_size / speed))
      ((Player.y (State.player ()) * tile_size)
      - ((720 - tile_size) / 2)
      + (i * dy * tile_size / speed));
    (match Player.orie (State.player ()) with
    | Player.N ->
        Draw.draw_sprite (player_sprite_n_walk (i / 5 mod 3)) 368 328 ()
    | Player.E ->
        Draw.draw_sprite (player_sprite_e_walk (i / 5 mod 3)) 368 328 ()
    | Player.S ->
        Draw.draw_sprite (player_sprite_s_walk (i / 5 mod 3)) 368 328 ()
    | Player.W ->
        Draw.draw_sprite (player_sprite_w_walk (i / 5 mod 3)) 368 328 ());
    Draw.present ();
    Input.sleep 0.016 ()
  done

let attempt_move dx dy orie =
  (* (if orie <> Player.orie (State.player ()) then *)
  Player.set_orie orie (State.player ());
  (* else *)
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
(* Input.poll (); match Input.key_option () with | Some 'w' ->
   attempt_move 0 1 Player.N | Some 'a' -> attempt_move (-1) 0 Player.W
   | Some 's' -> attempt_move 0 (-1) Player.S | Some 'd' -> attempt_move
   1 0 Player.E | Some k -> ignore k | None -> ()*)

let ui_refresh _ =
  Ui.clear_ui Ui.Background;
  Ui.clear_ui Gameplay;
  Ui.clear_ui Foreground;
  Ui.add_first_background draw;
  Ui.update_all ()

let rec run_tick _ =
  (match Input.poll_key_option () with
  | Some Sdlkeycode.W | Some Sdlkeycode.Up -> attempt_move 0 1 Player.N
  | Some Sdlkeycode.A | Some Sdlkeycode.Left ->
      attempt_move (-1) 0 Player.W
  | Some Sdlkeycode.S | Some Sdlkeycode.Down ->
      attempt_move 0 (-1) Player.S
  | Some Sdlkeycode.D | Some Sdlkeycode.Right ->
      attempt_move 1 0 Player.E
  | Some Sdlkeycode.E | Some Sdlkeycode.X ->
      Party_menu.init OverworldMode ()
  | Some Sdlkeycode.Q | Some Sdlkeycode.Z -> ()
  | Some k -> ignore k
  | None -> ());
  ui_refresh ();
  Input.sleep 0.016 ();
  (* if false then attempt_move 0 0 Player.N; if false then ui_refresh
     ();

     print_endline "OVERWORLD TICK!";*)
  run_tick ()

let run_overworld _ = run_tick ()
