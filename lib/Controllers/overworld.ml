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

let draw_entities tile_x tile_y graphic_x graphic_y =
  let entities = Map.get_entities (State.map ()) in

  for i = 0 to List.length entities - 1 do
    let _, e = List.nth entities i in
    if Entity.is_visible e then (
    let x, y = (fst e.pos - tile_x + 6, snd e.pos - tile_y + 5) in
    let w, h = Draw.get_dimension e.sprite in
    Draw.draw_sprite e.sprite
      ((x * tile_size) - graphic_x - (w / 4))
      ((y * tile_size) - graphic_y + (h / 4))
      ())
  done

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
  let s = 48 / 2 in
  match orie with
  | Player.N ->
      Draw.draw_sprite_centered
        (player_sprite_n_walk i)
        (Draw.width / 2)
        ((Draw.height / 2) + s)
        ()
  | Player.E ->
      Draw.draw_sprite_centered
        (player_sprite_e_walk i)
        (Draw.width / 2)
        ((Draw.height / 2) + s)
        ()
  | Player.S ->
      Draw.draw_sprite_centered
        (player_sprite_s_walk i)
        (Draw.width / 2)
        ((Draw.height / 2) + s)
        ()
  | Player.W ->
      Draw.draw_sprite_centered
        (player_sprite_w_walk i)
        (Draw.width / 2)
        ((Draw.height / 2) + s)
        ()

let draw _ =
  draw_tiles
    (Player.x (State.player ()))
    (Player.y (State.player ()))
    0 0;
  draw_entities
    (Player.x (State.player ()))
    (Player.y (State.player ()))
    0 0;
  draw_player (Player.orie (State.player ()))

let encounter_anim _ =
  for i = 1 to 30 do
    let i = i * 2 in
    Draw.set_color 0;
    draw ();
    Draw.fill_rect (400 - (7 * i)) (360 - (6 * i)) (14 * i) (12 * i);
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
    draw_entities
      (Player.x (State.player ()))
      (Player.y (State.player ()))
      (i * dx * tile_size / speed)
      (i * dy * tile_size / speed);
    draw_player ~i:(i / 5 mod 3) (Player.orie (State.player ()));

    Draw.present ();
    Input.sleep Draw.tick_rate ()
  done

let attempt_move dx dy orie =
  if Player.get_orie (State.player ()) = orie then begin
    let new_x, new_y =
      (State.player_x () + dx, State.player_y () + dy)
    in
    match Map.get_type (State.map ()) (new_x, new_y) with
    | Path -> (
        try
          let e =
            List.assoc (new_x, new_y) (Map.get_entities (State.map ()))
          in
          if Entity.is_obstacle e = false then begin
            move_scroll dx dy;
            Player.set_coord new_x new_y (State.player ())
          end
        with Not_found ->
          move_scroll dx dy;
          Player.set_coord new_x new_y (State.player ()))
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
  end
  else Player.set_orie orie (State.player ())

let redraw _ =
  draw ();
  Draw.present ()

let attemp_action () =
  let new_x, new_y =
    match Player.get_orie (State.player ()) with
    | N -> (0, 1)
    | E -> (1, 0)
    | S -> (0, -1)
    | W -> (-1, 0)
  in
  try
    let e =
      List.assoc
        (new_x + State.player_x (), new_y + State.player_y ())
        (Map.get_entities (State.map ()))
    in
    if Entity.is_obstacle e then
      Entity.interact e (State.player ()) (fun () ->
          Ui.add_first_background draw;
          Ui.add_first_gameplay
            (Draw.draw_sprite DrawText.battle_bot 0 0))
  with Not_found -> ()
  
  (* let coord_add (orie : Entity.orientation) = 
    match orie with
    | N -> (0, 1)
    | E -> (1, 0)
    | S -> (0, -1)
    | W -> (-1, 0)

let rec incr_coord_list (x, y) dir num =
  let add_x, add_y = coord_add dir in
  if num = 0 then []
  else (x + add_x * num, y + add_y * num) :: incr_coord_list (x, y) dir (num - 1)

let max_sight_coords pos dir = incr_coord_list pos dir 4 |> List.rev

let is_obstacle (x, y) = 
  let e = List.assoc (x, y) (Map.get_entities (State.map ())) in 
  let is_e_obs = e.obstacle in 
  let is_t_obs = Map.get_type (State.map ()) (x, y) = Map.Obstacle in
  is_e_obs || is_t_obs

let rec sight_len clist = 
  match clist with
  | [] -> 4
  | h :: _ when is_obstacle h -> List.length clist
  | _ :: t -> sight_len t

let sight_dist trainer = 
  let pos = Entity.get_position trainer in
  let dir = Entity.get_orientation trainer in 
  max_sight_coords pos dir |> sight_len *)
  
let rec run_tick _ =
  (match Input.get_ctrl_option (Input.poll_key_option ()) with
  | Some Up -> attempt_move 0 1 Player.N
  | Some Left -> attempt_move (-1) 0 Player.W
  | Some Down -> attempt_move 0 (-1) Player.S
  | Some Right -> attempt_move 1 0 Player.E
  | Some Action -> attemp_action ()
  | Some Start -> Party_menu.init OverworldMode ()
  | Some Back -> ()
  | Some k -> ignore k
  | None -> ());
  redraw ();
  Input.sleep Draw.tick_rate ();
  run_tick ()

let run_overworld _ = run_tick ()
