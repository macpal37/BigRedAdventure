open Util

let tile_size = 64
let tile_resolution = 16
let tile_dpi = tile_size / tile_resolution
let player_spritesheet = Util.null ()
let win_title = Util.null ()

let load_assets _ =
  player_spritesheet
  *= Spritesheet.get_spritesheet
       "assets/entity_sprites/player_sprite_overworld.png";
  win_title *= Draw.get_sprite2 "win" GUI_Folder

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
    if Entity.is_visible e then
      let x, y = (fst e.pos - tile_x + 6, snd e.pos - tile_y + 5) in
      let w, h = Draw.get_dimension e.sprite in
      Draw.draw_sprite e.sprite
        ((x * tile_size) - graphic_x - (w / 4))
        ((y * tile_size) - graphic_y + (h / 4))
        ()
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
    draw_player (Player.orie (State.player ()));
    Draw.present ();
    Input.sleep Draw.tick_rate ()
  done;
  for i = 1 to 10 do
    Draw.set_color 0;
    Draw.fill_rect 0 0 Draw.width Draw.height;
    draw_player (Player.orie (State.player ()));
    Draw.set_draw_color ~a:(i * 25) 0 0 0;
    Draw.fill_rect 0 0 Draw.width Draw.height;
    Draw.present ();
    Input.sleep Draw.tick_rate ()
  done;
  Draw.set_color 0;
  Draw.fill_rect 0 0 Draw.width Draw.height;
  Draw.present ()

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

let player_check e =
  (* let e_pos = Entity.get_position e in *)
  let player_pos = (State.player_x (), State.player_y ()) in
  match Entity.get_trigger e with
  | Trainer t ->
      if List.mem player_pos t.sight then print_endline "I SEE YOU"
  (* | Door _ -> if e_pos = player_pos then failwith "TODO: DO DOOR
     TELEPORTAION" *)
  | _ -> ()

let teleport map coord dx dy =
  move_scroll dx dy;
  Player.set_coord
    (State.player_x () + dx)
    (State.player_y () + dy)
    (State.player ());
  for i = 0 to 28 do
    draw ();
    Draw.set_draw_color ~a:(i * 9) 0 0 0;
    Draw.fill_rect 0 0 Draw.width Draw.height;
    Draw.present ();
    Input.sleep Draw.tick_rate ()
  done;
  Draw.set_draw_color 0 0 0;
  Draw.fill_rect 0 0 Draw.width Draw.height;
  Draw.present ();
  Input.sleep 0.2 ();

  let x, y = coord in
  State.set_map (Map.get_map map);

  Player.set_x x (State.player ());
  Player.set_y y (State.player ());
  for i = 0 to 28 do
    draw ();
    Draw.set_draw_color ~a:(255 - (i * 9)) 0 0 0;
    Draw.fill_rect 0 0 Draw.width Draw.height;
    Draw.present ();
    Input.sleep Draw.tick_rate ()
  done

let rec win_loop _ =
  match Input.pop_key_option () with
  | Some _ ->
      for i = 0 to 80 do
        Draw.set_draw_color 0 0 0;
        Draw.fill_rect 0 0 Draw.width Draw.height;
        Draw.draw_sprite_centered ~!win_title (Draw.width / 2) 640 ();
        Draw_text.draw_string_colored
          ((Draw.width / 2) - 170)
          300 Draw_text.Medium "Press any key to exit" Draw.white
          Draw.text_color ();
        Draw.set_draw_color ~a:(min (i * 4) 255) 0 0 0;
        Draw.fill_rect 0 0 Draw.width Draw.height;
        Draw.present ()
      done;
      exit 0
  | None ->
      Draw.set_draw_color 0 0 0;
      Draw.fill_rect 0 0 Draw.width Draw.height;
      Draw.draw_sprite_centered ~!win_title (Draw.width / 2) 640 ();
      Draw_text.draw_string_colored
        ((Draw.width / 2) - 170)
        300 Draw_text.Medium "Press any key to exit" Draw.white
        Draw.text_color ();
      Draw.present ();
      Input.sleep Draw.tick_rate ();
      win_loop ()

let win_fade dx dy =
  move_scroll dx dy;
  Player.set_coord
    (State.player_x () + dx)
    (State.player_y () + dy)
    (State.player ());
  for i = 0 to 70 do
    draw ();

    Draw.set_draw_color ~a:(min (i * 4) 255) 0 0 0;
    Draw.fill_rect 0 0 Draw.width Draw.height;
    Draw.draw_sprite_centered ~!win_title (Draw.width / 2)
      (int_of_float
         (900.
         -. 260.
            *. Float.pow (float_of_int (max (i - 35) 45) /. 45.) 0.5))
      ();
    Draw.present ();
    Input.sleep Draw.tick_rate ()
  done;
  Draw.set_draw_color 0 0 0;
  Draw.fill_rect 0 0 Draw.width Draw.height;
  Draw.draw_sprite_centered ~!win_title (Draw.width / 2) 640 ();
  Draw.present ();
  Input.sleep 1. ();
  win_loop ()

let attempt_move dx dy orie =
  if Player.get_orie (State.player ()) = orie then
    let new_x, new_y =
      (State.player_x () + dx, State.player_y () + dy)
    in
    let e_opt =
      try
        Some
          (List.assoc (new_x, new_y) (Map.get_entities (State.map ())))
      with Not_found -> None
    in

    let process_move _ =
      match Map.get_type (State.map ()) (new_x, new_y) with
      | Path -> (
          match e_opt with
          | Some e ->
              if Entity.is_obstacle e = false then begin
                move_scroll dx dy;
                Player.set_coord new_x new_y (State.player ());
                player_check e
              end
          | None ->
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
    in

    match e_opt with
    | Some e -> (
        match e.e_type with
        | Door (map, coord) -> teleport map coord dx dy
        | Win -> win_fade dx dy
        | _ -> process_move ())
    | None -> process_move ()
  else Player.set_orie orie (State.player ())

let redraw _ =
  draw ();
  Draw.present ()

let attempt_action () =
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
      Entity.interact e State.player (fun () ->
          Ui.add_first_background draw;
          Ui.add_first_gameplay
            (Draw.draw_sprite Draw_text.battle_bot 0 0))
    (* match e.e_type with | Trainer t -> if e.state = 0 then
       Battle.start_trainer_battle t.party | _ -> () *)
  with Not_found -> ()

let rec iter_entities f e_list =
  match e_list with
  | [] -> ()
  | (_, e) :: t ->
      f e;
      iter_entities f t

let trainer_detect e =
  let player_pos = (State.player_x (), State.player_y ()) in
  match Entity.get_trigger e with
  | Trainer t -> (
      print_endline (Entity.get_state e |> string_of_int);
      match Entity.get_state e with
      | 0 ->
          if List.mem player_pos t.sight then (
            draw ();
            let x, y = e.pos in
            Draw_text.draw_string_colored
              (((x - State.player_x ()) * tile_size)
              + (Draw.width / 2) - 12)
              (((y - State.player_y ()) * tile_size)
              + (Draw.height / 2) + 40)
              Draw_text.Huge "!" 0 0 ();
            Draw_text.draw_string_colored
              (((x - State.player_x ()) * tile_size)
              + (Draw.width / 2) - 10)
              (((y - State.player_y ()) * tile_size)
              + (Draw.height / 2) + 38)
              Draw_text.Huge "!" Draw.white 0 ();
            Draw.present ();
            Input.sleep 0.8 ();
            encounter_anim ();
            Battle.start_trainer_battle t.party;
            Entity.set_state e 1;
            e.dialogue <- t.alt_dialogue)
      | _ -> ())
  | _ -> ()

let trainer_action () =
  State.map () |> Map.get_entities |> iter_entities trainer_detect

let save (save_p : Saves.save_preview) time_start =
  Saves.save_game
    {
      save_p with
      money = Player.money (State.player ());
      time = int_of_float (Unix.time ()) - time_start + save_p.time;
    };
  Animation.display_text_box "Saved the game!" false
    (fun _ ->
      draw ();
      Draw.draw_sprite Draw_text.battle_bot 0 0 ())
    ()

let rec run_tick save_preview time_start =
  trainer_action ();
  (match Input.get_ctrl_option (Input.poll_key_option ()) with
  | Some Up -> attempt_move 0 1 Player.N
  | Some Left -> attempt_move (-1) 0 Player.W
  | Some Down -> attempt_move 0 (-1) Player.S
  | Some Right -> attempt_move 1 0 Player.E
  | Some Action -> attempt_action ()
  | Some Start -> Party_menu.init OverworldMode ()
  | Some Save -> save save_preview time_start
  | Some k -> ignore k
  | None -> ());
  redraw ();
  Input.sleep Draw.tick_rate ();
  run_tick save_preview time_start

let run_overworld save_preview =
  run_tick save_preview (int_of_float (Unix.time ()))
