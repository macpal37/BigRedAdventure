open Draw
open Graphics
open Creature
open Animation

let creature_menu_bg = load_sprite "creature_menu" GUI_Folder 3 ()
let menu_position = Util.new_point ()
let party_position = Util.new_point ()
let current_creature = ref (create_creature "clefairy" 100)
let switch_position = Util.new_point ()

let move_x x () =
  if switch_position.x = -1 then begin
    if menu_position.x + x >= 0 && menu_position.x + x <= 1 then
      menu_position.x <- menu_position.x + x
  end
  else if switch_position.x + x >= 0 && switch_position.x + x <= 1 then
    switch_position.x <- switch_position.x + x

let move_y y () =
  if switch_position.x = -1 then begin
    if menu_position.y + y >= 0 && menu_position.y + y <= 1 then
      menu_position.y <- menu_position.y + y
  end
  else if switch_position.y + y >= 0 && switch_position.y + y <= 1 then
    switch_position.y <- switch_position.y + y

let draw_stats () =
  let x, y, dif, x2 = (290, 623, 35, 556) in
  let _, buff, nerf = get_nature current_creature.contents in
  let buff_color = rgb 180 32 42 in
  let nerf_color = rgb 121 58 128 in
  let normal = white in
  let stat_lst = [ Attack; Defense; Sp_Attack; Sp_Defense; Speed ] in
  for i = 0 to 4 do
    let s = List.nth stat_lst i in
    let color =
      if s = buff && buff <> nerf then buff_color
      else if s = nerf && buff <> nerf then nerf_color
      else normal
    in
    Ui.add_last_foreground
      (draw_string_colored x
         (y - (dif * (i + 1)))
         1 30 (string_of_stat s) color);
    Ui.add_last_foreground
      (draw_string_colored x2
         (y - (dif * (i + 1)))
         1 30
         (string_of_int (get_stat current_creature.contents s))
         color)
  done

let draw_selector w h () =
  if menu_position.x >= 0 then begin
    set_line_width 6;
    let x, y =
      (282 + (w * menu_position.x), 274 - (h * menu_position.y))
    in
    set_color red;
    draw_rect x y w h
  end;
  if switch_position.x <> -1 then begin
    set_line_width 6;
    let x, y =
      (282 + (w * switch_position.x), 274 - (h * switch_position.y))
    in
    set_color blue;
    draw_rect x y w h
  end

let draw_moves () =
  let moves = get_moves current_creature.contents in
  let size = List.length moves in
  (* let box_w, box_h = (249, 90) in let box_x, box_y = (20, 200 -
     box_h) in *)
  let text_x, text_y = (289, 326) in
  let box_w, box_h = (249, 90) in
  set_line_width 6;
  let rec draw_all_moves i j length =
    if length <= 0 then ()
    else
      let move = List.nth moves (i + (j * 2)) in
      let x, y = (text_x + (box_w * i), text_y - (box_h * j)) in
      Ui.add_first_foreground
        (draw_string_colored x y 2 28 move.move_name
           (get_color_from_etype move.etype));

      Ui.add_first_foreground
        (draw_string_colored x (y - 30) 1 24
           (string_of_etype move.etype)
           (get_color_from_etype move.etype));

      set_color text_color;
      Ui.add_first_foreground
        (draw_string_colored (x + 120) (y - 30) 1 24
           ("PP:"
           ^ string_of_int move.curr_pp
           ^ "/"
           ^ string_of_int move.max_pp)
           white);

      if i = 1 then draw_all_moves 0 (j + 1) (length - 1)
      else draw_all_moves (i + 1) j (length - 1)
  in
  draw_all_moves 0 0 size;
  Ui.add_last_foreground (draw_selector box_w box_h)

let refresh () =
  Ui.add_first_foreground
    (draw_string_colored 24 605 4 60 "SUMMARY" (rgb 255 170 40));
  Ui.add_last_background (draw_sprite creature_menu_bg 0 0);
  Ui.add_first_gameplay
    (draw_sprite (get_front_sprite current_creature.contents) 12 318);
  Ui.add_first_gameplay
    (draw_string_colored 20 246 1 30
       (get_nickname current_creature.contents)
       white);
  Ui.add_first_gameplay
    (draw_string_colored 20 220 1 20
       ("LVL: " ^ string_of_int (get_level current_creature.contents))
       white);
  let max, _, aft = get_hp_status current_creature.contents in
  Ui.add_first_gameplay (draw_health_bar max aft aft 56 192 180 6 true);
  let type1, type2 = get_types current_creature.contents in
  let type_str =
    string_of_etype type1
    ^ if type2 = None then "" else "/" ^ string_of_etype type2
  in
  Ui.add_first_gameplay
    (draw_string_colored 20 132 1 24 ("TYPE: " ^ type_str) white);
  draw_stats ();
  let nature, _, _ = get_nature current_creature.contents in
  Ui.add_first_gameplay
    (draw_string_colored 20 102 1 24 ("NATURE: " ^ nature) white);
  draw_stats ();
  (* Ui.add_first_gameplay (draw_string_colored 20 100 1 24 type_str
     white); *)
  Ui.add_first_foreground
    (draw_string_colored 482 383 2 36 "Moves" white);
  Ui.add_first_foreground
    (draw_string_colored 398 636 2 36 "Stats" white);
  Ui.add_first_foreground
    (draw_text_string_pos 290 120 30 32
       (get_move_description_i current_creature.contents
          (menu_position.x + (2 * menu_position.y)))
       text_color2);
  draw_moves ()

let set_creature i =
  party_position.x <- i;
  current_creature.contents <- Player.party_i (State.player ()) i

let next_creature i =
  let size = List.length (Player.party (State.player ())) in
  let x = i + party_position.x in
  let i = if x >= size then 0 else if x < 0 then size - 1 else x in
  set_creature i;
  refresh ()

let switch_move () =
  let a, b =
    ( get_move_i current_creature.contents
        (menu_position.x + (2 * menu_position.y)),
      get_move_i current_creature.contents
        (switch_position.x + (2 * switch_position.y)) )
  in
  let moves = get_moves current_creature.contents in
  let rec switch_move new_lst = function
    | [] -> new_lst
    | h :: t ->
        if a = h then switch_move (new_lst @ [ b ]) t
        else if b = h then switch_move (new_lst @ [ a ]) t
        else switch_move (new_lst @ [ h ]) t
  in
  let new_moves = switch_move [] moves in
  set_moves current_creature.contents new_moves

let rec run_tick () =
  Input.poll ();
  let key =
    match Input.key_option () with
    | Some c -> c
    | None -> '#'
  in
  if menu_position.x = -3 then menu_position.x <- -2;

  if menu_position.x <> -2 then begin
    if key = 'w' then move_y (-1) ();
    if key = 's' then move_y 1 ();
    if key = 'a' then move_x (-1) ();
    if key = 'd' then move_x 1 ()
  end
  else begin
    if key = 'a' then next_creature (-1);
    if key = 'd' then next_creature 1
  end;

  (*====== Switch ====== *)
  if key = 'e' && menu_position.x >= 0 && switch_position.x = -1 then begin
    switch_position.x <- menu_position.x + 0;
    switch_position.y <- menu_position.y + 0;
    refresh ()
  end
  else if key = 'e' && switch_position.x <> -1 && menu_position.x <> -2
  then begin
    switch_move ();
    menu_position.x <- switch_position.x + 0;
    menu_position.y <- switch_position.y + 0;

    switch_position.x <- -1;
    refresh ()
  end;

  if key = 'e' && menu_position.x = -2 then begin
    menu_position.x <- 0;
    refresh ();
    draw_moves ()
  end;
  if key = 'q' && menu_position.x <> -2 then begin
    menu_position.x <- -3;
    refresh ()
  end;

  if key = 'w' || key = 's' || key = 'd' || key = 'a' then refresh ();

  Ui.update_all ();
  if key <> 'q' || menu_position.x <> -2 then run_tick ()

let init () =
  set_synced_mode false;
  menu_position.x <- -2;
  switch_position.x <- -1;

  refresh ();
  Ui.add_first_background clear_screen;
  run_tick ()
