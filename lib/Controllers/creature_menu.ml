open Draw
open Creature
open Animation
open DrawText
open Input
open Util

let creature_menu_bg = Util.null ()
let menu_position = Util.new_point ()
let party_position = Util.new_point ()
let current_creature = ref null_creature
let switch_position = Util.new_point ()
let faint = Util.null ()

let load_assets _ =
  creature_menu_bg
  *= Sprite_assets.get_sprite2 "creature_menu" GUI_Folder;
  faint *= Sprite_assets.get_sprite2 "faint" GUI_Folder

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

let draw_status status () =
  let x, y = (56 - 20, 192 - 30) in
  match status with
  | Fainted -> draw_sprite ~!faint x y ()
  | _ -> ()

let draw_stats () =
  let x, y, dif, x2 = (290 - 4, 623 - 8, 35, 556) in
  let _, buff, nerf = get_nature !current_creature in
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
         Medium (string_of_stat s) color text_color);
    Ui.add_last_foreground
      (draw_string_colored x2
         (y - (dif * (i + 1)))
         Medium
         (Util.string_of_intf (get_stat !current_creature s))
         color text_color)
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
  let moves = get_moves !current_creature in
  let size = Array.length moves in
  (* let box_w, box_h = (249, 90) in let box_x, box_y = (20, 200 -
     box_h) in *)
  let text_x, text_y = (289, 326) in
  let box_w, box_h = (249, 90) in
  set_line_width 6;
  let rec draw_all_moves i j length =
    if length <= 0 then ()
    else
      let move = get_move_i !current_creature (i + (j * 2)) in
      (match move with
      | None -> ()
      | Some m ->
          let x, y = (text_x + (box_w * i), text_y - (box_h * j)) in
          Ui.add_first_foreground
            (draw_string_colored x y Medium m.move_name
               (get_color_from_etype m.etype)
               text_color);

          Ui.add_first_foreground
            (draw_string_colored x (y - 30) Small
               (string_of_etype m.etype)
               (get_color_from_etype m.etype)
               text_color);

          set_color text_color;
          Ui.add_first_foreground
            (draw_string_colored (x + 120) (y - 30) Small
               ("PP:" ^ string_of_int m.curr_pp ^ "/"
              ^ string_of_int m.max_pp)
               white text_color));

      if i = 1 then draw_all_moves 0 (j + 1) (length - 1)
      else draw_all_moves (i + 1) j (length - 1)
  in
  draw_all_moves 0 0 size;
  Ui.add_last_foreground (draw_selector box_w box_h)

let refresh () =
  Ui.add_first_foreground
    (draw_string_colored 18 590 Huge "SUMMARY" (rgb 255 170 40) white);
  Ui.add_last_background (draw_sprite ~!creature_menu_bg 0 (-3));
  Ui.add_first_gameplay
    (draw_sprite (get_front_sprite !current_creature) 9 318);
  Ui.add_first_gameplay
    (draw_string_colored 20 246 Medium
       (get_nickname !current_creature)
       white text_color);
  Ui.add_first_gameplay
    (draw_string_colored 20 220 Small
       ("LVL: " ^ string_of_int (get_level !current_creature))
       white text_color);
  let max, aft = get_hp_status !current_creature in
  Ui.add_first_gameplay (draw_health_bar max aft 56 196 180 6 true);
  Ui.add_first_gameplay (draw_status (get_status !current_creature));
  let curr_exp, min_exp, max_exp = get_exp !current_creature in
  Ui.add_first_gameplay
    (draw_string_colored 20 148 Small "EXP:" white text_color);
  Ui.add_first_gameplay
    (draw_exp_bar (max_exp -. min_exp) (curr_exp -. min_exp) 20 136 210
       8);
  Ui.add_first_gameplay
    (draw_string_colored 20
       (104 - 21 + 16)
       Small
       (Util.string_of_intf (curr_exp -. min_exp)
       ^ "/"
       ^ Util.string_of_intf (max_exp -. min_exp))
       white text_color);

  let type1, type2 = get_types !current_creature in

  Ui.add_first_gameplay
    (draw_string_colored 20 (53 + 16) Small "TYPE: " white text_color);

  let type_str =
    string_of_etype type1 ^ if type2 = NoType then "" else "/"
  in
  Ui.add_first_gameplay
    (draw_string_colored 116 (56 + 16) Small type_str
       (Creature.get_color_from_etype type1)
       text_color);
  draw_stats ();
  let nature, _, _ = get_nature !current_creature in
  if type2 <> NoType then begin
    Ui.add_first_gameplay
      (draw_string_colored 116 (28 + 16) Small (string_of_etype type2)
         (Creature.get_color_from_etype type2)
         text_color);
    draw_stats ();
    Ui.add_first_gameplay
      (draw_string_colored 20 20 Small ("NATURE: " ^ nature) white
         text_color)
  end
  else
    Ui.add_first_gameplay
      (draw_string_colored 20 (24 + 16) Small ("NATURE: " ^ nature)
         white text_color);

  draw_stats ();
  (* Ui.add_first_gameplay (draw_string_colored 20 100 1 24 type_str
     white); *)
  Ui.add_first_foreground
    (draw_string_colored 482 383 Medium "Moves" white text_color);
  Ui.add_first_foreground
    (draw_string_colored 398 636 Medium "Stats" white text_color);
  (match
     get_move_i !current_creature
       (menu_position.x + (2 * menu_position.y))
   with
  | None -> ()
  | Some m ->
      Ui.add_first_foreground
        (draw_text_string_pos 290 120 Small 32 m.description));
  draw_moves ()

let set_creature i =
  party_position.x <- i;
  current_creature := Player.party_i (State.player ()) i

let next_creature i =
  let size = List.length (Player.party (State.player ())) in
  let x = i + party_position.x in
  let i = if x >= size then 0 else if x < 0 then size - 1 else x in
  set_creature i;
  refresh ()

let switch_move () =
  let i1, i2 =
    ( menu_position.x + (2 * menu_position.y),
      switch_position.x + (2 * switch_position.y) )
  in
  let a, b =
    (get_move_i !current_creature i1, get_move_i !current_creature i2)
  in
  match a with
  | None -> ()
  | _ -> (
      match b with
      | None -> ()
      | _ ->
          (get_moves !current_creature).(i1) <- b;
          (get_moves !current_creature).(i2) <- a)

(* let moves = get_moves !current_creature in let rec switch_move
   new_lst = function | [] -> new_lst | h :: t -> if a = h then
   switch_move (new_lst @ [ b ]) t else if b = h then switch_move
   (new_lst @ [ a ]) t else switch_move (new_lst @ [ h ]) t in let
   new_moves = switch_move [] moves in set_moves !current_creature
   new_moves *)

let rec run_tick () =
  Input.sleep Draw.tick_rate ();
  let key =
    match Input.pop_key_option () with
    | Some c -> get_ctrl_key c
    | None -> NoKey
  in
  if menu_position.x = -3 then menu_position.x <- -2;

  if menu_position.x <> -2 then begin
    if key = Up then move_y (-1) ();
    if key = Down then move_y 1 ();
    if key = Left then move_x (-1) ();
    if key = Right then move_x 1 ()
  end
  else begin
    if key = Left then next_creature (-1);
    if key = Right then next_creature 1
  end;

  (*====== Switch ====== *)
  if key = Action && menu_position.x >= 0 && switch_position.x = -1 then begin
    switch_position.x <- menu_position.x + 0;
    switch_position.y <- menu_position.y + 0
  end
  else if
    key = Action && switch_position.x <> -1 && menu_position.x <> -2
  then begin
    switch_move ();
    menu_position.x <- switch_position.x + 0;
    menu_position.y <- switch_position.y + 0;

    switch_position.x <- -1;
    refresh ()
  end;

  if key = Action && menu_position.x = -2 then begin
    menu_position.x <- 0;
    refresh ();
    draw_moves ()
  end;
  if key = Back && menu_position.x <> -2 then begin
    menu_position.x <- -3;
    refresh ()
  end;

  refresh ();

  Ui.update_all ();
  if key <> Back || menu_position.x <> -2 then run_tick ()

let init () =
  menu_position.x <- -2;
  switch_position.x <- -1;
  ignore (Input.poll_key_option ());
  run_tick ()
