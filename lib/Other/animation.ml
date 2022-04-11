open Draw
open Graphics
open Util
open Spritesheet

let d a b c =
  let x = a * b in
  x / c

let hp_to_string hp =
  if hp < 10 then "  " ^ string_of_int hp
  else if hp < 100 then " " ^ string_of_int hp
  else string_of_int hp

let draw_hp_val x y curr max player animate () =
  if player = false then ()
  else begin
    if animate then auto_synchronize false;
    let combat_bg = point_color x y in
    set_font_size 30 ();
    moveto x y;
    set_color combat_bg;
    fill_rect (current_x () - 2) (current_y () + 4) 100 24;
    set_color white;
    draw_string (hp_to_string curr ^ "/" ^ hp_to_string max);
    if animate then auto_synchronize true
  end

let draw_health_bar
    max
    before
    after
    xh
    yh
    hwidth
    hheight
    hp_text
    animate
    () =
  if animate then auto_synchronize true;
  let max, before, after =
    (bound max 0 max, bound before 0 max, bound after 0 max)
  in
  let blank = rgb 84 97 89 in
  let bar_yellow = rgb 221 193 64 in
  let bar_red = rgb 246 85 55 in
  let bar_green = rgb 103 221 144 in
  set_color text_color;
  set_line_width 8;
  draw_rect xh yh hwidth hheight;
  if hp_text then
    draw_hp_val
      (xh + (hwidth / 2))
      (yh - hheight - 5 - 22)
      before max hp_text animate ();
  set_color blank;
  fill_rect xh yh hwidth hheight;
  let ratio = d before 100 max in
  if ratio <= 1 then set_color blank
  else if ratio <= 20 then set_color bar_red
  else if ratio <= 50 then set_color bar_yellow
  else set_color bar_green;
  let before_bar = d before 100 max in
  fill_rect xh yh (d before_bar hwidth 100) hheight;

  let after_bar = d after 100 max in
  if before <> after then begin
    let rec render_health start target =
      if start = target || start <= 0 then ()
      else if start > target then begin
        (*=====LOSING HEALTH=====*)
        if start <= 1 then set_color blank
        else if start <= 20 then set_color bar_red
        else if start <= 50 then set_color bar_yellow
        else set_color bar_green;
        fill_rect xh yh (d start hwidth 100) hheight;
        set_color blank;
        fill_rect
          (xh + d start hwidth 100)
          yh
          (hwidth - d start hwidth 100)
          hheight;
        (* HP NUMBER *)
        draw_hp_val
          (xh + (hwidth / 2))
          (yh - hheight - 5 - 22)
          (d max start 100) max hp_text animate ();
        Input.sleep 0.025 ();
        render_health (start - 1) target
      end
      else begin
        (*=====Gaining HEALTH=====*)
        if start == 1 then set_color blank
        else if start <= 20 then set_color bar_red
        else if start <= 50 then set_color bar_yellow
        else set_color bar_green;
        (* HP NUMBER *)
        draw_hp_val
          (xh + (hwidth / 2))
          (yh - hheight - 5 - 22)
          (d max start 100) max hp_text animate ();

        fill_rect xh yh (d hwidth start 100) hheight;
        Input.sleep 0.025 ();
        render_health (start + 1) target
      end
    in
    render_health before_bar after_bar;

    draw_hp_val
      (xh + (hwidth / 2))
      (yh - hheight - 5 - 22)
      (if after >= 0 then after else 0)
      max hp_text animate ()
  end;
  if animate then auto_synchronize false

let draw_exp_bar max before after xh yh hwidth hheight () =
  if before <> after then auto_synchronize true;
  let max, before, after =
    (bound max 0 max, bound before 0 after, bound after 0 max)
  in
  let blank = rgb 20 52 100 in
  let bar_color = rgb 255 213 65 in

  set_color text_color;
  (* set_line_width 8; draw_rect xh yh hwidth hheight; *)
  set_color blank;
  fill_rect xh yh hwidth hheight;
  set_color bar_color;
  let before_bar = d before 100 max in
  fill_rect xh yh (d before_bar hwidth 100) hheight;

  let after_bar = d after 100 max in
  (if before <> after then
   let rec render_bar_progress start target =
     if start = target || start < 0 then ()
     else if start <= target then begin
       set_color bar_color;
       fill_rect xh yh (d start hwidth 100 + 4) hheight;
       (* set_color blank; fill_rect (xh + d start hwidth 100) yh 2
          hheight; *)
       Input.sleep 0.02 ();
       render_bar_progress (start + 1) target
     end
   in
   render_bar_progress before_bar after_bar);

  set_color text_color;
  if before <> after then auto_synchronize false

let draw_creature_effect sprite player red green blue value () =
  let rec effect count =
    let rr = red / value in
    let gg = green / value in
    let bb = blue / value in
    add_rgb sprite rr gg bb ();
    draw_creature sprite player ();
    Input.sleep 0.025 ();
    if count > 0 then effect (count - 1) else ()
  in
  effect value

let switch_out
    switching_out
    switching_in
    player
    name_in
    name_out
    clear_function
    () =
  clear_text battle_bot ();
  Draw.sync_draw clear_function ();
  let time = 0.075 in
  set_synced_mode true;
  draw_creature_effect switching_out player 255 255 255 4 ();
  set_synced_mode false;
  Draw.sync_draw clear_function ();
  let small1 = Draw.change_dpi switching_out 2 in
  draw_creature small1 player ();
  Input.sleep time ();
  Draw.sync_draw clear_function ();
  let small2 = Draw.change_dpi switching_out 1 in
  draw_creature small2 player ();
  Input.sleep time ();
  Draw.sync_draw clear_function ();
  Input.sleep time ();

  draw_text ("Come back " ^ name_in ^ "!") 40 true false ();
  let small3 = Draw.change_dpi switching_in 1 in
  add_rgb small3 255 255 255 ();
  draw_creature small3 player ();
  Input.sleep time ();
  Draw.sync_draw clear_function ();
  let small4 = Draw.change_dpi switching_in 2 in
  add_rgb small4 255 255 255 ();
  draw_creature small4 player ();
  Input.sleep time ();
  Draw.sync_draw clear_function ();
  draw_creature switching_in player ();
  draw_text ("Go " ^ name_out ^ "!") 40 true false ();
  reset_rgb switching_out ();
  reset_rgb switching_in ()

let lower_stat_effect sprite player () =
  set_synced_mode true;
  make_grayscale sprite ();

  for _ = 0 to 2 do
    draw_creature_effect sprite player (-100) (-100) 0 5 ();
    draw_creature_effect sprite player 80 80 0 5 ()
  done;
  reset_rgb sprite ();
  draw_creature sprite player ();
  set_synced_mode false

let raise_stat_effect sprite player () =
  set_synced_mode true;
  make_grayscale sprite ();
  for _ = 0 to 2 do
    draw_creature_effect sprite player 0 0 (-200) 5 ();
    draw_creature_effect sprite player 0 0 200 5 ();
    clear_text battle_bot ()
  done;
  reset_rgb sprite ();
  draw_creature sprite player ();
  clear_text battle_bot ();
  set_synced_mode false

let play_animation anim x y delay clear_function () =
  for i = 0 to Array.length anim - 1 do
    sync_draw clear_function ();
    sync_draw (draw_sprite (Array.get anim i) x y) ();
    Input.sleep delay ()
  done

let toss_ball_animation anim delay clear_function () =
  for i = 0 to 21 do
    let x = 114 + (20 * i) in
    let y = ((x - 375) * (x - 375) / -250) + 500 in

    sync_draw clear_function ();
    sync_draw (draw_sprite (Array.get anim (i mod 3)) x y) ();
    Input.sleep delay ()
  done

let capture_animation
    spritesheet
    creature
    results
    pokeball
    clear_function
    () =
  let c = spritesheet.columns in
  print_endline ("Columns: " ^ string_of_int c);
  let x, y = (510 + 30, 430 - 34) in
  let toss_anim =
    Array.init 3 (fun i -> get_sprite spritesheet ((i * c) + pokeball))
  in
  let capture_anim =
    Array.init 13 (fun i ->
        if i < 12 then get_sprite spritesheet (((i + 3) * c) + pokeball)
        else get_sprite spritesheet ((18 * c) + pokeball))
  in
  let shake_anim =
    Array.init 8 (fun i ->
        if i < 5 then get_sprite spritesheet (((i + 15) * c) + pokeball)
        else get_sprite spritesheet (((23 - i) * c) + pokeball))
  in

  let fail_anim =
    Array.init 7 (fun i ->
        get_sprite spritesheet (((i + 20) * c) + pokeball))
  in
  let success_anim =
    Array.init 6 (fun i ->
        get_sprite spritesheet (((i + 27) * c) + pokeball))
  in
  toss_ball_animation toss_anim 0.015 (clear_function 2) ();

  play_animation capture_anim x (y + 4) 0.03 (clear_function 2) ();

  let time = 0.075 in
  set_synced_mode true;
  draw_creature_effect creature false 255 255 255 4 ();
  set_synced_mode false;
  Draw.sync_draw (clear_function 0) ();
  let small1 = Draw.change_dpi creature 2 in
  draw_sprite small1 (x + 20) (y + 40) ();
  Input.sleep time ();
  Draw.sync_draw (clear_function 0) ();
  let small2 = Draw.change_dpi creature 1 in
  draw_sprite small2 (x + 50) (y + 60) ();
  Input.sleep time ();
  Draw.sync_draw (clear_function 0) ();
  Input.sleep time ();
  let time = 0.05 in
  reset_rgb creature ();
  let rec handle_shakes = function
    | [] -> play_animation success_anim x y 0.02 (clear_function 0) ()
    | h :: t ->
        if h then begin
          if List.length t <= 3 then
            play_animation shake_anim (x + 4) y 0.03 (clear_function 0)
              ();

          Input.sleep (time *. 2.) ();
          handle_shakes t
        end
        else begin
          Input.sleep (time *. 2.) ();
          play_animation fail_anim x y 0.02 (clear_function 0) ();
          sync_draw (clear_function 2) ()
        end
  in
  handle_shakes results;
  auto_synchronize false;
  set_synced_mode false
