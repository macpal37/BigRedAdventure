open Draw
open Util
open Spritesheet
open DrawText
open Creature
open Ui

type animation = {
  refresh_func : draw_func;
  animation : animation -> unit -> unit;
  mutable frame : int;
  mutable finished : bool;
}
(** Represents a function of an animation.*)

let make_animation
    (rf : draw_func)
    (a : animation -> unit -> unit)
    (fr : int) =
  { refresh_func = rf; animation = a; frame = fr; finished = false }

let rec run_animation (anim : animation) : unit =
  Input.sleep Draw.tick_rate ();
  anim.refresh_func ();
  anim.animation anim ();
  Ui.update_all ();
  if anim.finished then () else anim.frame <- anim.frame + 1;
  run_animation anim

(* let draw_text_aniamtion (clear : unit -> unit) (anim : animation) =
   let rec run_animation_rec frame = (* TODO *) run_animation_rec (frame
   + 1) in run_animation_rec 0 *)

let hp_to_string (hp : float) =
  if hp < 10. then "  " ^ (hp |> int_of_float |> string_of_int)
  else if hp < 100. then " " ^ (hp |> int_of_float |> string_of_int)
  else hp |> int_of_float |> string_of_int

let draw_hp_val x y (curr : float) (max : float) player () =
  if player = false then ()
  else
    let combat_bg = white in
    set_color combat_bg;
    fill_rect (current_x () - 2) (current_y () + 4) 100 24;
    Ui.add_first_foreground
      (DrawText.draw_string_colored x y 0
         (hp_to_string curr ^ "/" ^ hp_to_string max)
         white text_color)

let draw_health_bar
    (max : float)
    (curr : float)
    (xh : int)
    (yh : int)
    (hwidth : int)
    (hheight : int)
    (hp_text : bool)
    () =
  let curr = boundf curr 0. max in
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
      curr max hp_text ();
  set_color blank;
  fill_rect xh yh hwidth hheight;
  let ratio = 100. *. curr /. max in
  let hp_color =
    if ratio <= 1. then blank
    else if ratio <= 20. then bar_red
    else if ratio <= 50. then bar_yellow
    else bar_green
  in
  set_color hp_color;
  fill_rect xh yh
    (int_of_float (curr /. max *. float_of_int hwidth))
    hheight

let draw_exp_bar
    (max : float)
    (before : float)
    (after : float)
    (xh : int)
    (yh : int)
    (hwidth : int)
    (hheight : int)
    () =
  let max, before, after =
    (boundf max 0. max, boundf before 0. after, boundf after 0. max)
  in
  let h = float_of_int hheight in
  let blank = rgb 20 52 100 in
  let bar_color = rgb 255 213 65 in
  let frame = anim.frame in
  set_color blank;
  Ui.add_first_gameplay (fun () -> fill_rect xh yh hwidth hheight);
  set_color bar_color;
  let curr_bar = (before /. max *. 100.) +. float_of_int frame in
  let goal_bar = after /. max *. 100. in
  Ui.add_first_gameplay (fun () ->
      fill_rect xh yh (int_of_float (curr_bar *. h /. 100.)) hheight);
  anim.finished <- goal_bar <= curr_bar

let animate_health_bar
    (max : float)
    (before : float)
    (after : float)
    (xh : int)
    (yh : int)
    (hwidth : int)
    (hheight : int)
    (hp_text : bool)
    (anim : animation)
    () =
  let max, before, after =
    (boundf max 0. max, boundf before 0. max, boundf after 0. max)
  in
  let frame = anim.frame in
  let blank = rgb 84 97 89 in
  let bar_yellow = rgb 221 193 64 in
  let bar_red = rgb 246 85 55 in
  let bar_green = rgb 103 221 144 in
  let new_hp =
    before
    +. float_of_int
         (if before > after then -frame
         else if after > before then frame
         else 0)
  in
  Ui.add_last_foreground (fun () ->
      set_color text_color;
      set_line_width 8;
      draw_rect xh yh hwidth hheight);
  if hp_text then
    draw_hp_val
      (xh + (hwidth / 2))
      (yh - hheight - 5 - 22)
      new_hp max hp_text ();

  Ui.add_last_foreground (fun () ->
      set_color blank;
      fill_rect xh yh hwidth hheight);
  let ratio = 100. *. new_hp /. max in
  let hp_color =
    if ratio <= 1. then blank
    else if ratio <= 20. then bar_red
    else if ratio <= 50. then bar_yellow
    else bar_green
  in

  Ui.add_last_foreground (fun () ->
      set_color hp_color;
      fill_rect xh yh
        (int_of_float (new_hp /. max *. float_of_int hwidth))
        hheight);

  anim.finished <- int_of_float new_hp = int_of_float after

let animate_exp_bar
    (max : float)
    (before : float)
    (after : float)
    (xh : int)
    (yh : int)
    (hwidth : int)
    (hheight : int)
    (anim : animation)
    () =
  let max, before, after =
    (boundf max 0. max, boundf before 0. after, boundf after 0. max)
  in
  let h = float_of_int hheight in
  let blank = rgb 20 52 100 in
  let bar_color = rgb 255 213 65 in
  let frame = anim.frame in
  set_color blank;
  Ui.add_first_gameplay (fun () -> fill_rect xh yh hwidth hheight);
  set_color bar_color;
  let curr_bar = (before /. max *. 100.) +. float_of_int frame in
  let goal_bar = after /. max *. 100. in
  Ui.add_first_gameplay (fun () ->
      fill_rect xh yh (int_of_float (curr_bar *. h /. 100.)) hheight);
  anim.finished <- goal_bar <= curr_bar

let animate_effect
    sprite
    player
    red
    green
    blue
    value
    (anim : animation)
    () =
  let frame = anim.frame in
  let rr = red / value in
  let gg = green / value in
  let bb = blue / value in
  Ui.add_first_gameplay (fun () ->
      add_rgb sprite rr gg bb ();
      draw_creature sprite player ());
  anim.finished <- frame = value

let animate_lower_stat_effect sprite player (anim : animation) () =
  let frame = anim.frame in
  if anim.frame = 0 then make_grayscale sprite ();
  match frame with
  | 0 | 1 ->
      run_animation
        (make_animation anim.refresh_func
           (animate_effect sprite player (-100) (-100) 0 5)
           0);
      run_animation
        (make_animation anim.refresh_func
           (animate_effect sprite player 80 80 0 5)
           0);
      false
  | 2 ->
      Ui.add_first_gameplay (fun () ->
          reset_rgb sprite ();
          draw_creature sprite player ());
      true
  | _ -> true

let animate_raise_stat_effect sprite player (anim : animation) () =
  let frame = anim.frame in
  if anim.frame = 0 then make_grayscale sprite ();
  match frame with
  | 0 | 1 ->
      run_animation
        (make_animation anim.refresh_func
           (animate_effect sprite player 0 0 (-200) 5)
           0);
      run_animation
        (make_animation anim.refresh_func
           (animate_effect sprite player 0 0 200 5)
           0);
      false
  | 2 ->
      Ui.add_first_gameplay (fun () ->
          reset_rgb sprite ();
          draw_creature sprite player ());
      true
  | _ -> true

let switch_out
    (switching_out : sprite)
    (switching_in : sprite)
    (player : bool)
    (name_out : string)
    (name_in : string)
    (anim : animation)
    () =
  let frame = anim.frame in

  (match frame with
  | 0 ->
      run_animation
        (make_animation anim.refresh_func
           (animate_effect switching_out player 255 255 255 4)
           0)
  | 1 ->
      let small1 = Draw.change_dpi switching_out 2 in
      draw_creature small1 player ()
  | 2 ->
      let small2 = Draw.change_dpi switching_out 1 in
      draw_creature small2 player ()
  | 3 -> draw_text ("Come back " ^ name_in ^ "!") 40 true false ()
  | 4 ->
      let small3 = Draw.change_dpi switching_in 1 in
      add_rgb small3 255 255 255 ();
      draw_creature small3 player ()
  | 5 ->
      let small4 = Draw.change_dpi switching_in 2 in
      add_rgb small4 255 255 255 ();
      draw_creature small4 player ()
  | 6 -> draw_text ("Go " ^ name_out ^ "!") 40 true false ()
  | 7 ->
      draw_creature switching_in player ();
      reset_rgb switching_out ();
      reset_rgb switching_in ()
  | _ -> ());
  anim.finished <- frame = 7

let animate_sprite (ss : sprite array) x y (anim : animation) () =
  let frame = anim.frame in
  Ui.add_last_gameplay (draw_sprite (Array.get ss frame) x y);
  anim.finished <- frame = Array.length ss - 1

let animate_toss_ball (ss : sprite array) (anim : animation) () =
  let frame = anim.frame in
  let x = 114 + (20 * frame) in
  let y = ((x - 375) * (x - 375) / -250) + 500 in
  Ui.add_last_gameplay (draw_sprite (Array.get ss (frame mod 3)) x y);
  anim.finished <- frame = 21

let animate_capture
    spritesheet
    creature
    results
    pokeball
    (anim : animation)
    () =
  let frame = anim.frame in
  let c = spritesheet.columns in
  let x, y = (510 + 30, 430 - 34) in

  (match frame with
  | 0 ->
      let toss_anim =
        Array.init 3 (fun i ->
            get_sprite spritesheet ((i * c) + pokeball))
      in
      run_animation
        (make_animation anim.refresh_func
           (animate_toss_ball toss_anim)
           0)
  | 1 ->
      let capture_anim =
        Array.init 13 (fun i ->
            if i < 12 then
              get_sprite spritesheet (((i + 3) * c) + pokeball)
            else get_sprite spritesheet ((18 * c) + pokeball))
      in
      run_animation
        (make_animation anim.refresh_func
           (animate_sprite capture_anim x (y + 4))
           0)
  | 2 ->
      run_animation
        (make_animation anim.refresh_func
           (animate_effect creature false 255 255 255 4)
           0)
  | 3 ->
      let small1 = Draw.change_dpi creature 2 in
      Ui.add_last_gameplay (draw_sprite small1 (x + 20) (y + 40))
  | 4 ->
      let small2 = Draw.change_dpi creature 1 in
      Ui.add_last_gameplay (draw_sprite small2 (x + 50) (y + 60))
  | 5 -> reset_rgb creature ()
  | 6 | 7 | 8 ->
      let shake_anim =
        Array.init 8 (fun i ->
            if i < 5 then
              get_sprite spritesheet (((i + 15) * c) + pokeball)
            else get_sprite spritesheet (((23 - i) * c) + pokeball))
      in
      run_animation
        (make_animation anim.refresh_func
           (animate_sprite shake_anim (x + 4) y)
           0)
  | _ -> ());
  if frame >= 6 then
    if List.nth results (frame - 6) = false then begin
      let fail_anim =
        Array.init 7 (fun i ->
            get_sprite spritesheet (((i + 20) * c) + pokeball))
      in
      run_animation
        (make_animation anim.refresh_func
           (animate_sprite fail_anim x y)
           0);
      anim.finished <- true
    end
    else if frame = 8 then begin
      let success_anim =
        Array.init 6 (fun i ->
            get_sprite spritesheet (((i + 27) * c) + pokeball))
      in
      run_animation
        (make_animation anim.refresh_func
           (animate_sprite success_anim x y)
           0);
      anim.finished <- true
    end

let animate_faint sprite player (anim : animation) () =
  let frame = anim.frame + 1 in
  let sprite_width, sprite_height = get_dimension sprite in
  let xx, yy =
    if player then (50, 166)
    else (width - 50 - sprite_width, height - 50 - sprite_height)
  in
  draw_sprite_crop sprite xx
    (yy - (sprite_height - (sprite_height / frame)))
    (0, sprite_width)
    (sprite_height - (sprite_height / frame), sprite_height)
    ();
  anim.finished <- frame = 20

let animate_damage_render player_sprite player (anim : animation) () =
  let frame = anim.frame in
  if frame mod 2 = 0 then draw_creature player_sprite player ();
  anim.finished <- frame = 8
