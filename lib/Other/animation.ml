open Draw
open Util
open Spritesheet
open DrawText
open Ui
open Input

type animation = {
  refresh_func : draw_func;
  animation : animation -> unit -> unit;
  mutable frame : int;
  mutable finished : bool;
}

let make_animation
    (rf : draw_func)
    (a : animation -> unit -> unit)
    (fr : int) =
  { refresh_func = rf; animation = a; frame = fr; finished = false }

let run = ref false

let run_animation (anim : animation) : unit =
  Ui.clear_all ();
  let rec run_animation_rec (anim : animation) =
    run := false;
    Input.sleep Draw.tick_rate ();
    anim.refresh_func ();
    anim.animation anim ();
    if !run = false then Ui.update_all ();
    run := true;
    if anim.finished then ()
    else begin
      anim.frame <- anim.frame + 1;
      run_animation_rec anim
    end
  in
  run_animation_rec anim

let rec run_text_animation
    (t : string)
    (anim : animation)
    (sticky : bool) : unit =
  Input.sleep Draw.tick_rate ();
  let key =
    match Input.pop_key_option () with
    | Some c -> Input.get_ctrl_key c
    | None -> Input.NoKey
  in
  anim.refresh_func ();
  anim.animation anim ();
  Ui.update_all ();

  if anim.finished then begin
    if sticky = false then wait 180 ()
  end
  else begin
    (match key with
    | Action -> anim.frame <- String.length t
    | _ -> anim.frame <- anim.frame + 1);
    run_text_animation t anim sticky
  end

let animate_text_box (t : string) (anim : animation) () : unit =
  let frame = anim.frame in
  let sx = 30 in
  let sy = 128 in
  let t' = String.sub t 0 frame in
  Ui.add_last_foreground (draw_text_string_pos sx sy 0 box_cap t');
  anim.finished <- frame >= String.length t

let display_text_box
    (text : string)
    (sticky : bool)
    (refresh_func : draw_func)
    () : unit =
  let rec get_text_boxes text_box c boxes = function
    | [] -> boxes @ [ text_box ]
    | h :: t ->
        if c < 3 then
          if text_box = "" then get_text_boxes h (c + 1) boxes t
          else get_text_boxes (text_box ^ " " ^ h) (c + 1) boxes t
        else get_text_boxes "" 0 (boxes @ [ text_box ]) (h :: t)
  in
  let text_boxes =
    get_text_boxes "" 0 [] (get_text_transcript text box_cap)
  in

  let rec run_text_rec = function
    | [] -> ()
    | h :: t ->
        Input.sleep Draw.tick_rate ();
        set_text_display "";

        refresh_func ();
        Ui.update_all ();
        run_text_animation h
          (make_animation refresh_func (animate_text_box h) 0)
          sticky;
        run_text_rec t;
        set_text_display h
  in

  run_text_rec text_boxes;
  if sticky = false then set_text_display ""

let hp_to_string (hp : float) =
  if hp < 10. then "  " ^ (hp |> int_of_float |> string_of_int)
  else if hp < 100. then " " ^ (hp |> int_of_float |> string_of_int)
  else hp |> int_of_float |> string_of_int

let draw_hp_val x y (curr : float) (max : float) player () =
  if player = false then ()
  else
    let combat_bg = white in
    set_color combat_bg;
    fill_rect (current_x () - 2) (current_y () - 4) 100 24;
    (DrawText.draw_string_colored x (y - 8) 0
       (hp_to_string curr ^ "/" ^ hp_to_string max)
       white text_color)
      ()

let show_hp_val = ref true

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
  if hp_text && !show_hp_val then
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
    (curr : float)
    (xh : int)
    (yh : int)
    (hwidth : int)
    (hheight : int)
    () =
  let curr = boundf curr 0. max in
  let w = float_of_int hwidth in
  let blank = rgb 20 52 100 in
  let bar_color = rgb 255 213 65 in
  set_color blank;
  fill_rect xh yh hwidth hheight;
  set_color bar_color;
  let curr_bar = curr /. max in
  fill_rect xh yh (int_of_float (curr_bar *. w)) hheight

let health_bar
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
  let frame = float_of_int anim.frame *. 1.5 in

  let new_hp =
    before
    +. (if before > after then 0. -. frame
       else if after > before then frame
       else 0.)
       *. max /. 100.
  in

  Ui.add_last_foreground (fun () ->
      if hp_text then show_hp_val := true;
      draw_health_bar max new_hp xh yh hwidth hheight hp_text ();
      if hp_text then show_hp_val := false);
  if before >= after then
    anim.finished <- int_of_float new_hp <= int_of_float after
  else anim.finished <- int_of_float new_hp >= int_of_float after

let animate_health_bar
    (max : float)
    (before : float)
    (after : float)
    (xh : int)
    (yh : int)
    (hwidth : int)
    (hheight : int)
    (hp_text : bool)
    (refresh_func : draw_func) =
  if hp_text then show_hp_val := false;
  run_animation
    (make_animation refresh_func
       (health_bar max before after xh yh hwidth hheight hp_text)
       0);
  show_hp_val := true

let exp_bar
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
  let frame = float_of_int anim.frame *. 2.5 in
  let curr_bar = (before /. max *. 100.) +. frame in
  let goal_bar = after /. max *. 100. in
  let curr_exp = curr_bar /. 100. *. max in
  Ui.add_last_foreground
    (draw_exp_bar max curr_exp xh yh hwidth hheight);
  anim.finished <- goal_bar <= curr_bar

let animate_exp_bar
    (max : float)
    (before : float)
    (after : float)
    (xh : int)
    (yh : int)
    (hwidth : int)
    (hheight : int)
    (refresh_func : draw_func) =
  run_animation
    (make_animation refresh_func
       (exp_bar max before after xh yh hwidth hheight)
       0)

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

let lower_stat_effect sprite player (anim : animation) () =
  let frame = anim.frame in
  if anim.frame = 0 then make_grayscale sprite ();
  (match frame with
  | 0 | 1 ->
      run_animation
        (make_animation anim.refresh_func
           (animate_effect sprite player (-100) (-100) 0 5)
           0);
      run_animation
        (make_animation anim.refresh_func
           (animate_effect sprite player 80 80 0 5)
           0)
  | 2 ->
      Ui.add_first_gameplay (fun () ->
          reset_rgb sprite ();
          draw_creature sprite player ())
  | _ -> ());
  anim.finished <- frame >= 2

let animate_lower_stat_effect sprite player (refresh_func : draw_func) =
  run_animation
    (make_animation refresh_func (lower_stat_effect sprite player) 0)

let raise_stat_effect sprite player (anim : animation) () =
  let frame = anim.frame in
  if anim.frame = 0 then make_grayscale sprite ();
  (match frame with
  | 0 | 1 ->
      run_animation
        (make_animation anim.refresh_func
           (animate_effect sprite player 0 0 (-200) 5)
           0);
      run_animation
        (make_animation anim.refresh_func
           (animate_effect sprite player 0 0 200 5)
           0)
  | 2 ->
      Ui.add_first_gameplay (fun () ->
          reset_rgb sprite ();
          draw_creature sprite player ())
  | _ -> ());
  anim.finished <- frame >= 2

let animate_raise_stat_effect sprite player (refresh_func : draw_func) =
  run_animation
    (make_animation refresh_func (raise_stat_effect sprite player) 0)

let switch_in
    (switching_in : sprite)
    (player : bool)
    (anim : animation)
    () =
  let frame = anim.frame in
  (match frame with
  | 0 ->
      let small3 = Draw.change_dpi switching_in 1 in
      add_rgb small3 255 255 255 ();
      Ui.add_last_gameplay (draw_creature small3 player);
      wait 15 ()
  | 1 ->
      let small4 = Draw.change_dpi switching_in 2 in
      add_rgb small4 255 255 255 ();
      Ui.add_last_gameplay (draw_creature small4 player);
      wait 15 ()
  | 2 ->
      Ui.add_last_gameplay (draw_creature switching_in player);
      Ui.add_last_gameplay (reset_rgb switching_in)
  | _ -> ());
  anim.finished <- frame >= 2

let switch_out
    (switching_out : sprite)
    (player : bool)
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
      Ui.add_last_gameplay (draw_creature small1 player)
  | 2 ->
      let small2 = Draw.change_dpi switching_out 1 in
      Ui.add_last_gameplay (draw_creature small2 player);
      Ui.add_last_gameplay (reset_rgb switching_out)
  | _ -> ());
  anim.finished <- frame >= 2

let animate_switch_in
    (switching_in : sprite)
    (player : bool)
    (refresh_func : draw_func) =
  run_animation
    (make_animation refresh_func (switch_in switching_in player) 0)

let animate_switch_out
    (switching_out : sprite)
    (player : bool)
    (refresh_func : draw_func) =
  run_animation
    (make_animation refresh_func (switch_out switching_out player) 0)

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

let capture spritesheet creature results ball_type (anim : animation) ()
    =
  let frame = anim.frame in
  let c = spritesheet.columns - 1 in
  let x, y = (540, 396) in

  (match frame with
  | 0 ->
      let toss_anim =
        Array.init 3 (fun i ->
            let ix = (i * c) + ball_type in

            get_sprite spritesheet ix)
      in
      Ui.add_last_gameplay (draw_creature creature false);
      run_animation
        (make_animation
           (fun () ->
             anim.refresh_func ();
             Ui.add_last_gameplay (draw_creature creature false))
           (animate_toss_ball toss_anim)
           0)
  | 1 ->
      let capture_anim =
        Array.init 13 (fun i ->
            if i < 12 then
              get_sprite spritesheet (((i + 3) * c) + ball_type)
            else get_sprite spritesheet ((18 * c) + ball_type))
      in
      run_animation
        (make_animation
           (fun () ->
             anim.refresh_func ();
             Ui.add_last_gameplay (draw_creature creature false))
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
  | 6 | 7 | 8 | 9 ->
      let shake_anim =
        Array.init 8 (fun i ->
            if i < 5 then
              get_sprite spritesheet (((i + 15) * c) + ball_type)
            else get_sprite spritesheet (((23 - i) * c) + ball_type))
      in
      run_animation
        (make_animation anim.refresh_func
           (animate_sprite shake_anim (x + 4) y)
           0);
      wait 5 ()
  | _ -> ());
  if frame >= 6 then
    if List.nth results (frame - 6) = false then begin
      let fail_anim =
        Array.init 7 (fun i ->
            get_sprite spritesheet (((i + 20) * c) + ball_type))
      in
      run_animation
        (make_animation anim.refresh_func
           (animate_sprite fail_anim x y)
           0);
      anim.finished <- true
    end
    else if frame = 9 then begin
      let success_anim =
        Array.init 6 (fun i ->
            get_sprite spritesheet (((i + 27) * c) + ball_type))
      in
      run_animation
        (make_animation anim.refresh_func
           (animate_sprite success_anim x y)
           0);
      anim.finished <- true
    end

let animate_capture
    spritesheet
    creature
    results
    ball_type
    (refresh_func : draw_func) =
  run_animation
    (make_animation refresh_func
       (capture spritesheet creature results ball_type)
       0)

let faint sprite player (anim : animation) () =
  let frame = (float_of_int anim.frame +. 2.) /. 2. in
  let sprite_width, sprite_height = get_dimension sprite in
  let xh, yh =
    if player then (50, 166)
    else (width - 50 - sprite_width, height - 50 - sprite_height)
  in
  Ui.add_last_gameplay (fun () ->
      draw_sprite_crop sprite xh yh (0, sprite_width)
        (0, int_of_float (float_of_int sprite_height /. frame))
        ());

  (* Ui.add_last_gameplay (fun () -> draw_sprite_crop sprite xh yh (0,
     sprite_width) ( sprite_height - int_of_float (float_of_int
     sprite_height /. frame), sprite_height ) ()); *)
  anim.finished <- frame >= 20.

let animate_faint sprite player (refresh_func : draw_func) =
  run_animation (make_animation refresh_func (faint sprite player) 0)

let damage_render sprite player (anim : animation) () =
  let frame = anim.frame / 2 in
  if frame mod 2 = 0 then
    Ui.add_last_gameplay (draw_creature sprite player);

  anim.finished <- frame = 10

let animate_damage_render
    (sprite : sprite)
    (player : bool)
    (refresh_func : draw_func) =
  run_animation
    (make_animation refresh_func (damage_render sprite player) 0)
