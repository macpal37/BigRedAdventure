open Draw
open Graphics
open Util

let d a b c =
  let x = a * b in
  x / c

let hp_to_string hp =
  if hp < 10 then "  " ^ string_of_int hp
  else if hp < 100 then " " ^ string_of_int hp
  else string_of_int hp

let draw_hp_val x y curr max player () =
  if player = false then ()
  else
    let combat_bg = point_color x y in
    set_font_size 30 ();
    moveto x y;
    set_color combat_bg;
    fill_rect (current_x () - 2) (current_y () + 4) 100 24;
    set_color white;
    draw_string (hp_to_string curr ^ "/" ^ hp_to_string max)

let draw_health_bar max before after xh yh hwidth hheight hp_text () =
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
      before max hp_text ();
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
          (d max start 100) max hp_text ();
        Input.sleep 0.05 ();
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
          (d max start 100) max hp_text ();

        fill_rect xh yh (d hwidth start 100) hheight;
        Input.sleep 0.005 ();
        render_health (start + 1) target
      end
    in
    render_health before_bar after_bar;

    draw_hp_val
      (xh + (hwidth / 2))
      (yh - hheight - 5 - 22)
      (if after >= 0 then after else 0)
      max hp_text ()
  end

let draw_exp_bar max before after xh yh hwidth hheight () =
  (* Draw.sync true (); *)
  let max, before, after =
    (bound max 0 max, bound before 0 after, bound after 0 max)
  in
  let blank = rgb 209 199 156 in
  let bar_color = rgb 77 195 232 in

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
       Input.sleep 0.05 ();
       render_bar_progress (start + 1) target
     end
   in
   render_bar_progress before_bar after_bar);
  (* Draw.sync false (); *)
  set_color text_color
