open Util

let main_box = Util.null ()
let title_sprite = Util.null ()
let position = Util.new_point ()

let load_assets _ =
  main_box *= Draw.load_sprite "main_menu" GUI_Folder 3 ();
  title_sprite *= Draw.load_sprite "title" GUI_Folder 3 ()

let draw_title _ =
  Draw.draw_sprite_centered ~!title_sprite (Draw.width / 2) 630 ()

let draw_box _ =
  Draw.draw_sprite_centered ~!main_box (Draw.width / 2) 270 ();
  DrawText.draw_string_colored 232
    (358 - (position.y * 104))
    DrawText.Medium ">" Draw.white Draw.text_color ();
  DrawText.draw_string_colored 232 354 DrawText.Medium "  New Game"
    Draw.white Draw.text_color ();
  DrawText.draw_string_colored 232 250 DrawText.Medium "  Continue"
    Draw.white Draw.text_color ();
  DrawText.draw_string_colored 232 146 DrawText.Medium "  Exit"
    Draw.white Draw.text_color ()

let action _ =
  for i = 0 to 28 do
    Draw.set_color Draw.white;
    Draw.fill_rect 0 0 Draw.width Draw.height;
    draw_title ();
    draw_box ();
    Draw.set_draw_color ~a:(i * 9) 0 0 0;
    Draw.fill_rect 0 0 Draw.width Draw.height;
    Draw.present ();
    Unix.sleepf Draw.tick_rate
  done;
  (match position.y with
  | 0 -> Saves_menu.new_game ()
  | 1 -> Saves_menu.load_game ()
  | 2 -> exit 0
  | _ -> failwith "Impossible");
  for i = 0 to 28 do
    Draw.set_color Draw.white;
    Draw.fill_rect 0 0 Draw.width Draw.height;
    draw_title ();
    draw_box ();
    Draw.set_draw_color ~a:(255 - (i * 9)) 0 0 0;
    Draw.fill_rect 0 0 Draw.width Draw.height;
    Draw.present ();
    Unix.sleepf Draw.tick_rate
  done

let rec run_tick _ =
  (match Input.get_ctrl_option (Input.pop_key_option ()) with
  | Some Down -> position.y <- min 2 (position.y + 1)
  | Some Up -> position.y <- max 0 (position.y - 1)
  | Some Action -> action ()
  | Some _ | None -> ());
  Draw.set_color Draw.white;
  Draw.fill_rect 0 0 Draw.width Draw.height;
  draw_title ();
  draw_box ();
  Draw.present ();
  Input.sleep Draw.tick_rate ();
  run_tick ()

let intro_anim _ =
  Draw.set_color 0;
  Draw.fill_rect 0 0 Draw.width Draw.height;
  Draw.present ();
  Unix.sleepf 0.5;
  for i = 0 to 28 do
    Draw.set_draw_color (i * 9) (i * 9) (i * 9);
    Draw.fill_rect 0 0 Draw.width Draw.height;
    Draw.present ();
    Unix.sleepf Draw.tick_rate
  done;
  Unix.sleepf 0.1;
  position.y <- 100;
  for i = 0 to 70 do
    Draw.set_color Draw.white;
    Draw.fill_rect 0 0 Draw.width Draw.height;
    Draw.moveto 0
      (int_of_float
         (-640.
         +. (640. *. Float.pow (float_of_int (min i 45) /. 45.) 0.5)));
    draw_title ();
    Draw.moveto 0
      (int_of_float
         (-600.
         +. 600.
            *. Float.pow (float_of_int (max (i - 25) 0) /. 45.) 0.5));
    draw_box ();
    Draw.moveto 0 0;
    Draw.present ();
    Unix.sleepf Draw.tick_rate
  done;
  Unix.sleepf 0.2;
  position.y <- 0;
  ignore (Input.pop_key_option ());
  run_tick ()

let init _ = intro_anim ()
