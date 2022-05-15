open Util

let main_box = Util.null ()
let title_sprite = Util.null ()

let load_assets _ =
  main_box *= Sprite_assets.get_sprite2 "main_menu" GUI_Folder;
  title_sprite *= Sprite_assets.get_sprite2 "title" GUI_Folder

let rec run_tick _ =
  Draw.set_draw_color 255 255 255;
  Draw.fill_rect 0 0 Draw.width Draw.height;
  Draw.draw_sprite_centered ~!main_box (Draw.width / 2) 270 ();
  DrawText.draw_string_colored 255 166 1 "yeet" Draw.white
    Draw.text_color ();
  DrawText.draw_string_colored 255 270 1 "yeet" Draw.white
    Draw.text_color ();
  DrawText.draw_string_colored 255 374 1 "yeet" Draw.white
    Draw.text_color ();
  Draw.draw_sprite_centered ~!title_sprite (Draw.width / 2) 630 ();
  Draw.present ();
  Input.sleep Draw.tick_rate ();
  run_tick ()

let init _ = run_tick ()
