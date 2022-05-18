open Util

type mode =
  | New
  | Load

let save_menu = Util.null ()
let position = Util.new_point ()

let load_assets _ =
  save_menu *= Draw.load_sprite "save_menu" GUI_Folder 3 ()

let convert_hours_minutes secs =
  let r = secs / 60 in
  let h = r / 60 in
  let m = r mod 60 in
  let ms =
    if m = 0 then "00"
    else if m < 10 then "0" ^ string_of_int m
    else string_of_int m
  in
  let hs =
    if h = 0 then "00"
    else if h < 10 then "0" ^ string_of_int h
    else string_of_int h
  in
  hs ^ ":" ^ ms

let write_preview (p : Saves.save_preview option) =
  match p with
  | Some p ->
      Draw_text.draw_string_colored 75 (Draw.height - 85)
        Draw_text.Small p.name Draw.white Draw.text_color ();
      Draw_text.draw_string_colored 355 (Draw.height - 85)
        Draw_text.Small
        ("$" ^ string_of_int p.money)
        Draw.white Draw.text_color ();
      Draw_text.draw_string_colored 605 (Draw.height - 85)
        Draw_text.Small
        (convert_hours_minutes p.time)
        Draw.white Draw.text_color ()
  | None ->
      Draw_text.draw_string_colored 75 (Draw.height - 85)
        Draw_text.Small "Empty" Draw.white Draw.text_color ()

let draw_all_preview_but (saves : int -> Saves.save_preview option) not
    =
  Draw.draw_sprite_centered ~!save_menu (Draw.width / 2)
    ((Draw.height / 2) - 2)
    ();
  Draw.moveto 0 (-2);
  if not <> 0 then write_preview (saves 0);
  Draw.moveto 0 (-242);
  if not <> 1 then write_preview (saves 1);
  Draw.moveto 0 (-482);
  if not <> 2 then write_preview (saves 2);
  Draw.moveto 0 (-2);
  Draw.set_line_width 8;
  Draw.set_draw_color 255 255 0;
  Draw.draw_rect (18 - 1)
    (Draw.height - 225 - (240 * position.y) + 4)
    (768 + 2) 210;
  Draw.moveto 0 0

let rec confirm_overwrite saves mode =
  draw_all_preview_but saves position.y;
  Draw.moveto 0 ((-240 * position.y) - 2);
  Draw_text.draw_string_colored 75 (Draw.height - 85) Draw_text.Small
    (match mode with
    | New -> "Are you sure you want to overwrite this save?"
    | Load -> "Do you want to start a new game?")
    Draw.white Draw.text_color ();

  Draw_text.draw_string_colored 95 (Draw.height - 119) Draw_text.Small
    "Yes" Draw.white Draw.text_color ();
  Draw_text.draw_string_colored 175 (Draw.height - 119) Draw_text.Small
    "No" Draw.white Draw.text_color ();
  Draw_text.draw_string_colored
    (75 + (position.x * 80))
    (Draw.height - 115) Draw_text.Small ">" Draw.white Draw.text_color
    ();

  Draw.moveto 0 0;
  Draw.present ();
  Input.sleep Draw.tick_rate ();
  let input = Input.get_ctrl_option (Input.pop_key_option ()) in
  (match input with
  | Some Right -> position.x <- min 1 (position.x + 1)
  | Some Left -> position.x <- max 0 (position.x - 1)
  | _ -> ());
  match input with
  | Some Action -> position.x = 0
  | Some Back -> false
  | _ -> confirm_overwrite saves mode

let draw_request_name name ticks =
  Draw_text.draw_string_colored 75 (Draw.height - 85) Draw_text.Small
    "Name your character: " Draw.white Draw.text_color ();
  Draw_text.draw_string_colored 75 (Draw.height - 115) Draw_text.Small
    (name ^ if ticks / 20 mod 2 = 0 then "_" else "")
    Draw.white Draw.text_color ()

let rec request_name saves name ticks =
  draw_all_preview_but saves position.y;
  Draw.moveto 0 ((-240 * position.y) - 2);
  draw_request_name name ticks;
  Draw.moveto 0 0;
  Draw.present ();
  Input.sleep Draw.tick_rate ();
  let key = Input.pop_key_option () in
  let name =
    match key with
    | Some Sdlkeycode.Space -> name ^ " "
    | Some Backspace ->
        String.sub name 0 (max (String.length name - 1) 0)
    | Some c ->
        let c_to_string = Sdlkeycode.to_string c in
        if String.length c_to_string = 1 && String.length name <= 14
        then name ^ c_to_string
        else name
    | None -> name
  in
  match key with
  | Some Return ->
      for i = 0 to 28 do
        draw_all_preview_but saves position.y;
        Draw.moveto 0 ((-240 * position.y) - 2);
        draw_request_name name (-20);
        Draw.moveto 0 0;
        Draw.set_draw_color ~a:(i * 9) 0 0 0;
        Draw.fill_rect 0 0 Draw.width Draw.height;
        Draw.present ();
        Unix.sleepf Draw.tick_rate
      done;
      name
  | _ -> request_name saves name (ticks + 1)

let fade_out saves =
  for i = 0 to 28 do
    draw_all_preview_but saves (-1);
    Draw.set_draw_color ~a:(i * 9) 0 0 0;
    Draw.fill_rect 0 0 Draw.width Draw.height;
    Draw.present ();
    Unix.sleepf Draw.tick_rate
  done

let launch_new_game id name =
  let save_preview : Saves.save_preview =
    Loading_screen.await [ Loading_screen.loading_label ];
    { name; id; money = -1; time = 0 }
  in
  State.new_game ();
  Overworld.run_overworld save_preview

let launch_load_game (save_preview : Saves.save_preview) =
  Loading_screen.submit_job (fun _ ->
      Saves.load_game save_preview.id;
      "loaded_save");
  Loading_screen.await [ Loading_screen.loading_label; "loaded_save" ];

  Overworld.run_overworld save_preview

let action mode saves =
  position.x <- 0;
  match mode with
  | New -> (
      match saves position.y with
      | Some _ ->
          if confirm_overwrite saves mode then
            let name = request_name saves "" 0 in
            launch_new_game position.y name
          else ()
      | None ->
          let name = request_name saves "" 0 in
          launch_new_game position.y name)
  | Load -> (
      match saves position.y with
      | Some s ->
          fade_out saves;
          launch_load_game s
      | None ->
          if confirm_overwrite saves mode then
            let name = request_name saves "" 0 in
            launch_new_game position.y name
          else ())

let rec choose (saves : int -> Saves.save_preview option) mode =
  let input = Input.get_ctrl_option (Input.pop_key_option ()) in
  (match input with
  | Some Down -> position.y <- min 2 (position.y + 1)
  | Some Up -> position.y <- max 0 (position.y - 1)
  | Some Action -> action mode saves
  | Some _ | None -> ());
  draw_all_preview_but saves (-1);
  Draw.present ();
  Input.sleep Draw.tick_rate ();
  if input = Some Back then (
    position.y <- 100;
    fade_out saves)
  else choose saves mode

let intro_anim _ =
  let saves = Saves.get_previews () in
  position.y <- 100;
  Unix.sleepf 0.5;
  for i = 0 to 28 do
    draw_all_preview_but saves (-1);
    Draw.set_draw_color ~a:(255 - (i * 9)) 0 0 0;
    Draw.fill_rect 0 0 Draw.width Draw.height;
    Draw.present ();
    Unix.sleepf Draw.tick_rate
  done;
  Unix.sleepf 0.1;
  position.y <- 0;
  saves

let new_game _ = choose (intro_anim ()) New
let load_game _ = choose (intro_anim ()) Load
