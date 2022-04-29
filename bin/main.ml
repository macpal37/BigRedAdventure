open CreatureGame

let rec loop w r =
  (match Sdlevent.poll_event () with
  | Some (Sdlevent.Quit _) ->
      Sdl.quit ();
      exit 0
  | Some (KeyDown e) ->
      print_endline ("Key Pressed: " ^ Sdlkeycode.to_string e.keycode)
  | Some (KeyUp e) ->
      print_endline ("Key Released: " ^ Sdlkeycode.to_string e.keycode)
  | _ -> ());
  Sdlrender.set_draw_color r
    ~rgb:(Random.int 256, Random.int 256, Random.int 256)
    ~a:255;
  Sdlrender.fill_rect r
    (Sdlrect.make1
       (Random.int 50, Random.int 50, Random.int 150, Random.int 150));
  Sdlrender.render_present r;
  Unix.sleepf 0.016;
  loop w r

let main _ =
  Random.init 69;
  Sdl.init [ `VIDEO; `JOYSTICK ];
  let width, height = (300, 300) in
  let window, renderer =
    Sdlrender.create_window_and_renderer ~width ~height ~flags:[]
  in
  Sdlrender.set_viewport renderer (Sdlrect.make1 (100, 100, 300, 300));

  Sdlrender.set_viewport renderer (Sdlrect.make1 (50, 50, 300, 300));
  loop window renderer

let _ =
  if true then Controller.main ();
  if false then main ()
