open CreatureGame

let rec loop w r t =
  (match Sdlevent.poll_event () with
  | Some (Sdlevent.Quit _) ->
      Sdl.quit ();
      exit 0
  | Some (KeyDown e) ->
      print_endline ("Key Pressed: " ^ Sdlkeycode.to_string e.keycode)
  | Some (KeyUp e) ->
      print_endline ("Key Released: " ^ Sdlkeycode.to_string e.keycode)
  | _ -> ());
  for _ = 0 to 10 do
    Sdlrender.set_draw_color r
      ~rgb:(Random.int 256, Random.int 256, Random.int 256)
      ~a:255;
    Sdlrender.fill_rect r
      (Sdlrect.make1
         (Random.int 50, Random.int 50, Random.int 150, Random.int 150))
  done;
  Sdlrender.set_draw_color r ~rgb:(0, 0, 0) ~a:100;
  Sdlrender.fill_rect r (Sdlrect.make1 (0, 0, 100, 100));
  Sdlrender.render_present r;
  Sdlrender.clear r;
  Unix.sleepf 0.016;
  print_endline (string_of_float (Unix.gettimeofday () -. t));
  loop w r (Unix.gettimeofday ())

let main _ =
  Random.init 69;
  Sdl.init [ `VIDEO; `JOYSTICK ];
  let width, height = (300, 300) in
  let window =
    Sdlwindow.create2 ~title:"Big Red Adventure" ~x:`undefined
      ~y:`undefined ~width ~height ~flags:[]
  in
  let renderer =
    Sdlrender.create_renderer ~win:window ~index:(-1)
      ~flags:[ PresentVSync; Accelerated ]
  in
  Sdlrender.set_draw_blend_mode renderer Blend;
  Sdlrender.set_viewport renderer (Sdlrect.make1 (100, 100, 300, 300));

  Sdlrender.set_viewport renderer (Sdlrect.make1 (50, 50, 300, 300));
  loop window renderer (Sys.time ())

let _ =
  if true then Controller.main ();
  if false then main ()
