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
  Sdlrender.set_draw_color r
    ~rgb:(Random.int 256, Random.int 256, Random.int 256)
    ~a:255;
  Sdlrender.fill_rect r
    (Sdlrect.make1
       (Random.int 50, Random.int 50, Random.int 150, Random.int 150));
  Sdlrender.render_present r;
  print_endline (string_of_float (Sys.time () -. t));
  loop w r (Sys.time ())

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
      ~flags:[ PresentVSync ]
  in
  Sdlrender.set_viewport renderer (Sdlrect.make1 (100, 100, 300, 300));

  Sdlrender.set_viewport renderer (Sdlrect.make1 (50, 50, 300, 300));
  loop window renderer (Sys.time ())

let _ =
  if true then Controller.main ();
  if false then main ()
