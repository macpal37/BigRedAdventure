let busy_mutex = Mutex.create ()
let channel = Event.new_channel ()

let load_assets _ =
  Draw.load_sprites ();
  Spritesheet.load_spritesheets ();
  Item.load_items ();
  Map.load_maps ();
  Creature_menu.load_assets ();
  Overworld.load_assets ();
  Battle.load_assets ();
  Event_menu.load_assets ();
  Inventory_menu.load_assets ();
  Party_menu.load_assets ()
(* print_endline "Done loading" *)

let load _ =
  ignore
    (Thread.create
       (fun _ ->
         try
           Mutex.lock busy_mutex;
           let e = Event.send channel () in
           Event.sync e;
           load_assets ();
           Mutex.unlock busy_mutex
         with e ->
           let msg = Printexc.to_string e
           and stack = Printexc.get_backtrace () in
           prerr_endline
             ("Loader thread encountered exception: " ^ msg
            ^ "\nStack trace:\n" ^ stack);
           exit 2)
       ());
  let e = Event.receive channel in
  Event.sync e

let draw ticks =
  Draw.set_draw_color 10 10 10;
  Draw.fill_rect 0 0 Draw.width Draw.height;
  DrawText.draw_string_colored 75 75 DrawText.Medium
    ("Loading"
    ^
    match ticks / 45 mod 4 with
    | 0 -> ""
    | 1 -> "."
    | 2 -> ".."
    | 3 -> "..."
    | _ -> failwith "Impossible")
    (Draw.rgb 220 220 220) (Draw.rgb 60 60 60) ()

let fade_out _ =
  for i = 0 to 42 do
    draw i;
    Draw.set_draw_color ~a:(i * 6) 0 0 0;
    Draw.fill_rect 0 0 Draw.width Draw.height;
    Draw.present ();
    Input.sleep Draw.tick_rate ()
  done;
  Unix.sleepf 0.5

let rec loading_screen_wait ticks =
  draw ticks;
  Draw.present ();
  Input.sleep Draw.tick_rate ();
  if Mutex.try_lock busy_mutex then (
    Mutex.unlock busy_mutex;
    fade_out ())
  else loading_screen_wait (ticks + 1)

let fade_in _ =
  Unix.sleepf 0.5;
  for i = 0 to 42 do
    draw i;
    Draw.set_draw_color ~a:(255 - (i * 6)) 0 0 0;
    Draw.fill_rect 0 0 Draw.width Draw.height;
    Draw.present ();
    Input.sleep Draw.tick_rate ()
  done;
  loading_screen_wait 0

let await _ =
  if Mutex.try_lock busy_mutex then Mutex.unlock busy_mutex
  else fade_in ()
