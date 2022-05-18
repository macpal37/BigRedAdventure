let busy_mutex = Mutex.create ()

(* let channel = Event.new_channel () *)
let completed_jobs = ref []
let loading_label = "loading_screen.ml:load_assets"

let load_assets _ =
  (* Unix.sleepf 10.; *)
  Draw.load_sprites ();
  Spritesheet.load_spritesheets ();
  Item.load_items ();
  Map.load_maps ();
  Creature_menu.load_assets ();
  Overworld.load_assets ();
  Battle.load_assets ();
  Event_menu.load_assets ();
  Inventory_menu.load_assets ();
  Party_menu.load_assets ();
  loading_label
(* print_endline "Done loading" *)

let submit_job f =
  ignore
    (Thread.create
       (fun _ ->
         try
           Mutex.lock busy_mutex;
           let l = f () in
           completed_jobs := l :: !completed_jobs;
           Mutex.unlock busy_mutex
         with e ->
           let msg = Printexc.to_string e
           and stack = Printexc.get_backtrace () in
           prerr_endline
             ("Loader thread encountered exception: " ^ msg
            ^ "\nStack trace:\n" ^ stack);
           exit 2)
       ())

let clear_labels _ =
  Mutex.lock busy_mutex;
  completed_jobs := [];
  Mutex.unlock busy_mutex

let draw ticks =
  Draw.set_draw_color 10 10 10;
  Draw.fill_rect 0 0 Draw.width Draw.height;
  Draw_text.draw_string_colored 75 75 Draw_text.Medium
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

let jobs_completed jobs =
  if Mutex.try_lock busy_mutex then
    if List.for_all (fun s -> List.mem s !completed_jobs) jobs then (
      Mutex.unlock busy_mutex;
      true)
    else (
      Mutex.unlock busy_mutex;
      false)
  else false

let rec loading_screen_wait ticks jobs =
  draw ticks;
  Draw.present ();
  Input.sleep Draw.tick_rate ();
  if jobs_completed jobs then fade_out ()
  else loading_screen_wait (ticks + 1) jobs

let fade_in jobs =
  if jobs_completed jobs then ()
  else (
    for i = 0 to 42 do
      draw i;
      Draw.set_draw_color ~a:(255 - (i * 6)) 0 0 0;
      Draw.fill_rect 0 0 Draw.width Draw.height;
      Draw.present ();
      Input.sleep Draw.tick_rate ()
    done;
    loading_screen_wait 0 jobs)

let await jobs =
  if jobs_completed jobs then ()
  else (
    Unix.sleepf 0.5;
    fade_in jobs)
