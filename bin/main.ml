open Graphics
open CreatureGame.Draw

let white = rgb 55 255 255
let blue = rgb 200 200 240
let black = rgb 0 0 0
let red = rgb 200 50 50
let width = 800
let height = 720

type mode =
  | Adventure
  | Combat
  | Menu

type game_controller = { mutable mode : mode }

let open_window =
  open_graph (" " ^ string_of_int width ^ "x" ^ string_of_int height);
  set_window_title "PokemonGame"

(* no way of setting background color; resizing shows white *)
let clear_window color =
  let fg = foreground in
  set_color color;
  fill_rect 0 0 (size_x ()) (size_y ());
  set_color fg

(* no function for converting color back to rgb in Graphics *)
let color_to_rgb color =
  let r = (color land 0xFF0000) asr 0x10
  and g = (color land 0x00FF00) asr 0x8
  and b = color land 0x0000FF in
  (r, g, b)

let get_move () : char option =
  if Graphics.key_pressed () then Some (Graphics.read_key ()) else None

let rayquaza = load_creature "rayquaza" ()
let clefairy_back = load_creature "clefairy_back" ()

let clear () =
  set_color blue;
  fill_rect 0 0 width height;
  set_color black;
  moveto 100 200

let start_up () =
  set_color red;
  fill_rect 0 0 width 212;
  set_color black;
  moveto 80 (height - 50 - 40);
  draw_string "RAYQUAZA :L80";
  draw_health_bar 100 100 100 true ();
  draw_health_bar 100 100 100 false ();
  draw_creature rayquaza false ();
  draw_creature clefairy_back true ();
  Unix.sleep 1;
  damage_render rayquaza false ();
  draw_health_bar 356 356 0 false ();
  Unix.sleepf 1.5;
  faint 20 2 rayquaza ();
  draw_text "It was super-effective!" ();
  draw_text "Clefairy is the best!!" ()

let run_game key_pressed game () =
  let c = String.make 1 key_pressed in
  match game.mode with
  | Adventure ->
      print_endline ("Adventure" ^ c);
      game.mode <- Combat
  | Combat -> game.mode <- Menu
  | Menu -> game.mode <- Adventure

let rec event_loop wx wy start game =
  (* there's no resize event so polling in required *)
  let _ = wait_next_event [ Poll ]
  and wx' = size_x ()
  and wy' = size_y () in
  if wx' <> wx || wy' <> wy then clear_window blue;
  Unix.sleepf 0.005;

  if start then start_up ();

  let font = 40 in
  let xx = get_move () in
  (match xx with
  | Some '.' -> clear ()
  | Some 'p' ->
      draw_creature rayquaza false ();

      set_color black
  | Some 'o' -> draw_creature clefairy_back true ()
  | Some 'm' -> start_up ()
  | Some 'c' ->
      moveto 80 (height - 50 - font);
      draw_string "RAYQUAZA :L80"
  | Some 'd' ->
      damage_render rayquaza false ();
      draw_health_bar 356 356 0 false ();
      Unix.sleepf 1.5;
      faint 20 2 rayquaza ()
  | Some 'f' -> faint 20 2 rayquaza ()
  | Some 'g' ->
      draw_health_bar 100 100 100 true ();
      draw_health_bar 100 100 100 false ()
  | Some 'h' -> draw_health_bar 15 15 6 true ()
  | Some 'y' -> draw_health_bar 100 99 0 false ()
  | Some 'w' ->
      draw_text
        "I know that a lot of people want to catch em' all, but my job \
         is a much bigger challenge. It is my goal to masturbate to \
         all 807 Pokemon, plain and simple. I usually try to do it \
         twice a day, regardless of the difficulties. At the end, I \
         always win. I go on places like Deviantart, rule 34 and, \
         occasionally e621 in order to achieve this massive goal, and \
         when I finally do, I will become a Pokemon Master. Sometimes, \
         it is easy. I can come in five minutes looking at Gardevoir \
         or Lopunny pornos. Sometimes I come across major challenges \
         that I have to overcome, in the case of Garbodor and Magikarp \
         especially. I have to imagine the wet, sloppy fish mouth \
         sucking on my cock without thinking about the actual fish \
         itself. It is very hard, but the satisfaction you get when \
         you achieve victory is immense. Not only do you get the \
         generally pleasurable feeling from ejaculation, but you also \
         know that you overcame an obstacle few men have dared to try. \
         I have a total of 347 successful ejaculations total, but it \
         only gets harder as I move on. When I see a Serperior, for \
         instance, I have to think to myself In what way can I imagine \
         this creature in order to get off to it? It is a puzzle for \
         sure, considering I do not have a thing for (most) of these \
         creatures, making it extremely entertaining and interesting \
         for others to watch. I try to focus in on its somewhat \
         beautiful face, and think about that more than the yards of \
         snake behind it. I sometimes have issues with Pokemon like \
         Machamp, who appear extremely male. But I always find a way. \
         There has been no hurdle too steep for me. I want to be the \
         very best. Anything lower does not cut it. And that is why I \
         am beating off to pictures of Lucario on the Internet, mom"
        ()
  | Some c -> run_game c game ()
  | None -> run_game '#' game ());

  event_loop wx' wy' false game

let () =
  open_window;
  let game = { mode = Adventure } in
  moveto 100 200;
  set_font "-*-fixed-bold-r-semicondensed--40-*-*-*-*-*-iso8859-1";
  let r, g, b = color_to_rgb background in
  Printf.printf "Background color: %d %d %d\n" r g b;
  try event_loop 0 0 true game
  with Graphic_failure _ -> print_endline "Exiting..."
