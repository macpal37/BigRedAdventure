open Graphics
open PokemonGame.Draw

let white = rgb 55 255 255
let blue = rgb 200 200 240
let black = rgb 0 0 0
let red = rgb 200 50 50
let width = 800
let height = 720

(* no function for converting color back to rgb in Graphics *)
let color_to_rgb color =
  let r = (color land 0xFF0000) asr 0x10
  and g = (color land 0x00FF00) asr 0x8
  and b = color land 0x0000FF in
  (r, g, b)

let open_window =
  open_graph (" " ^ string_of_int width ^ "x" ^ string_of_int height);
  set_window_title "PokemonGame"

(* no way of setting background color; resizing shows white *)
let clear_window color =
  let fg = foreground in
  set_color color;
  fill_rect 0 0 (size_x ()) (size_y ());
  set_color fg

(* create a gradient of colors from black at 0,0 to white at w-1,h-1 *)
let gradient arr w h =
  for y = 0 to h - 1 do
    for x = 0 to w - 1 do
      let s = 255 * (x + y) / (w + h - 2) in
      arr.(y).(x) <- rgb s s s
    done
  done

let draw_gradient w h =
  (* w and h are flipped from perspective of the matrix *)
  let arr = Array.make_matrix h w white in
  gradient arr w h;
  draw_image (make_image arr) 0 0

let c = 0

let count x =
  if x mod 60 = 1 then print_endline " ";
  if x mod 2 = 1 then print_int x;
  x + 1

let get_move () : char option =
  if Graphics.key_pressed () then Some (Graphics.read_key ()) else None

let cool c () = draw_char c
let size = 240
let rayquaza = load_pet "rayquaza" ()
let clefairy_back = load_pet "clefairy_back" ()

let rec damage_render c () =
  if c = 0 then
    draw_pet rayquaza
      (width - (size / 2) - 50)
      (height - (size / 2) - 50)
      ()
  else begin
    if c mod 2 = 0 then
      draw_pet rayquaza
        (width - (size / 2) - 50)
        (height - (size / 2) - 50)
        ()
    else begin
      set_color blue;
      draw_pixel (size + 4)
        (width - (size / 2) - 50)
        (height - (size / 2) - 50)
        ()
    end;
    Unix.sleepf 0.10;
    damage_render (c - 1) ()
  end;
  set_color black

let rec faint base c () =
  if c = base - 2 then
    draw_pixel (size + 4)
      (width - (size / 2) - 50)
      (height - (size / 2) - 50)
      ()
  else begin
    set_color blue;
    draw_pixel (size + 4)
      (width - (size / 2) - 50)
      (height - (size / 2) - 50)
      ();
    draw_sprite rayquaza
      (width - (size / 2) - 50)
      (height - (size / 2) - 50 - (size - (size / c)))
      240 (240 / c) ();
    Unix.sleepf 0.05;
    set_color blue;

    (* draw_pixel (size+4) (width-size/2-50) (height-size/2-50-size+40)
       (); *)
    faint base (c + 1) ()
  end;
  set_color black

let clear () =
  set_color blue;
  fill_rect 0 0 width height;
  set_color black;
  moveto 100 200

let rec event_loop wx wy =
  (* there's no resize event so polling in required *)
  let _ = wait_next_event [ Poll ]
  and wx' = size_x ()
  and wy' = size_y () in
  if wx' <> wx || wy' <> wy then clear_window blue;
  Unix.sleepf 0.005;

  (* cool draw_string; *)
  let size = 240 in
  let font = 30 in
  let xx = get_move () in
  (* print_char (match xx with | Some 'a' -> '?'| Some c -> c | None ->
     ' '); *)
  (match xx with
  | Some '.' -> clear ()
  | Some 'p' ->
      draw_sprite rayquaza
        (width - (size / 2) - 50)
        (height - (size / 2) - 50)
        120 240 ();
      set_color black
  | Some 'o' ->
      draw_pet clefairy_back ((size / 2) + 50) ((size / 2) + 160) ()
  | Some 'm' ->
      set_color red;
      fill_rect 0 0 width 212;
      set_color black
  | Some 'c' ->
      moveto 80 (height - 50 - font);
      draw_string "RAYQUAZA :L80"
  | Some 'd' -> damage_render 7 ()
  | Some 'f' -> faint 20 2 ()
  | Some c -> cool c ()
  | None -> plot 0 0);

  (* match get_move () with Some c -> pp draw_char c | None -> pp
     draw_char ' '; *)
  event_loop wx' wy'

let () =
  open_window;
  moveto 100 200;
  set_font "-*-fixed-bold-r-semicondensed--40-*-*-*-*-*-iso8859-1";
  (* set_font
     "-misc-dejavu-sans-mono-bold-r-normal--256-0-0-0-m-0-iso8859-1"; *)
  let r, g, b = color_to_rgb background in
  Printf.printf "Background color: %d %d %d\n" r g b;
  try event_loop 0 0
  with Graphic_failure _ -> print_endline "Exiting..."
