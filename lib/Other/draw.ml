type color = int

type sprite = {
  pixels : int array;
  width : int;
  height : int;
  mutable color_palette : color list;
  base_palette : color list;
  dpi : int;
  mutable active : bool;
}

type folder =
  | Creature_Folder
  | GUI_Folder
  | Tile_Folder
  | Item_Folder

let rgb r g b =
  Int.shift_left r 16 lor Int.shift_left g 8 lor Int.shift_left b 0

let color_to_rgb color =
  let r = (color land 0xFF0000) asr 0x10
  and g = (color land 0x00FF00) asr 0x8
  and b = color land 0x0000FF in
  (r, g, b)

let white = rgb 255 255 255
let red = rgb 255 0 0
let blue = rgb 0 0 255
let text_color = rgb 68 68 68
let text_color2 = rgb 215 215 255

let empty_sprite =
  {
    pixels = Array.make 0 0;
    width = 0;
    height = 0;
    color_palette = [];
    base_palette = [];
    dpi = 1;
    active = true;
  }

let create_sprite pixels palette width height dpi =
  {
    pixels;
    width = width * dpi;
    height = height * dpi;
    color_palette = palette;
    base_palette = palette;
    dpi;
    active = true;
  }

(* let blue = rgb 200 200 240 *)

let width = 800
let height = 720
let tick_rate = 0.016
let window = ref None
let renderer = ref None

let open_window _ =
  let w =
    Sdlwindow.create2 ~title:"Big Red Adventure" ~x:`undefined
      ~y:`undefined ~width ~height ~flags:[]
  in
  window := Some w;
  renderer :=
    Some (Sdlrender.create_renderer ~win:w ~index:(-1) ~flags:[])
(* let window _ = match !window with | Some w -> w | None -> failwith
   "Window not initialized"*)

let renderer _ =
  match !renderer with
  | Some r -> r
  | None -> failwith "Window not initialized"

let present _ = Sdlrender.render_present (renderer ())
let get_dimension sprite = (sprite.width, sprite.height)

let change_dpi sprite dpi =
  {
    sprite with
    width = sprite.width / sprite.dpi * dpi;
    height = sprite.height / sprite.dpi * dpi;
    dpi;
  }

let set_draw_color r g b =
  Sdlrender.set_draw_color3 (renderer ()) ~r ~g ~b ~a:255

let set_color c =
  let r, g, b = color_to_rgb c in
  set_draw_color r g b

let fill_rect x y w h =
  Sdlrender.fill_rect (renderer ())
    (Sdlrect.make1 (x, height - y - h, w, h))

let line_width = ref 1
let set_line_width i = line_width := i

let draw_rect x y w h =
  fill_rect x y !line_width h;
  fill_rect x y w !line_width;
  fill_rect (x + w - (1 * !line_width)) y !line_width h;
  fill_rect x (y + h - (1 * !line_width)) w !line_width

let offset_x = ref 0
let offset_y = ref 0
let current_x _ = !offset_x
let current_y _ = !offset_y

let moveto x y =
  offset_x := x;
  offset_y := -y;
  Sdlrender.set_viewport (renderer ())
    (Sdlrect.make1 (!offset_x, !offset_y, width, height))

let rmoveto x y =
  offset_x := !offset_x + x;
  offset_y := !offset_y - y;
  Sdlrender.set_viewport (renderer ())
    (Sdlrect.make1 (!offset_x, !offset_y, width, height))

let draw_pixel size x y () =
  fill_rect (x - (size / 2)) (y - (size / 2)) size size

let draw_from_pixels sprite x y min_w min_h max_w max_h () =
  if sprite.active then
    for j = min_h / sprite.dpi to (max_h / sprite.dpi) - 1 do
      for i = min_w / sprite.dpi to (max_w / sprite.dpi) - 1 do
        let c = sprite.pixels.(i + (j * (sprite.width / sprite.dpi))) in
        let tx, ty = (i * sprite.dpi, j * sprite.dpi) in
        if c <> 0 && tx >= min_w && ty >= min_h then begin
          set_color (List.nth sprite.color_palette (c - 1));
          draw_pixel sprite.dpi (x + tx) (y + max_h - ty) ()
        end
      done
    done

let rec find x lst =
  match lst with
  | [] -> -1
  | h :: t -> if x = h then 0 else 1 + find x t

let image_to_sprite (image : Image.image) =
  let palette = ref [] in
  let pixels = Array.make (image.width * image.height) 0 in
  for j = 0 to image.height - 1 do
    for i = 0 to image.width - 1 do
      let color, alpha =
        Image.read_rgba image i j (fun r g b a () -> (rgb r g b, a)) ()
      in
      if List.mem color !palette = false then
        palette := !palette @ [ color ];
      if alpha > 0 then
        let index = find color !palette in
        pixels.(i + (j * image.width)) <- index + 1
      else pixels.(i + (j * image.width)) <- 0
    done
  done;
  (pixels, !palette)

(* no function for converting color back to rgb in Graphics *)

let load_image (image : Image.image) dpi =
  let pixels, palette = image_to_sprite image in
  let new_pallette = palette in

  {
    pixels;
    width = image.width * dpi;
    height = image.height * dpi;
    color_palette = new_pallette;
    base_palette = new_pallette;
    dpi;
    active = true;
  }

let load_sprite name folder dpi () =
  let filename =
    match folder with
    | Creature_Folder -> "assets/creature_sprites/" ^ name ^ ".png"
    | GUI_Folder -> "assets/gui_sprites/" ^ name ^ ".png"
    | Item_Folder -> "assets/item_sprites/" ^ name ^ ".png"
    | Tile_Folder -> "assets/tile_sprites/" ^ name ^ ".png"
  in
  let image = ImageLib_unix.openfile filename in
  load_image image dpi

let load_sprite_from_filepath filepath dpi () =
  let image = ImageLib_unix.openfile filepath in
  load_image image dpi

let load_creature name () =
  let filename = "assets/creature_sprites/" ^ name ^ ".png" in
  load_sprite_from_filepath filename 3 ()

let draw_sprite_crop
    sprite
    x
    y
    (width_min, width_max)
    (height_min, height_max)
    () =
  draw_from_pixels sprite x y width_min height_min width_max height_max
    ()

let draw_sprite sprite x y () =
  draw_from_pixels sprite x y 0 0 sprite.width sprite.height ()

let draw_creature sprite player () =
  if player then draw_sprite sprite 50 166 ()
  else
    draw_sprite sprite
      (width - sprite.width - 50)
      (height - 50 - sprite.height)
      ()

let rec wait timer () =
  if timer = 0 then () else Input.sleep 1. ();
  match Input.pop_key_option () with
  | Some _ -> ()
  | None -> wait (timer - 1) ()

let draw_gradient w h =
  for y = 0 to h - 1 do
    for x = 0 to w - 1 do
      let s = 255 * (x + y) / (w + h - 2) in
      set_draw_color s s s;
      Sdlrender.draw_point2 (renderer ()) ~x ~y
    done
  done

let add_rgb sprite red green blue () =
  let rec add_rgb_rec = function
    | [] -> []
    | h :: t ->
        let r, g, b = color_to_rgb h in
        rgb
          (Util.bound (r + red) 0 255)
          (Util.bound (g + green) 0 255)
          (Util.bound (b + blue) 0 255)
        :: add_rgb_rec t
  in

  sprite.color_palette <- add_rgb_rec sprite.color_palette

let make_grayscale sprite () =
  sprite.color_palette <-
    List.map
      (fun c ->
        let r, g, b = color_to_rgb c in
        let g = (r + g + b) / 3 in
        rgb g g g)
      sprite.color_palette

let reset_rgb sprite () = sprite.color_palette <- sprite.base_palette
let set_active (s : sprite) (flag : bool) = s.active <- flag

let change_color sprite i c =
  let rec replace j = function
    | [] -> []
    | h :: t ->
        if j = i then c :: replace (j + 1) t else h :: replace (j + 1) t
  in
  sprite.color_palette <- replace 0 sprite.color_palette
