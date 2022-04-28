open Graphics

type sprite = {
  pixels : int array;
  width : int;
  height : int;
  mutable color_palette : color list;
  base_palette : color list;
  dpi : int;
}

type folder =
  | Creature_Folder
  | GUI_Folder
  | Tile_Folder
  | Item_Folder

let font_size = ref 50
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
  }

let create_sprite pixels palette width height dpi =
  {
    pixels;
    width = width * dpi;
    height = height * dpi;
    color_palette = palette;
    base_palette = palette;
    dpi;
  }

let erase_mode = ref false
let synced_mode = ref true

(* let blue = rgb 200 200 240 *)
let width = 800
let height = 720
let sync flag () = auto_synchronize flag
let usync flag () = if !synced_mode then auto_synchronize flag

let set_font_size size () =
  font_size := size;
  set_font
    ("-*-fixed-bold-r-semicondensed--" ^ string_of_int size
   ^ "-*-*-*-*-*-iso8859-1")

let get_dimension sprite = (sprite.width, sprite.height)
let get_font_size () = !font_size
let set_erase_mode flag () = erase_mode := flag
let set_synced_mode flag = synced_mode := flag

let change_dpi sprite dpi =
  {
    sprite with
    width = sprite.width / sprite.dpi * dpi;
    height = sprite.height / sprite.dpi * dpi;
    dpi;
  }

let sync_draw draw () =
  sync false ();
  draw ();
  sync true ()

let clear_screen () =
  set_color (rgb 255 255 255);
  fill_rect 0 0 width height

let draw_pixel size x y () =
  fill_rect (x - (size / 2)) (y - (size / 2)) size size

let draw_from_pixels sprite x y min_w min_h max_w max_h () =
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

let color_to_rgb color =
  let r = (color land 0xFF0000) asr 0x10
  and g = (color land 0x00FF00) asr 0x8
  and b = color land 0x0000FF in
  (r, g, b)

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

let clear_sprite sprite x y () =
  usync false ();
  set_erase_mode true ();
  draw_from_pixels sprite x y 0 0 sprite.width sprite.height ();
  set_erase_mode false ();
  usync true ()

let draw_sprite_crop
    sprite
    x
    y
    (width_min, width_max)
    (height_min, height_max)
    () =
  usync false ();
  draw_from_pixels sprite x y width_min height_min width_max height_max
    ();
  usync true ()

let draw_sprite sprite x y () =
  usync false ();
  draw_from_pixels sprite x y 0 0 sprite.width sprite.height ();
  usync true ()

let draw_creature sprite player () =
  if player then begin
    let text_box = Graphics.get_image 3 0 797 216 in

    draw_sprite sprite 50 166 ();
    draw_image text_box 3 0
  end
  else
    draw_sprite sprite
      (width - sprite.width - 50)
      (height - 50 - sprite.height)
      ()

let rec wait timer () =
  if timer = 0 then ()
  else if Graphics.key_pressed () then begin
    if Graphics.read_key () = 'e' then ()
  end
  else wait (timer - 1) ()

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

let damage_render player_sprite player clear_function () =
  let rec damage_render_rec c () =
    set_synced_mode false;
    auto_synchronize false;
    if c = 0 then
      (* draw_creature enemy_sprite (player = false) (); *)
      draw_creature player_sprite player ()
    else begin
      if c mod 2 = 0 then draw_creature player_sprite player ()
      else begin
        auto_synchronize false;
        clear_function ();

        auto_synchronize true
      end;
      auto_synchronize true;
      Input.sleep 0.1 ();
      damage_render_rec (c - 1) ()
    end
  in
  damage_render_rec 7 ();
  auto_synchronize false;
  set_color text_color

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
