open Draw
open Graphics

type sprite_sheet = {
  sprites : sprite array;
  rows : int;
  columns : int;
  sheet_width : int;
  sheet_height : int;
}

let rec find x lst =
  match lst with
  | [] -> -1
  | h :: t -> if x = h then 0 else 1 + find x t

let little_sprite (image : Image.image) x y w h =
  let palette = ref [] in
  let pixels = Array.make (w * h) 0 in
  for j = 0 to h - 1 do
    for i = 0 to w - 1 do
      let color, alpha =
        Image.read_rgba image (i + x) (j + y)
          (fun r g b a () -> (rgb r g b, a))
          ()
      in
      if List.mem color !palette = false then
        palette := !palette @ [ color ];
      if alpha > 0 then
        let index = find color !palette in
        pixels.(i + (j * w)) <- index + 1
      else pixels.(i + (j * w)) <- 0
    done
  done;
  (pixels, !palette)

let init_spritesheet filepath sprite_width sprite_height dpi =
  let image = ImageLib_unix.openfile filepath in

  print_endline ("WIDTH :" ^ string_of_int image.width);
  print_endline ("HEIGHT :" ^ string_of_int image.height);
  let w = image.width / sprite_width in
  let h = image.height / sprite_height in
  let sprites = Array.make (w * h) Draw.empty_sprite in
  for j = 0 to h - 1 do
    for i = 0 to w - 1 do
      let pixels, palette =
        little_sprite image (sprite_width * i) (sprite_height * j)
          sprite_width sprite_height
      in
      let sprite =
        create_sprite pixels palette sprite_width sprite_height dpi
      in

      sprites.(i + (j * w)) <- sprite
    done
  done;
  {
    sprites;
    rows = h;
    columns = w + 1;
    sheet_width = image.width + 1;
    sheet_height = image.height + 1;
  }

let get_sprite sprite_sheet i =
  if i >= 0 && i < Array.length sprite_sheet.sprites then
    sprite_sheet.sprites.(i)
  else empty_sprite
