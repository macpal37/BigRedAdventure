open Draw
open Graphics

type sprite_sheet = {
  sprites : sprite list;
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
  let rec pixel_map i j sprite =
    if i < w then
      let new_i, new_j = if j = 0 then (i + 1, w - 1) else (i, j - 1) in

      let pixels, palette = sprite in
      let color, alpha =
        Image.read_rgba image (j + x) (i + y)
          (fun r g b a () -> (rgb r g b, a))
          ()
      in

      let new_pallette =
        if List.mem color palette = false then palette @ [ color ]
        else palette
      in

      if alpha > 0 then
        let index = find color new_pallette in
        pixel_map new_i new_j ((index + 1) :: pixels, new_pallette)
      else pixel_map new_i new_j (0 :: pixels, new_pallette)
    else sprite
  in
  pixel_map 0 (h - 1) ([], [])

let init_spritesheet filepath sprite_width sprite_height =
  let image = ImageLib_unix.openfile filepath in
  let w = (image.width / sprite_width) - 1 in
  let h = image.height / sprite_height in
  let rec split_sprites i j lst =
    if j < h then
      let pixels, palette =
        little_sprite image (sprite_width * i) (sprite_height * j)
          sprite_width sprite_height
      in

      let sprite =
        create_sprite pixels palette sprite_width sprite_height 4
      in

      if i < w then split_sprites (i + 1) j (sprite :: lst)
      else split_sprites 0 (j + 1) (sprite :: lst)
    else lst
  in

  let sprites = split_sprites 0 0 [] in
  {
    sprites = List.rev sprites;
    rows = h;
    columns = w;
    sheet_width = image.width + 1;
    sheet_height = image.height + 1;
  }

let get_sprite sprite_sheet i =
  if i >= 0 && i < List.length sprite_sheet.sprites then
    List.nth sprite_sheet.sprites i
  else empty_sprite
