type sprite_sheet = {
  sprites : Draw.sprite list;
  rows : int;
  columns : int;
  sheet_width : int;
  sheet_height : int;
}

val init_spritesheet : string -> int -> int -> sprite_sheet
(** [init_spritesheet filepath rows columns] Returns a sprite_sheet type
    from a given [image] given the number of [row] and [columns]. *)

val get_sprite : sprite_sheet -> int -> Draw.sprite
(** [get_sprite sprite_sheet id] Returns the sprite at index [i] from
    [sprite_sheet]*)
