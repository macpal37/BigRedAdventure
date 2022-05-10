open Draw

type sprite_sheet = {
  sprites : sprite array;
  rows : int;
  columns : int;
  sheet_width : int;
  sheet_height : int;
  dpi : int;
}

val init_spritesheet : string -> int -> int -> int -> sprite_sheet
(** [init_spritesheet filepath sprite_width sprite_column dpi] Returns a
    sprite_sheet type from a given [image] given the number of [row] and
    [columns] with dpi [dpi]. *)

val get_sprite : sprite_sheet -> int -> sprite
(** [get_sprite sprite_sheet id] Returns the sprite at index [i] from
    [sprite_sheet]*)

val set_text_color : sprite_sheet -> int -> int -> unit
