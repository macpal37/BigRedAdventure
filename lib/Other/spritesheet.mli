open Draw

type sprite_sheet = {
  sprites : sprite array;
  rows : int;
  columns : int;
  sheet_width : int;
  sheet_height : int;
  dpi : int;
}
(** Spritesheet type*)

val empty_spritesheet : sprite_sheet
(** Empty spritesheet*)

val init_spritesheet : string -> int -> int -> int -> sprite_sheet
(** [init_spritesheet filepath sprite_width sprite_column dpi] Returns a
    sprite_sheet type from a given [image] given the number of [row] and
    [columns] with dpi [dpi]. *)

val get_sprite : sprite_sheet -> int -> sprite
(** [get_sprite sprite_sheet id] Returns the sprite at index [i] from
    [sprite_sheet]*)

val set_text_color : sprite_sheet -> int -> int -> unit

val load_spritesheets : unit -> unit
(** Load spritesheets into memory*)

val get_spritesheet : string -> sprite_sheet
(** [get_spritesheet s] is the spritesheet with path [s]*)

val get_spritesheet2 : string -> Draw.folder -> sprite_sheet
(** [get_spritesheet2 s f] is the spritesheet with the specified
    location*)
