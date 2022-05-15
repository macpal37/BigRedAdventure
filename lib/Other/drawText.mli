open Draw
open Spritesheet

type font = {
  font_sprite : sprite_sheet;
  hspacing : int;
  vspacing : int;
}

type font_size =
  | Small
  | Medium
  | Large
  | Huge

val box_cap : int
val text_display : string ref
val battle_bot : sprite
val set_text_display : string -> unit

val clear_text : sprite -> unit -> unit
(** [clear_text clear_sprite] Clears the text with the givn sprite. *)

val get_text_transcript : string -> int -> string list

val draw_string_colored :
  int -> int -> font_size -> string -> int -> int -> unit -> unit
(** [draw_string_colored x y f t text_color shadow_color] Draws the text
    [t] at [x,y] of the screen with the given font [f] of the colors
    [text_color] and [shadow_color]. *)

val draw_text_string_pos :
  int -> int -> font_size -> int -> string -> unit -> unit
(** [draw_text_string_pos x y f char_cap  text  (_)] draws the given
    [text] with the given [font_size]on the [x y]. The text will be
    printed in rows based on the [char_cap]. There is no text scroll.*)
