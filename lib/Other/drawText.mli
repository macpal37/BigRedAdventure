open Draw

val battle_bot : sprite

val clear_text : sprite -> unit -> unit
(** [clear_text clear_sprite] Clears the text with the givn sprite. *)

val draw_string_colored :
  int ->
  int ->
  int ->
  int ->
  string ->
  Graphics.color ->
  Graphics.color ->
  unit ->
  unit
(** [draw_string_colored x y shadow_offset font_size text text_color shadow_color]
    Draws the [text] at [x,y] of the screen with the given [text_color].
    [shadow_offset] represents the offset disstance of the shodow to the
    text, when drawn. *)

val draw_string : string -> int -> int -> unit -> unit

val draw_text : string -> int -> bool -> bool -> unit -> unit
(** [draw_text text font_size auto_scroll sticky(_)] draws the given
    [text] with the given [font_size]on the bottom of the screen and
    simulates text scrolling. If [auto_scroll] is on, then it will
    scroll automatically after a delay. If [sticky] is on, then the text
    will remain there until new text replaces it.*)

val draw_text_string : string -> unit -> unit
(** [draw_text_string text font_size (_)] draws the given [text] with
    the given [font_size]on the bottom of the screen. There is no text
    scroll.*)

val draw_text_string_pos :
  int -> int -> int -> int -> string -> Graphics.color -> unit -> unit
(** [draw_text_string_pos x y font_size char_cap  text color (_)] draws
    the given [text] with the given [font_size]on the [x y]. There is no
    text scroll.*)
