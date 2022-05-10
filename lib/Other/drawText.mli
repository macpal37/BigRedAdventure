open Draw

val battle_bot : sprite

val clear_text : sprite -> unit -> unit
(** [clear_text clear_sprite] Clears the text with the givn sprite. *)

val draw_string_colored :
  int -> int -> int -> string -> int -> int -> unit -> unit
(** [draw_string_colored x y f t text_color shadow_color] Draws the text
    [t] at [x,y] of the screen with the given [text_color].
    [shadow_offset] represents the offset disstance of the shodow to the
    text, when drawn. *)

val draw_text : string -> int -> bool -> bool -> unit -> unit
(** [draw_text text font_size auto_scroll sticky(_)] draws the given
    [text] with the given [font_size]on the bottom of the screen and
    simulates text scrolling. If [auto_scroll] is on, then it will
    scroll automatically after a delay. If [sticky] is on, then the text
    will remain there until new text replaces it.*)

val draw_text_string : string -> unit -> unit
(** [draw_text_string text (_)] draws the given [text] with the given
    [font_size]on the bottom of the screen. There is no text scroll.*)

val draw_text_string_pos :
  int -> int -> int -> int -> string -> int -> unit -> unit
(** [draw_text_string_pos x y f char_cap  text color (_)] draws the
    given [text] with the given [font_size]on the [x y]. There is no
    text scroll.*)
