(** Module for drawing text on screen using custom fonts.. *)

type font_size =
  | Small
  | Medium
  | Large
  | Huge  (** [font_size] varaint for text font sizes. *)

val box_cap : int
(** [box_cap] max number of characters that can be displayed on the text
    box. *)

val get_text_display : string ref
(** [text_display] text_displayed on the text_box. *)

val battle_bot : Draw.sprite
(** [battle_bot] general graphic sprite for text_boxes. *)

val set_text_display : string -> unit
(** [set_text_display text] sets the current text displayed on the text
    box to [text]. *)

val clear_text : Draw.sprite -> unit -> unit
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
