(** Abstract Representation of the View

    This module containsa all the functions sued to load and render all
    the assets used in the Game *)

type pixel
(** The abstract type that represents a pixel. It stores the (x,y)
    position and the r,g,b values of the pixel. It is used to render
    images ont the screen.*)

val set_text_bg : string list -> string list -> unit
val set_text_char_cap : int -> unit
val pokemon_sprite_size : int

val draw_pixel : int -> int -> int -> unit -> unit
(** Draws a pixel of a given (size) to the screen on the (x,y)
    coordinate provided*)

val load_creature : string -> unit -> string list
(** [load_creature name (_)]Loads a given [name.json] from the creatures
    sprites and generates a lsit that represents the sprites. To be sed
    in conjuction with draw_sprite*)

val load_sprite : string -> unit -> string list

val draw_sprite :
  string list -> int -> int -> int -> int -> unit -> unit
(** [draw_sprite sprite x y width height (_)] draws an image from the
    given [sprite] representation at [x] and [y] with the given [width]
    and [height]*)

val draw_creature_pos : string list -> int -> int -> unit -> unit
(** [draw_creature_pos sprite x y (_)] draws the [sprite] at the given
    position [x] [y] *)

val draw_creature : string list -> bool -> unit -> unit
(** [draw_creature_pos sprite is_player (_)] draws the [sprite] at
    combat positions. If [is_player] is true then it draws the [sprite]
    in the lower left corner of the screen. Otherwise its an enemy and
    draws the [sprite] on the upper right corner of the screen.*)

val draw_text : string -> unit -> unit
(** [draw_text text (_)] draws the given text on the bottom of the
    screen and simulates text scrolling. *)

val draw_gradient : int -> int -> unit

val animate_faint : string list -> unit -> unit
(** [animate_faint sprite (_)] performs the fainting animation of the
    creature [sprite] *)

val damage_render : string list -> bool -> unit -> unit
(** [damage_render sprite is_player (_)] performs the damage animation
    of either the enemy or the player depedning on the [is_player]
    boolean *)

val draw_health_bar : int -> int -> int -> bool -> unit -> unit
(** [draw_health_bar max_hp before_current_hp after_current_hp is_player(_)]
    performs the health bar animation of either the enemy or the player
    depending on the [is_player] boolean *)
