(** Abstract Representation of the View

    This module containsa all the functions sued to load and render all
    the assets used in the Game *)

type sprite
(** The abstract type that represents a pixel. It stores the (x,y)
    position and the r,g,b values of the pixel. It is used to render
    images ont the screen.*)

val width : int
val height : int
val empty_sprite : sprite
val text_color : Graphics.color
val set_text_bg : sprite -> sprite -> unit
val set_text_char_cap : int -> unit
val get_dimension : sprite -> int * int

(*** ================= Set Functions Functions =================**)
val set_sticky_text : bool -> unit -> unit
val set_erase_mode : bool -> unit -> unit
val set_synced_mode : bool -> unit
val set_font_size : int -> unit -> unit
val get_font_size : int

val wait : int -> unit -> unit
(** [wait delay_time] waits for [delay_time] or util user input to
    continue. If [delay_time = -1] then it waits indefinetely until user
    input *)

val sync_draw : (unit -> unit) -> unit -> unit
val sync : bool -> unit -> unit

val load_creature : string -> unit -> sprite
(** [load_creature name (_)]Loads a given [name.json] from the creatures
    sprites and generates a lsit that represents the sprites. To be sed
    in conjuction with draw_sprite*)

val load_sprite : string -> int -> unit -> sprite

(*** =================DRAW Functions =================**)

val clear_text : unit -> unit
(** [draw_text is_synced] clears the text screen by redrawing the text
    background sprites. The drawing function is synced if
    [is_synced = true] *)

val draw_string_colored :
  int -> int -> int -> string -> Graphics.color -> unit -> unit

val draw_pixel : int -> int -> int -> unit -> unit
(** Draws a pixel of a given (size) to the screen on the (x,y)
    coordinate provided*)

val clear_sprite : sprite -> int -> int -> unit -> unit
(** [draw_creature_pos sprite x y (_)] draws the [sprite] at the given
    position [x] [y] *)

val draw_sprite : sprite -> int -> int -> unit -> unit
(** [draw_creature_pos sprite x y (_)] draws the [sprite] at the given
    position [x] [y] *)

val draw_creature : sprite -> bool -> unit -> unit
(** [draw_creature_pos sprite is_player (_)] draws the [sprite] at
    combat positions. If [is_player] is true then it draws the [sprite]
    in the lower left corner of the screen. Otherwise its an enemy and
    draws the [sprite] on the upper right corner of the screen.*)

val draw_sprite_crop :
  sprite -> int -> int -> int * int -> int * int -> unit -> unit
(** [draw_sprite sprite x y (width_min, width_max)
    (height_min, height_max) (_)]
    draws an image from the given [sprite] representation at [x] and [y]
    with cropped based on the bounds defined*)

val draw_text : string -> int -> bool -> unit -> unit
(** [draw_text text font_size auto_scroll (_)] draws the given [text]
    with the given [font_size]on the bottom of the screen and simulates
    text scrolling. If [auto_scroll] is on, then it will scroll
    automatically after a delay*)

val damage_render : sprite -> bool -> unit -> unit
(** [damage_render sprite is_player (_)] performs the damage animation
    of either the enemy or the player depedning on the [is_player]
    boolean *)

val draw_gradient : int -> int -> unit
