(** Abstract Representation of the View

    This module containsa all the functions sued to load and render all
    the assets used in the Game *)

type pixel
(** The abstract type that represents a pixel. It stores the (x,y)
    position and the r,g,b values of the pixel. It is used to render
    images ont the screen.*)

val pokemon_sprite_size : int

val draw_pixel : int -> int -> int -> unit -> unit
(** Draws a pixel of a given (size) to the screen on the (x,y)
    coordinate provided*)

val load_creature : string -> unit -> string list
(** Loads a given (filename) from the pokemon_sprites*)

val draw_sprite :
  string list -> int -> int -> int -> int -> unit -> unit

val draw_creature_pos : string list -> int -> int -> unit -> unit
val draw_creature : string list -> bool -> unit -> unit
val draw_text : string -> unit -> unit
val gradient : int array array -> int -> int -> unit
val draw_gradient : int -> int -> unit
val faint : int -> int -> string list -> unit -> unit
val damage_render : string list -> bool -> unit -> unit
val draw_health_bar : int -> int -> int -> bool -> unit -> unit
