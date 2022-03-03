(** Abstract Representation of the View

    This module containsa all the functions sued to load and render all
    the assets used in the Game *)

type pixel
(** The abstract type that represents a pixel. It stores the (x,y)
    position and the r,g,b values of the pixel. It is used to render
    images ont the screen.*)

val draw_pixel : int -> int -> int -> unit -> unit
(** Draws a pixel of a given (size) to the screen on the (x,y)
    coordinate provided*)

val load_pet : string -> unit -> string list
(** Loads a given (filename) from the pokemon_sprites*)

val draw_sprite :
  string list -> int -> int -> int -> int -> unit -> unit

val draw_pet : string list -> int -> int -> unit -> unit
