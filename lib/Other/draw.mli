(** Abstract Representation of the View

    This module contains all the functions used to load and render all
    the assets used in the Game *)

type sprite
(** The abstract type that represents a sprite. It stores the width and
    height of the sprite, all the pixels of the sprites, and the color
    palettes used to color the sprite *)

type color = int
(** A single int representing a color*)

type folder =
  | Creature_Folder
  | GUI_Folder
  | Tile_Folder
  | Item_Folder
      (** Variant that represents what folder to look into to retrieve
          the assets.*)

val width : int
(** Width of the screen*)

val height : int
(** Height of the screen*)

val tick_rate : float
(** The time the program should pause after each frame*)

val empty_sprite : sprite
(** Represents a blank sprite that draws nothing *)

val white : color
val red : color
val blue : color

val text_color : color
(** Represents the color of the text.*)

val text_color2 : color
(** Represents the color of the text.*)

val open_window : unit -> unit
(** Makes the window*)

val renderer : unit -> Sdlrender.t
(** DEBUG exposing renderer*)

val present : unit -> unit
(** Makes new drawing visible*)

val rgb : int -> int -> int -> color
(** mirrors Graphics.rgb*)

val set_draw_color : ?a:int -> int -> int -> int -> unit
val set_color : color -> unit

val fill_rect : int -> int -> int -> int -> unit
(** [fill_rect x y w h] fills a rectangle with bottom corner (x,y) and
    dimensions (w,h)*)

val set_line_width : int -> unit
(** Sets the width of draw functions*)

val draw_rect : int -> int -> int -> int -> unit
(** [draw_rect x y w h] draws a rectangle with bottom corner (x,y) and
    dimensions (w,h)*)

val current_x : unit -> int
val current_y : unit -> int
val moveto : int -> int -> unit
val rmoveto : int -> int -> unit

(** {1 Getters and Setters}*)

val get_dimension : sprite -> int * int
(** [get_dimension sprite] returns the width and height of a sprite as a
    tuple.*)

val set_active : sprite -> bool -> unit
(** [set_active s flag] sets the sprite [s] to be active or not based on
    the [flag] boolean .*)

val change_dpi : sprite -> int -> sprite

val wait : int -> unit -> unit
(** [wait delay_time] waits for [delay_time] or util user input to
    continue. If [delay_time = -1] then it waits indefinetely until user
    input *)

(** {1 Loading Sprites}*)

val sprite_path : string -> folder -> string

val load_creature : string -> unit -> sprite
(** [load_creature name ()] Creates a sprite from the given [name.png]
    file from the creature_sprites folder.*)

val load_sprite : string -> folder -> int -> unit -> sprite
(** [load_sprite name folder_type ()] Creates a sprite from the given
    [name.png] file from the given [folder_type].*)

val load_sprite_from_filepath : string -> int -> unit -> sprite
(** [load_sprite_from_filepath filepath ()] Creates a sprite from the
    given [filepath].*)

(** {1 Drawing Functions}*)

val draw_pixel : int -> int -> int -> unit -> unit
(** [draw_pixel x y s (_)] Draws a pixel of at the screen cordinate
    [x,y] wit the given size [s]*)

val draw_sprite : sprite -> int -> int -> unit -> unit
(** [draw_creature_pos sprite x y (_)] draws the [sprite] at the given
    position [x] [y] *)

val draw_sprite_centered : sprite -> int -> int -> unit -> unit
(** [draw_creature_pos sprite x y (_)] draws the [sprite] centered at
    [x] [y] *)

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

val draw_gradient : int -> int -> unit
(** (draw_gradient w h ) draws a cool radiant with width [w] adnd height
    [h].*)

val make_grayscale : sprite -> unit -> unit

val reset_rgb : sprite -> unit -> unit
(** (reset_rgb sprite () ) resets the rgb values of the sprite.*)

val add_rgb : sprite -> int -> int -> int -> unit -> unit

val create_sprite :
  int array -> Graphics.color list -> int -> int -> int -> sprite

val change_color : sprite -> int -> int -> unit
