(** Abstract Representation of the View

    This module containsa all the functions sued to load and render all
    the assets used in the Game *)

type sprite
(** The abstract type that represents a sprite. It stores the width and
    height of the sprite, all the pixels of the sprites, and the color
    palettes used to color the sprite.contents *)

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

val empty_sprite : sprite
(** Represents a blank sprite that draws nothing *)

val text_color : Graphics.color
(** Represents the color of the text.*)

(** {1 Getters and Setters}*)

val set_text_bg : sprite -> sprite -> unit
(** [set_text_bg bg_left bg_right] Sets the background sprites for the
    text screen. Takes in two sprites. [bg_left] draws the background
    starting at the bottom left corner of the screen. [bg_right] start
    drawing thebackground at the bottm midde of the screen. These
    sprites are drawn when [clear_text()] is called *)

val set_text_char_cap : int -> unit
(** [set_text_char_cap char_cap] Sets the maximum number of characters
    that can be drawn on the screen.*)

val get_dimension : sprite -> int * int
(** [get_dimension sprite] returns the width and height of a sprite as a
    tuple.*)

val set_sticky_text : bool -> unit -> unit
val set_erase_mode : bool -> unit -> unit
val set_synced_mode : bool -> unit
val set_font_size : int -> unit -> unit
val get_font_size : unit -> int

val clear_screen : unit -> unit
(** [clear_screen ()] Clears the screen with the background color.*)

val wait : int -> unit -> unit
(** [wait delay_time] waits for [delay_time] or util user input to
    continue. If [delay_time = -1] then it waits indefinetely until user
    input *)

val sync_draw : (unit -> unit) -> unit -> unit
val sync : bool -> unit -> unit

(** {1 Loading Sprites}*)

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

val clear_text : unit -> unit
(** [draw_text is_synced] clears the text screen by redrawing the text
    background sprites. The drawing function is synced if
    [is_synced = true] *)

val draw_string_colored :
  int -> int -> int -> int -> string -> Graphics.color -> unit -> unit
(** [draw_string_colored x y shadow_offset font_size text text_color]
    Draws the [text] at [x,y] of the screen with the given [text_color].
    [shadow_offset] represents the offset disstance of the shodow to the
    text, when drawn. *)

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

val draw_text_string : string -> unit -> unit
(** [draw_text_string text font_size (_)] draws the given [text] with
    the given [font_size]on the bottom of the screen. There is no text
    scroll.*)

val damage_render : sprite -> bool -> unit -> unit
(** [damage_render sprite is_player (_)] performs the damage animation
    of either the enemy or the player depedning on the [is_player]
    boolean *)

val draw_gradient : int -> int -> unit
(** (draw_gradient w h ) draws a cool radiant with width [w] adnd height
    [h].*)

val make_grayscale : sprite -> unit -> unit

val reset_rgb : sprite -> unit -> unit
(** (reset_rgb sprite () ) resets the rgb values of the sprite.*)

val draw_creature_effect :
  sprite -> bool -> int -> int -> int -> int -> unit -> unit

val lower_stat_effect : sprite -> bool -> unit -> unit
(** Cool Random Effects :)*)

val raise_stat_effect : sprite -> bool -> unit -> unit
(** Cool Random Effects :)*)
