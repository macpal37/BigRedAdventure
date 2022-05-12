open Draw
open Ui

type animation

val make_animation :
  draw_func -> (animation -> unit -> unit) -> int -> animation

val run_animation : animation -> unit
val display_text_box : string -> draw_func -> unit -> unit

val draw_health_bar :
  float -> float -> int -> int -> int -> int -> bool -> unit -> unit
(** [draw_health_bar max_hp curr_hp x y width height is_hptext (_)]
    draws the hp bar. *)

val draw_exp_bar :
  float -> float -> int -> int -> int -> int -> unit -> unit
(** [draw_exp_bar max_xp curr_xp x y width height(_)] draws the xp bar. *)

val animate_health_bar :
  float ->
  float ->
  float ->
  int ->
  int ->
  int ->
  int ->
  bool ->
  animation ->
  unit ->
  unit
(** [draw_health_bar max_hp before_curr_hp after_curr_hp x y width height is_hptext is_animated(_)]
    performs the health bar gain/loss animation. *)

val animate_exp_bar :
  float ->
  float ->
  float ->
  int ->
  int ->
  int ->
  int ->
  animation ->
  unit ->
  unit
(** [draw_exp_bar max_xp before_curr_xp after_curr_xp x y width height(_)]
    performs the xp gain animation*)

val animate_effect :
  sprite ->
  bool ->
  int ->
  int ->
  int ->
  int ->
  animation ->
  unit ->
  unit

val animate_lower_stat_effect :
  sprite -> bool -> animation -> unit -> unit
(** Cool Random Effects :)*)

val animate_raise_stat_effect :
  sprite -> bool -> animation -> unit -> unit
(** Cool Random Effects :)*)

val switch_out :
  sprite ->
  sprite ->
  bool ->
  string ->
  string ->
  animation ->
  unit ->
  unit

val animate_capture :
  Spritesheet.sprite_sheet ->
  sprite ->
  bool list ->
  int ->
  animation ->
  unit ->
  unit

val animate_faint : sprite -> bool -> animation -> unit -> unit
(** [animate_faint sprite (_)] performs the fainting animation of the
    creature [sprite] *)

val animate_damage_render : sprite -> animation -> unit -> unit
(** [damage_render sprite is_player (_)] performs the damage animation
    of either the enemy or the player depedning on the [is_player]
    boolean *)
