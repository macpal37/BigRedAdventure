open Draw
open Ui

type animation

val make_animation :
  draw_func -> (animation -> unit -> unit) -> int -> animation

val run_animation : animation -> unit
val display_text_box : string -> bool -> draw_func -> unit -> unit

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
  draw_func ->
  unit
(** [draw_health_bar max_hp before_curr_hp after_curr_hp x y width height is_hptext(_)]
    performs the health bar gain/loss animation. *)

val animate_exp_bar :
  float ->
  float ->
  float ->
  int ->
  int ->
  int ->
  int ->
  draw_func ->
  unit
(** [draw_exp_bar max_xp before_curr_xp after_curr_xp x y width height(_)]
    performs the xp gain animation*)

val animate_lower_stat_effect : sprite -> bool -> draw_func -> unit
(** Cool Random Effects :)*)

val animate_raise_stat_effect : sprite -> bool -> draw_func -> unit
(** Cool Random Effects :)*)

val animate_switch_out : sprite -> bool -> draw_func -> unit
val animate_switch_in : sprite -> bool -> draw_func -> unit

(* val animate_switch : sprite -> sprite -> bool -> string -> string ->
   draw_func -> unit *)
(** [animate_switch switch_out switch_in player out_name in_name rf (_)]
    Animates the switch out/switch in animation using the [switch_out]
    [switch_in] sprites. [player] determines whether the player or enemy
    switches out. [rf] represents the refresh function.*)

val animate_capture :
  Spritesheet.sprite_sheet ->
  sprite ->
  bool list ->
  int ->
  draw_func ->
  unit

val animate_faint : sprite -> bool -> draw_func -> unit
(** [animate_faint sprite (_)] performs the fainting animation of the
    creature [sprite] *)

val animate_damage_render : sprite -> bool -> draw_func -> unit
(** [animate_damage_render sprite is_player (_)] performs the damage
    animation of either the enemy or the player depedning on the
    [is_player] boolean *)

val animate_evolution : sprite -> sprite -> draw_func -> unit
(** [animate_evolution old_sprite new_sprite (_)] performs the evolution
    animation. *)

val animate_status :
  sprite -> bool -> Creature.status -> draw_func -> unit
(** [animate_status sprite player status ref] Performs the status effect
    animation*)
