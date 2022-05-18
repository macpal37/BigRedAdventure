(** Module for all the animations. *)

open Ui

type animation
(** Abstract type that represents an animation. It stores the frames and
    the boolean that determines whether an animation is done or not.*)

val make_animation :
  draw_func -> (animation -> unit -> unit) -> int -> animation
(** [make_animation rf anim_func sf] creates an animation from the
    redraw function [rf] and animation function [anim_func] with the
    frame of the animation starting at [sf].*)

val run_animation : animation -> unit
(** [run_animation anim] runs and displays the animation.*)

val display_text_box : string -> bool -> draw_func -> unit -> unit
(** [display_text_box text is_sticky refresh_func (_)] animates text box
    animated. The tex box displays [text]. [is_sticky] determines
    whether the text remains on the screen or dispapears afterwards.
    [refresh_func] is a [fun ()->()] function that refreshes. *)

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

val animate_lower_stat_effect : Draw.sprite -> bool -> draw_func -> unit
(** Cool Random Effects :)*)

val animate_raise_stat_effect : Draw.sprite -> bool -> draw_func -> unit
(** Cool Random Effects :)*)

val animate_switch_out : Draw.sprite -> bool -> draw_func -> unit

val animate_switch_in : Draw.sprite -> bool -> draw_func -> unit
(** [animate_switch switch_out switch_in player out_name in_name rf (_)]
    Animates the switch out/switch in animation using the [switch_out]
    [switch_in] sprites. [player] determines whether the player or enemy
    switches out. [rf] represents the refresh function.*)

val animate_capture :
  Spritesheet.sprite_sheet ->
  Draw.sprite ->
  bool list ->
  int ->
  draw_func ->
  unit
(** [animate_capture ss sprite r ball_type ref (_)] performs the
    capturing animation of the creature [sprite] using the sprite_sheet
    [ss] and the results [r] to determine the capture animation. *)

val animate_faint : Draw.sprite -> bool -> draw_func -> unit
(** [animate_faint sprite (_)] performs the fainting animation of the
    creature [sprite] *)

val animate_damage_render : Draw.sprite -> bool -> draw_func -> unit
(** [animate_damage_render sprite is_player (_)] performs the damage
    animation of either the enemy or the player depedning on the
    [is_player] boolean *)

val animate_evolution : Draw.sprite -> Draw.sprite -> draw_func -> unit
(** [animate_evolution old_sprite new_sprite (_)] performs the evolution
    animation. *)

val animate_status :
  Draw.sprite -> bool -> Creature.status -> draw_func -> unit
(** [animate_status sprite player status ref] Performs the status effect
    animation*)

val animate_switch_out_trainer :
  Draw.sprite ->
  Draw.sprite ->
  Spritesheet.sprite_sheet ->
  draw_func ->
  unit
(** [animate_switch_out_trainer switch_out switch_in sprite_sheet rf (_)]
    Animates the switch out/switch in animation for trainers using the
    [switch_out] [switch_in] sprites and the ball [sprite_sheet] [rf]
    represents the refresh function.*)
