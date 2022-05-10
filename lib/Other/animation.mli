open Draw

val draw_exp_bar :
  int -> int -> int -> int -> int -> int -> int -> unit -> unit
(** [draw_exp_bar max_xp before_curr_xp after_curr_xp x y width height(_)]
    performs the xp gain animation*)

val draw_health_bar :
  int ->
  int ->
  int ->
  int ->
  int ->
  int ->
  int ->
  bool ->
  bool ->
  unit ->
  unit
(** [draw_health_bar max_hp before_curr_hp after_curr_hp x y width height is_hptext is_animated(_)]
    performs the health bar gain/loss animation. *)

val draw_creature_effect :
  sprite -> bool -> int -> int -> int -> int -> unit -> unit

val lower_stat_effect : sprite -> bool -> unit -> unit
(** Cool Random Effects :)*)

val raise_stat_effect : sprite -> bool -> unit -> unit
(** Cool Random Effects :)*)

val switch_out :
  sprite ->
  sprite ->
  bool ->
  string ->
  string ->
  (unit -> unit) ->
  unit ->
  unit

val capture_animation :
  Spritesheet.sprite_sheet ->
  sprite ->
  bool list ->
  int ->
  (int -> unit -> unit) ->
  unit ->
  unit

val animate_faint : sprite -> bool -> unit -> unit
(** [animate_faint sprite (_)] performs the fainting animation of the
    creature [sprite] *)
