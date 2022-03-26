val draw_exp_bar :
  int -> int -> int -> int -> int -> int -> int -> unit -> unit
(** [draw_exp_bar max_xp before_curr_xp after_curr_xp x y width height(_)]
    performs the xp gain animation*)

val draw_health_bar :
  int -> int -> int -> int -> int -> int -> int -> bool -> unit -> unit
(** [draw_health_bar max_hp before_curr_hp after_curr_hp x y width height(_)]
    performs the health bar gain/loss animation. *)
