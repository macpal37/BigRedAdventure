(** Module for the menu that display creature stats and moves.*)

val init : unit -> unit
(** [init ()] initializes the creature menu. *)

val set_creature : int -> unit
(** [set_creature i] sets the [i]th creature from the player's party as
    the creatuer being currently displayed by the menu. *)

val draw_status : int -> int -> Creature.status -> unit -> unit

val load_assets : unit -> unit
(** [load_assets ()] loads all the assets for the menu. *)
