(** The module representing the game state*)

type state
(** Abstract type representing the current game state*)

val get_state : unit -> state
(** [get_state _] is the current state of the game*)

val player : unit -> Player.player
(** [player _] is the player of game*)
