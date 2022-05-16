(** The module representing the game state*)

type state
(** Abstract type representing the current game state*)

val get_state : unit -> state
(** [get_state _] is the current state of the game*)

val player : unit -> Player.player
(** [player _] is the player of the game*)

val player_x : unit -> int
(** [player_x _] is the x position for the player*)

val player_y : unit -> int
(** [player_y _] is the x position for the player*)

val map : unit -> Map.t
(** [map _] is the current map of the game*)

val set_player : Player.player -> unit
val set_map : Map.t -> unit
val adhoc_init : unit -> unit
