(** The high level controller of the game

    This module primarily takes care of user inputs and forwarding them
    to the appropriate game module*)

val tick_rate : float
(** The time the program should pause after each frame*)

type modes =
  | ModeOverworld
  | ModeBattle
  | ModeMenu
      (** The modes the game may be in; specifies which module is
          handling ticks*)

type state
(** Abstract type representing the current game state*)

val get_state : unit -> state
(** [get_state _] is the current state of the game*)

val get_player : state -> Player.player
(** [player s] is the player of game state [s]*)

val main : unit -> unit
(** Program Main*)
