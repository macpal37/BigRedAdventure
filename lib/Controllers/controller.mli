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

val current_state : state ref
(** [current_state] is the current state of the game*)

val player : unit -> Player.player
(** [player _] is the player of game*)

val main : unit -> unit
(** Program Main*)
