(** Representation of core player attributes

    This module represents the attributes of the player, such as their
    name, or which badges they have explored*)

type player
(** The abstract type that represents a player*)

val new_player : string -> player
(** [new_player s] is a player with name [s], no money, no time played,
    no badges*)

val name : player -> string
(** [name p] is the name of player [p]*)

val money : player -> int
(** [money p] is the money held by player [p]*)

val time_played : player -> int
(** [time_played p] is the time played by player [p]*)

val badges : player -> string list
(** [badges p] are the badges held by player [p]*)

val add_money : int -> player -> player
(** [add_money i p] is player [p] with [i] more money*)

val add_time_played : int -> player -> player
(** [add_time_played i p] is player [p] with [i] more time played*)

val add_badge : string -> player -> player
(** [add_badge b p] is player [p] with added badge [b]*)
