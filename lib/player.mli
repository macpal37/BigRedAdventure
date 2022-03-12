(** Representation of core player attributes

    This module represents the attributes of the player, such as their
    name, or which badges they have explored*)

type player
(** The abstract type that represents a player*)

val new_player : string -> player
(** [new_player s] is a player with name [s], no money, no time played,
    no badges, an empty party, no creatures, an empty inventory*)

val name : player -> string
(** [name p] is the name of player [p]*)

val money : player -> int
(** [money p] is the money held by player [p]*)

val inventory : player -> Inventory.inventory
(** [inventory p] is the inventory of [p]*)

val time_played : player -> int
(** [time_played p] is the time played by player [p]*)

val badges : player -> string list
(** [badges p] are the badges held by player [p]*)

val has_badge : string -> player -> bool
(** [badges b p] is whether player [p] has badge [b]*)

val party : player -> Creature.creature list
(** [party p] is the party of player [p]*)

val set_party : Creature.creature list -> player -> unit
(** [set_party party player] sets the party of [player] to [party]. This
    call mutates [player]*)

val creatures : player -> Creature.creature list
(** [creatures p] is the list of all the creatures player [p] has*)

val add_creature : Creature.creature -> player -> unit
(** [add_creature c p] adds creature [c] to player [p]. This call
    mutates [p]*)

val remove_creature : Creature.creature -> player -> unit
(** [remove_creature c p] removes creature [c] from player [p]. This
    call mutates [p] Raises [Failure] if the player possess no such
    creature [c]*)

val add_money : int -> player -> unit
(** [add_money i p] adds [i] money to player [p]. This call mutates [p]*)

val add_time_played : int -> player -> unit
(** [add_time_played i p] adds [i] time played to player [p]. This call
    mutates [p]*)

val add_badge : string -> player -> unit
(** [add_badge i p] adds badge [b] to player [p]. This call mutates [p]*)
