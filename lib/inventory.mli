(** Representation of an inventory

    This module represents a player inventory, divided into categories
    that track the number of copies the player has of items*)

open Item

exception Insufficient of int
(** Raised when attempting to consume more copies of an item than
    present. The associated integer is the actual number of copies
    present*)

type inventory
(** The abstract type that represents the inventory*)

type bag
(** The abstract type that represents a collection of items of a certain
    type*)

val new_inventory : unit -> inventory
(** [new_inventory _] is an empty inventory*)

val get_bag : inventory -> item_type -> bag
(** [get_bag i t] is the item bag in inventory [i] that stores item of
    type [t]*)

val list_items : bag -> (item * int) list
(** [list_items b] is list of pairs of items and their number of copies
    present in [b]*)

val add : bag -> ?count:int -> item -> unit
(** [add b ~count:c i] adds [c] copies of item [t] in bag [b]. [c] is 1
    by default. This call mutates [b]*)

val consume : bag -> ?count:int -> item -> unit
(** [consume b ~count:c i] removes [c] copies of item [t] from bag [b].
    [c] is 1 by default. This call mutates [b]. Raises [Insufficient r]
    if the number of copies [r] of item [t] is less than [c]*)
