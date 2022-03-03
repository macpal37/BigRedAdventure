(** Representation of static creature data.

    This module represents the data stored in creature_list files,
    including the stats and moves. It handles loading of that data from
    JSON as well as querying the data. *)

type creature
(** The abstract type that represents a creature. All of a creature
    stats and features will be stored in this type. *)

val create_creature : Yojson.Basic.t -> string -> int -> creature
val get_nature : unit -> unit
val gen_ivs : int list

val get_hp : creature -> int * int
(** Returns the creature's current and max hp as the tuple:
    (current_hp*max_hp) Used to display values*)

val get_stats : creature -> int list
(** Returns all of the creature's stats as a list The list can be ready
    as this: 1 - current_hp 2 - max_hp 3 - attack 4 - defense 5 -
    sp_attack 6 - sp_defense 7 - speed *)
