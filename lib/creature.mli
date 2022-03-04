(** Representation of static creature data.

    This module represents the data stored in creature_list files,
    including the stats and moves. It handles loading of that data from
    JSON as well as querying the data. *)

type creature
(** The abstract type that represents a creature. All of a creature
    stats and features will be stored in this type. *)

val create_creature : string -> int -> creature
(** [create_creature name level] creates an instance of a creature of
    the following [name] with the specify [level]. This creature is
    unique, in the sense that its features differ slightly from creature
    to creature of the name [name] *)

val get_hp : creature -> int * int
(** [get_hp creature] Returns the [creature]'s current and max hp as the
    tuple: (current_hp,max_hp). Used to display values*)

val get_stat : creature -> string -> int
(** [get_stat creature stat] returns the value of the specific [stat]
    from that [creature]*)

val get_types : creature -> string * string
(** [get_types creature] returns the types of the creature as a tuple of
    strings*)

val get_stats : creature -> int list
(** [get_stats creature] returns all of the [creature]'s stats as a
    list. Here is the the list of value by index:

    - 1: Max HP
    - 2: Current HP
    - 3: Attack
    - 4: Defense
    - 5: Special Attack
    - 6: Special Defense
    - 7: Speed The list is ended by the blank line.*)

(** Combat-Related Functions*)

val get_type_mod : string -> creature -> float
(** [get_type_mod attack_type defender] returns the damage modification
    caused by type resistances, weaknesses or immunities from the
    [defender] by the [attack_type].*)

(* val affect_stat : creature -> string -> int -> creature *)
(** [affect_stat target stat stages] returns a creature with the stats
    modified by the number of stages increased or decresed. *)
