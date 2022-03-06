(** Representation of static creature data.

    This module represents the data stored in creature_list files,
    including the stats and moves. It handles loading of that data from
    JSON as well as querying the data. *)

type creature
(** The abstract type that represents a creature. All of a creature
    stats and features will be stored in this type. *)

type stats = {
  mutable max_hp : int;
  mutable attack : int;
  mutable defense : int;
  mutable sp_attack : int;
  mutable sp_defense : int;
  mutable speed : int;
}

type stat =
  | HP
  | Attack
  | Defense
  | Sp_Attack
  | Sp_Defense
  | Speed

type etype =
  | Normal
  | Fire
  | Water
  | Grass
  | Fairy
  | None

val stat_to_string : stat -> string
val get_nature : creature -> string * stat * stat
val etype_to_string : etype -> string
val string_to_etype : string -> etype
val mod_stat : stats -> stat -> float -> int

val create_creature : string -> int -> creature
(** [create_creature name level] creates an instance of a creature of
    the following [name] with the specify [level]. This creature is
    unique, in the sense that its features differ slightly from creature
    to creature of the name [name] *)

val get_current_hp : creature -> int
(** [get_current_hp creature] Returns the [creature]'s current hp *)

val set_current_hp : creature -> int -> unit
(** [set_current_hp creature] Sets the [creature]'s current hp to
    [amount] *)

val get_types : creature -> etype * etype
(** [get_types creature] returns the types of the creature as a tuple of
    strings*)

val get_stats : creature -> stats

val get_type_mod : etype -> creature -> float
(** [get_type_mod attack_type defender] returns the damage modification
    caused by type resistances, weaknesses or immunities from the
    [defender] by the [attack_type].*)

(* val affect_stat : creature -> string -> int -> creature *)
(** [affect_stat target stat stages] returns a creature with the stats
    modified by the number of stages increased or decresed. *)

val get_stab_mod : creature -> etype -> float
val get_moves : creature -> string list
