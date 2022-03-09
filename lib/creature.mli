(** Representation of creature data.

    This module represents the data stored in creature_list files,
    including the stats and moves. It handles loading of that data from
    JSON as well as querying the data. *)

(** {1 Abstract types}*)

type creature
(** The abstract type that represents a creature. All of a creature
    stats and features will be stored in this type. *)

(** {1 Concrete types}*)

type stats = {
  mutable max_hp : int;
  mutable attack : int;
  mutable defense : int;
  mutable sp_attack : int;
  mutable sp_defense : int;
  mutable speed : int;
}
(** The mutable type that represents the stats of a creature *)

type status =
  | Healthy
  | Sleep
  | Freeze
  | Paralyze
  | Poison
  | Burn

(** Represents the status of a creature*)

type stat =
  | HP
  | Attack
  | Defense
  | Sp_Attack
  | Sp_Defense
  | Speed

(** Represents a single stat name of a creature*)

type etype =
  | Normal
  | Fire
  | Water
  | Grass
  | Fairy
  | None

(** Represents the elemental type of a creature*)

(** {1 Creature Creation}*)

val create_creature : string -> int -> creature
(** [create_creature name level] creates an instance of a creature of
    the following [name] with the specify [level]. This creature is
    unique, in the sense that its features differ slightly from creature
    to creature of the name [name] *)

val mod_stat : stats -> stat -> float -> int
(** [mod_stat stats stat power] returns the modified value of the [stat]
    by [power]*)

(** {1 String Formatting}*)

val stat_to_string : stat -> string
(** [stat_to_string stat] returns a string representation of the [stat].*)

val etype_to_string : etype -> string
(** [etype_to_string etype] returns a string representation of the
    [etype].*)

val string_to_etype : string -> etype
(** [string_to_etype etype_name] returns the etype representation of the
    [etype_name] string.*)

val status_to_string : status -> string
(** [status_to_string status] returns a string representation of the
    [status].*)

(** {1 Getters and Setters}*)

val get_nature : creature -> string * stat * stat
(** [get_nature creature] returns the [creature]'s nature name, buff
    stat, and nerf stat as a tuple*)

val get_status : creature -> status
(** [get_status creature] returns the [creature]'s current status.*)

val get_current_hp : creature -> int
(** [get_current_hp creature] Returns the [creature]'s current hp *)

val set_current_hp : creature -> int -> unit
(** [set_current_hp creature] Sets the [creature]'s current hp to
    [amount] *)

val get_types : creature -> etype * etype
(** [get_types creature] returns the types of the creature as a tuple of
    strings*)

val get_stats : creature -> stats
(** [get_type_mod attack_type defender] returns the damage modification
    caused by type resistances, weaknesses or immunities from the
    [defender] by the [attack_type].*)

val get_type_mod : etype -> creature -> float
(** [get_type_mod attack_type defender] returns the damage modification
    caused by type resistances, weaknesses or immunities from the
    [defender] by the [attack_type].*)

val get_stab_mod : creature -> etype -> float
(** [get_stab_mod creature etype] returns the power boost gain when a
    [creature] uses a move with the same type. This function tests if
    [etype] is one of the creature's type and returns a float of the
    damage modification*)

val get_moves : creature -> string list
(** [get_moves creature] returns all of the moves of the [creature] by
    name. Used in conjuction with Move.execute_move. *)

val get_catch_rate : creature -> float
(** [get_catch_rate creature] returns a [creature]'s catch rate a s
    float to be used to determine catching. *)

val get_level : creature -> int
(** [get_level creature] returns a [creature]'s current level*)

val get_exp : creature -> int
(** [get_exp creature] returns a [creature]'s current exp*)

val add_exp : creature -> int -> int
(** [add_exp creature amount] add [amount] to the current exp of
    [creature]*)

val get_nickname : creature -> string
(** [get_nickname creature] returns a [creature]'s nickname*)

val set_nickname : creature -> string -> unit
(** [get_nickname creature] returns a [creature]'s nickname*)
