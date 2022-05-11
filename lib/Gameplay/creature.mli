(** Representation of creature data.

    This module represents the data stored in creature_list files,
    including the stats and moves. It handles loading of that data from
    JSON as well as querying the data. *)

(** {1 Abstract types}*)

type creature
(** The abstract type that represents a creature. All of a creature
    stats and features will be stored in this type. *)

(** {1 Concrete types}*)

exception NoEffect
exception MalformedJson
exception NoCreature

type stats = {
  mutable max_hp : float;
  mutable attack : float;
  mutable defense : float;
  mutable sp_attack : float;
  mutable sp_defense : float;
  mutable speed : float;
}
(** The mutable type that represents the stats of a creature *)

type status =
  | Healthy
  | Sleep of int
  | Freeze of int
  | Poison of int
  | Paralyze
  | Burn
  | Fainted

(** Represents the status of a creature*)

type stat =
  | HP
  | Attack
  | Defense
  | Sp_Attack
  | Sp_Defense
  | Speed

(** Represents a single stat name of a creature*)
(* type etype = | Neutral | Fire | Water | Air | Earth | Electric | Ice
   | Metal | Acid | Light | Shadow | Specter | Nature | Cosmic | None *)

type etype =
  | Normal
  | Fire
  | Water
  | Grass
  | Fairy
  | Rock
  | Ghost
  | Dark
  | Steel
  | Electric
  | Poison
  | Psychic
  | Ground
  | Dragon
  | Bug
  | Ice
  | Fighting
  | NoType

(** Represents the elemental type of a creature*)

(** {1 Moves}*)

(** Loads and handles all the moves performed during combat*)
module Move : sig
  type move_catgeory =
    | Physical
    | Special
    | Status  (** Moves used when in combat*)

  val etype_of_string : string -> etype
  (** [string_to_etype etype_name] returns the etype representation of
      the [etype_name] string.*)

  type move = {
    move_name : string;
    power : float;
    accuracy : float;
    mutable priority : int;
    mutable curr_pp : int;
    mutable max_pp : int;
    etype : etype;
    category : move_catgeory;
    description : string;
    effect_ids : int list;
    effect_chance : float;
  }

  val create_move : string -> move
  (** [get_create_movemove move_name] generates a move given by the
      [move_name] from the move_list.json.*)
end

(** {1 Creature Creation}*)

val create_creature : string -> int -> creature
(** [create_creature name level] creates an instance of a creature of
    the following [name] with the specify [level]. This creature is
    unique, in the sense that its features differ slightly from creature
    to creature of the name [name] *)

val mod_stat : stats -> stat -> float -> float
(** [mod_stat stats stat power] returns the modified value of the [stat]
    by [power]*)

val empty_stats : stats
(** Represents an emtpy set of stats*)

(** {1 String Formatting}*)

val string_of_stat_short : stat -> string
(** [stat_to_string stat] returns a string representation of the [stat].*)

val string_of_stat : stat -> string
(** [stat_to_string stat] returns a string representation of the [stat].*)

val string_of_etype : etype -> string
(** [etype_to_string etype] returns a string representation of the
    [etype].*)

val string_of_status : status -> string
(** [status_to_string status] returns a string representation of the
    [status].*)

(** {1 Getters and Setters}*)

val get_nature : creature -> string * stat * stat
(** [get_nature creature] returns the [creature]'s nature name, buff
    stat, and nerf stat as a tuple*)

val get_status : creature -> status
(** [get_status creature] returns the [creature]'s current status.*)

val get_current_hp : creature -> float
(** [get_current_hp creature] Returns the [creature]'s current hp *)

val set_current_hp : creature -> float -> unit
(** [set_current_hp creature] Sets the [creature]'s current hp to
    [amount] *)

val get_types : creature -> etype * etype
(** [get_types creature] returns the types of the creature as a tuple of
    strings*)

val get_stat2 : stats -> stat -> float

val get_stats : creature -> stats
(** [get_type_mod attack_type defender] returns the damage modification
    caused by type resistances, weaknesses or immunities from the
    [defender] by the [attack_type].*)

val get_stat : creature -> stat -> float
val get_ivs : creature -> stats
val get_evs : creature -> stats
val get_ev_gain : creature -> stat * float
val add_ev_gain : creature -> stat * float -> unit
val get_exp_gain : creature -> int
val add_hp : creature -> float -> unit

val get_type_mod : etype -> creature -> float
(** [get_type_mod attack_type defender] returns the damage modification
    caused by type resistances, weaknesses or immunities from the
    [defender] by the [attack_type].*)

val get_stab_mod : creature -> etype -> float
(** [get_stab_mod creature etype] returns the power boost gain when a
    [creature] uses a move with the same type. This function tests if
    [etype] is one of the creature's type and returns a float of the
    damage modification*)

val apply_status : creature -> status -> unit
(** [apply_status creature status] Applies a status effect onto a
    creture. Raises No_Effect exception if the [creature] does not have
    the [Healthy] status.*)

val remove_status : creature -> status -> unit
(** [remove creature status] Removes a status effect frok a creture.
    Raises No_Effect exception if the [creature] already has the
    [Healthy] status.*)

val get_moves : creature -> Move.move option array
(** [get_moves creature] returns all of the moves of the [creature] by
    name. Used in conjuction with Move.execute_move. *)

val get_catch_rate : creature -> float
(** [get_catch_rate creature] returns a [creature]'s catch rate a s
    float to be used to determine catching. *)

val get_level : creature -> int
(** [get_level creature] returns a [creature]'s current level*)

val level_up : creature -> unit -> unit
(** [level_up creature] levels up the creature by one level, modifying
    its stats.*)

val get_exp : creature -> int * int * int
(** [get_exp creature] returns a tuple that represents the creautre's
    exp [curr,min,max]*)

val add_exp : creature -> int -> (int * int * int * int) list
(** [add_exp creature amount] add [amount] to the current exp of
    [creature]. Returns a report of adding the exp as a ruple
    [max, before, after, level]*)

val add_pp : creature -> string -> int -> unit

val get_nickname : creature -> string
(** [get_nickname creature] returns a [creature]'s nickname*)

val set_nickname : creature -> string -> unit
(** [get_nickname creature] returns a [creature]'s nickname*)

val get_front_sprite : creature -> Draw.sprite
val set_front_sprite : creature -> Draw.sprite -> unit
val get_back_sprite : creature -> Draw.sprite
val set_back_sprite : creature -> Draw.sprite -> unit
val get_hp_status : creature -> float * float
val get_specias : creature -> string
val get_color_from_etype : etype -> Draw.color
val get_move_i : creature -> int -> Move.move option
