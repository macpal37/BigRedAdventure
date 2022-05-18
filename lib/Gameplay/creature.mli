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
(** [NoEffect] raised when an an action on a creature has no effect *)

exception MalformedJson
(** [MalformedJson] raised when the creature json is malformed *)

exception NoCreature
(** [NoCreature] raised when no creature of the provided name exists. *)

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
  | Sleep of int ref
  | Poison of int ref
  | Freeze
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

val null_creature : creature
(** [null_creature] represents an empty creature*)

val empty_stats : unit -> stats
(** Represents an emtpy set of stats*)

(** {1 Creature Creation}*)

val create_creature : string -> int -> creature
(** [create_creature name level] creates an instance of a creature of
    the following [name] with the specify [level]. This creature is
    unique, in the sense that its features differ slightly from creature
    to creature of the name [name] *)

val mod_stat : stats -> stat -> float -> float
(** [mod_stat stats stat power] returns the modified value of the [stat]
    by [power]*)

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
(** [get_stat c s] returns the stat value of the respective stat [s]
    from creature [c]. *)

val get_ivs : creature -> stats
(**[get_ivs c] returns the ivs of creature [c]. *)

val get_evs : creature -> stats
(**[get_evs c] returns the evs of creature [c]. *)

val get_ev_gain : creature -> stat * float
(**[get_evs c] returns the ev_gain of creature [c]. *)

val add_ev_gain : creature -> stat * float -> unit
(**[add_ev_gain c ev_gain] adds the respective [(ev_state,ev_amount)]
   from [ev_gain] gain to creature [c] mutating it. *)

val get_exp_gain : creature -> float
(**[get_exp_gain c] returns the amount of exp earned from defeating
   creature [c]. *)

val add_hp : creature -> float -> unit
(**[add_hp c hp] adds [hp] amount of hp to creature [c] and mutates it. *)

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
(** [remove creature status] Removes a status effect from a creture.
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

val get_exp : creature -> float * float * float
(** [get_exp creature] returns a tuple that represents the creautre's
    exp [curr,min,max]*)

val add_exp : creature -> float -> (float * float * float * int) list
(** [add_exp creature amount] add [amount] to the current exp of
    [creature]. Returns a report of adding the exp as a ruple
    [max, before, after, level]*)

val add_pp : creature -> string -> int -> unit
(** [add_pp creature move amount] adds [amount] of pp to the [move] from
    the [creature]*)

val get_nickname : creature -> string
(** [get_nickname creature] returns a [creature]'s nickname*)

val set_nickname : creature -> string -> unit
(** [get_nickname creature] returns a [creature]'s nickname*)

val get_front_sprite : creature -> Draw.sprite
(** [get_front_sprite creature] returns the front_sprite of the
    [creature]*)

val get_back_sprite : creature -> Draw.sprite
(** [get_back_sprite creature] returns the back_sprite of the [creature]*)

val get_hp_status : creature -> float * float
(** [get_hp_status creature] returns the [max] and [current] hp of the
    [creature] as a tuple.*)

val get_specias : creature -> string
(** [get_specias creature] returns the species name of the creature*)

val get_color_from_etype : etype -> Draw.color
(** [get_color_from_etype etype] returns a color that corresponds to the
    elemental type [etype]. *)

val get_move_i : creature -> int -> Move.move option
(** [get_move_i creature i] returns the [i]th move from the [creature] *)

val num_moves : creature -> int
(** [num_moves creature i] returns the number of moves from the
    [creature] *)

val add_move : creature -> Move.move -> unit
(** [add_move creature move] adds a [move] to the last slot of
    [creature] *)

val add_move_i : creature -> Move.move -> int -> unit
(** [num_moves creature move i] replaces the [i]th move with [move] of
    the [creature] *)

val get_evolution_name : creature -> string
(** [get_evolution_name c] returns the evolution name of creature [c]*)

val can_evolve : creature -> bool
(** [can_evolve c] determiens whther a creature [c] can evolve.*)

val evolve : creature -> unit
(** [evolve c] mutates the creature [c] and evovles it to its next
    stage.*)

val is_shiny : creature -> bool
(** [is_shiny c] returns true if [c] is shiny, false otherwise. *)

val level_up_move : creature -> Move.move
(** [level_up_move creature] returns a possible move when levels up.
    Raise Not_foundwhen when ther is no possible move.*)

val serialize : creature -> Yojson.Basic.t
(** [serialize c] is the json representation of [c]*)

val deserialize : Yojson.Basic.t -> creature
(** [deserialize j] is the creature encoded by [j]*)
