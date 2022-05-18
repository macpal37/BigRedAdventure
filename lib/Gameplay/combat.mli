(** Representation of combat data.

    This module rerpesents all the data and logic needed to simulate a
    battle between creatures. This module *)

open Creature.Move

type bstatus =
  | Victory
  | Loss
  | Flee
  | Catch
  | Ongoing
      (** [bstatus ] the battle status of a battle to determine its
          reseult. *)

(** [btype ] the type of battle encountered. *)
type btype =
  | Trainer
      (** [Trainer] can't run away and can't capture their creature.
          Encountered by trainer entities*)
  | Wild
      (** [Wild] can runaway and can capture creature. Encountered by
          grass tiles.*)

type damage_type =
  | SuperEffective
  | Effective
  | NotEffective
  | Immune
      (** [damage_type ] the type of damage received by a creature.*)

(** [combat_status] the status of creature in combat.*)
type combat_status =
  | Status of Creature.status
  | Confused of int ref
  | Flinch
  | LeechSeed  (** Type of Action taken by a creature *)

(** [action] variant for all the actions made in a battle *)
type action =
  | ChooseMove of move
  | Damage of float * float * float * damage_type * bool
      (** Damage: [damage, damage_type,
        is_critical_hit].*)
  | Heal of float  (** Heal: [heal_amount]*)
  | StatusGain of bool * combat_status
      (** StatusGain: [gain_or_remove,status]*)
  | StatusEffect of combat_status * float * float * float
      (** StatusEffect: [status,damage]*)
  | MaxStat
  | StatGain of int  (** StatGain: [stat,stages_gained]*)
  | Switch of Creature.creature
      (** StatGain: Whether the creautre is switched*)
  | Fainted  (** Fainted: Whether the creautre has fainted*)

type battle_creature = {
  mutable creature : Creature.creature;
  mutable current_move : move option;
  mutable stat_changes : Creature.stats;
  mutable status_cond : (string * combat_status) list;
  mutable active : bool;
  is_player : bool;
}

type battle_record = {
  mutable player_creatures : Creature.creature list;
  mutable enemy_creatures : Creature.creature list;
  battle_type : btype;
  mutable battle_status : bstatus;
  mutable catch_attempts : int;
  mutable escape_attempts : int;
  mutable player_battler : battle_creature;
  mutable enemy_battler : battle_creature;
  mutable turn_counter : int;
  mutable creatures_switched : Creature.creature list;
}
(** The abstract type that represents the standing data of a Creature
    battle at a given turn. This type will store the pokemon engaged in
    battle, as well as their evolving victory status.*)

type battle_action = battle_creature * action * string

val battle_actions : battle_action list ref

val wild_init :
  Creature.creature list -> Creature.creature list -> battle_record
(**[wild_init p_cl e_cl] initializes a wild battle battle_record with
   [p_cl] creatures for the player and [e_cl] creatures for the enemy .*)

val trainer_init :
  Creature.creature list -> Creature.creature list -> battle_record
(**[trainer_init p_cl e_cl] initializes a trainer battle battle_record
   with [p_cl] creatures for the player and [e_cl] creatures for the
   trainer.*)

val battle_sim : battle_record -> move option -> unit
(**Given a brecord and a move chosen for the player creature to execute,
   turn_builder will return a battle record with player move, enemy
   move, and turn_position ready for a battle phase. Raises:
   NotBuilderReady if turn_pos is not Choosing*)

val run_away : battle_record -> unit
(**Given a battle record, checks if the player is able to run away. If
   so, return battle_record with victory status set as Flee.*)

val capture : battle_record -> float -> bool list
(**Given a battle record, checks if the player is able to catch the
   creature. If so, return battle_record with victory status set as
   Catch*)

val reset_battler : battle_creature -> unit
(**[reset_battler bc ] resets all mutable properties of battle_creature
   [bc].*)

val switch_player : battle_record -> Creature.creature -> unit
(**[switch_player bc c] Switches the current player_battler with
   creature [c] and modifies battle record [bc].*)

val switch_trainer : battle_record -> Creature.creature
(**[switch_trainer bc ] Switches the current enemy_battler with a random
   alive creature from [enemy_creatures] and modifeis the battle_record
   [bc].*)

val switching_pending : Creature.creature option ref
(** [switch_trainer] the current player creature pending to switch out.*)
