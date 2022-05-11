open Creature
open Creature.Move

type bstatus =
  | Victory
  | Loss
  | Flee
  | Catch
  | Ongoing

type btype =
  | Trainer
  | Wild

type turn_status =
  | Choosing
  | Pending
  | Halfway
  | Finished

type damage_type =
  | SuperEffective
  | Effective
  | NotEffective
  | Immune

(** Type of Action taken by a creature *)
type action =
  | Damage of int * int * damage_type * bool
      (** Damage:
          [hp_before, hp_after, damage_type,
        is_critical_hit].*)
  | Heal of int * int  (** Heal: [hp before, hp after]*)
  | StatusGain of bool * status
      (** StatusGain: [gain_or_remove,status]*)
  | StatusEffect of status * int * int
      (** StatusEffect: [status,hp_before,hp_after]*)
  | StatGain of stat * int  (** StatGain: [stat,stages_gained]*)
  | Switch  (** StatGain: Whether the creautre is switched*)
  | Fainted  (** Fainted: Whether the creautre has fainted*)

type battle_creature = {
  mutable creature : creature;
  mutable current_move : move option;
  mutable stat_changes : stats;
  mutable status_effect : status;
  mutable active : bool;
  is_player : bool;
}

type battle_record = {
  mutable player_creatures : creature list;
  mutable enemy_creatures : creature list;
  battle_type : btype;
  mutable battle_status : bstatus;
  mutable catch_attempts : int;
  mutable escape_attempts : int;
  mutable player_battler : battle_creature;
  mutable enemy_battler : battle_creature;
  mutable turn_counter : int;
  mutable turn_pos : turn_status;
  mutable creatures_switched : creature list;
}

(** The abstract type that represents the standing data of a Pokemon
    battle at a given turn. This type will store the pokemon engaged in
    battle, as well as their evolving victory status.*)

val refresh_battle : (int -> int -> int -> unit -> unit) ref
val health_bar : (int -> int -> int -> bool -> bool -> unit -> unit) ref

val wild_init : creature list -> creature list -> battle_record
(**Initializes a battle record for a wild creature encounter.*)

val trainer_init : creature list -> creature list -> battle_record
(**Initializes a battle record for a trainer encounter.*)

val turn_builder : battle_record -> move option -> unit
(**Given a brecord and a move chosen for the player creature to execute,
   turn_builder will return a battle record with player move, enemy
   move, and turn_position ready for a battle phase. Raises:
   NotBuilderReady if turn_pos is not Choosing*)

val battle_sim_fh : battle_record -> unit
(**Given a turn-ready battle record, battle_sim_fh will execute the
   first half of battle based on the creature who has not yet acted.
   Raises: NotBattleReady if battle record's turn_pos is not Pending*)

val battle_sim_sh : battle_record -> unit
(**Given a halfway executed battle record, battle_sim_sh will execute
   the second half of battle based on the creature who has not yet
   acted. Raises: NotBattleReady if battle record's turn_pos is not
   Halfway*)

val run_away : battle_record -> unit
(**Given a battle record, checks if the player is able to run away. If
   so, return battle_record with victory status set as Flee.*)

val capture : battle_record -> float -> bool list
(**Given a battle record, checks if the player is able to catch the
   creature. If so, return battle_record with victory status set as
   Catch*)

val switch_player : battle_record -> creature -> creature list -> unit
val switching_pending : creature option ref
