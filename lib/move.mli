open Creature
(** Loads and handles all the moves performed during combat*)

type move_catgeory =
  | Physical
  | Special
  | Status

type move = {
  move_name : string;
  power : int;
  accuracy : float;
  mutable pp : int;
  etype : etype;
  category : move_catgeory;
  description : string;
  effect_ids : int list;
}
(** Loads and handles all the moves performed during combat*)

val get_move : string -> move
(** [get_move move_name] returns the the move*)

val execute_move : move -> creature -> creature -> creature * creature
(** [execute_move move attacker defender] returns the after math after
    the [attacker] performs the [move] on the [defender]. The aftermath
    is represented as a tuple: [(attacker,defender)].*)
