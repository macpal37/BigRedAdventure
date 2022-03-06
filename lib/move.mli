open Creature
(** Loads and handles all the moves performed during combat*)

type move_catgeory =
  | Physical
  | Special
  | Status

type move = {
  name : string;
  power : int;
  accuracy : int;
  mutable pp : int;
  etype : etype;
  category : move_catgeory;
  description : string;
  effect_ids : int list;
}
(** Loads and handles all the moves performed during combat*)

val get_move : string -> move
(** Loads and handles all the moves performed during combat*)

val execute_move : string -> creature -> creature -> creature * creature
