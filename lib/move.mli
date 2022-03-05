open Creature
(** Loads and handles all the moves performed during combat*)

type move = {
  name : string;
  power : int;
  accuracy : int;
  mutable pp : int;
  effect_ids : int list;
  description : string;
}
(** Loads and handles all the moves performed during combat*)

val get_move : string -> move
(** Loads and handles all the moves performed during combat*)

val execute_move : string -> creature -> creature -> creature * creature
