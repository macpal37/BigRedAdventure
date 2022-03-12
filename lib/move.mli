open Creature
(** Loads and handles all the moves performed during combat*)

val execute_move : move -> creature -> creature -> creature * creature
(** [execute_move move attacker defender] returns the after math after
    the [attacker] performs the [move] on the [defender]. The aftermath
    is represented as a tuple: [(attacker,defender)].*)
