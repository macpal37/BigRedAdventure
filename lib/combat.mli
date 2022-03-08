(*Parsing of player commands. *)
open Creature

type battle_record
(** The abstract type that represents the standing data of a Pokemon
    battle at a given turn. This type will store the pokemon engaged in
    battle, as well as their evolving victory status.*)

val wild_init : creature list -> creature list -> battle_record
(**Initializes a battle record for a wild creature encounter.*)

val trainer_init : creature list -> creature list -> battle_record
(**Initializes a battle record for a trainer encounter.*)

(*val get_moves : creature -> string list (**Given a creature,
  [get_moves] returns a string of all moves it current knows.*)*)

val execute_move : bool -> string -> battle_record -> battle_record
(**Given a creature, as well as a string containing the move ID,
   [execute_move] produces an updated battle record.*)

(* val rand_move : creature -> string *)
(**Given a creature, a move is randomly chosen from its list of
   acceptable moves*)

val run_away : battle_record -> battle_record
(**Given a battle record, checks if the player is able to run away. If
   so, return battle_record with victory status set as Flee.*)

val capture : battle_record -> battle_record
(**Given a battle record, checks if the player is able to catch the
   creature. If so, return battle_record with victory status set as
   Catch*)
