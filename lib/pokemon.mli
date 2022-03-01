


type pokemon
(** The abstract type that represents a pokemon. 
All of a pokemon stats and features will be stored in this type. *)


val create_pokemon: Yojson.Basic.t -> string  -> int -> pokemon

val get_nature: unit -> unit

val gen_ivs: int list

val get_hp: pokemon -> (int*int)
(** Returns the pokemon's current and max hp as the tuple: (current_hp*max_hp) 
    Used to display values*)

val get_stats: pokemon -> int list
(** Returns all of the pokemon's stats as a list
    The list can be ready as this:
    1 - current_hp
    2 - max_hp
    3 - attack
    4 - defense
    5 - sp_attack
    6 - sp_defense
    7 - speed
    *)