


type pokemon
(** The abstract type that represents a pokemon. 
All of a pokemon stats and features will be stored in this type. *)

val create_pokemon: Yojson.Basic.t -> string  -> int -> Yojson.Basic.t
