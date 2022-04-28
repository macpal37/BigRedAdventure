open Creature
open Draw

type t
(** abstract data type representing an entity *)

type coord = float * float
(** type representing the coordinate of a tile on a map. (0, 0) is the
    bottom left corner of the map *)

type entity_type =
  | Trainer of creature list
  | KeyItem of string
  | ButtonItem of string

type direction =
  | N
  | S
  | E
  | W

val load_entity : string -> t
(** [load_npc file] is the npc represented by [file]. Raises
    [Malformed_Json] if the json file is invalid *)

val get_entity : t -> entity_type
(** [get_type n] returns the type of npc [n] is *)

val get_direction : t -> direction
(** [get_direction n] *)

val get_coord : t -> coord
(** [get_coord n] returns [n]'s current position *)

val get_sprite : unit -> Draw.sprite
(** [get_sprite n] returns [n]'s current sprite representation *)

val get_dialogue : t -> string list
(** [get_dialogue n] returns the dialogue [n] gives *)

val update : t -> unit
(** [update npc] increments [npc]'s movement loop by one step and
    returns [npc]'s new coordinate *)

val go : t -> direction -> unit
(** [go n d] moves one tile in direction [d] *)

val turn : t -> direction -> unit
(** [turn n d] turns [n] to face [d] *)

val in_animation : t -> bool
(** [in_animation n] is [true] if [n] is currently in an animation and
    false otherwise *)