open Creature
open Draw

type t
(** abstract data type representing an entity *)

type coord = float * float
(** type representing the coordinate of a tile on a map. (0, 0) is the
    bottom left corner of the map *)

type entity_trigger =
  | Battle of creature list
  | KeyItem of string
  | Button of string
  | None

type orientation =
  | N
  | S
  | E
  | W  (** Type representing the four orientations *)

val load_entity : string -> t
(** [load_npc file] is the npc represented by [file]. Raises
    [Malformed_Json] if the json file is invalid *)

val get_trigger : t -> entity_trigger
(** [get_trigger n] returns what interaction triggers [n] *)

val get_orientation : t -> orientation
(** [get_orientation n] is the current direction [n] is facing *)

val get_position : t -> coord
(** [get_position n] returns [n]'s current position *)

val get_sprite : unit -> sprite
(** [get_sprite n] returns [n]'s current sprite representation *)

val get_handler : t -> string
(** [get_handler n] returns the id of [n]'s associated handler *)

val get_dialogue : t -> string list
(** [get_dialogue n] returns the dialogue [n] gives *)

val update : t -> unit
(** [update npc] increments [npc]'s movement loop by one step *)

val go : t -> orientation -> int -> unit
(** [go e d n] moves n tiles in direction [d]. In addition, this stops
    [n]'s movement loop until it is restarted with [restart_loop n] *)

val turn : t -> orientation -> unit
(** [turn n d] turns [n] to face direction [d] *)

val wait : t -> int -> unit
(** [wait n t] makes [n] wait for [t] intervals *)

val in_motion : t -> bool
(** [in_motion n] is [true] if [n] is currently in motion and false
    otherwise *)

val restart_loop : t -> unit
(** *)

val is_static : t -> bool
(** [is_static e] returns whether e has a preset movement loop or not *)