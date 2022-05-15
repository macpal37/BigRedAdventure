open Draw

type t
(** abstract data type representing an entity *)

type coord = float * float
(** type representing the coordinate of a tile on a map. (0, 0) is the
    bottom left corner of the map *)

type entity_interaction =
| Trainer of string
| Sign
| GiveItem of string
| Grass
| Heal
| Merchant
| Door of string
| None

type orientation =
  | N
  | S
  | E
  | W  (** Type representing the four orientations *)

val load_entity : string -> t
(** [load_entity file] is the entity represented by [file]. Raises
    [Malformed_Json] if the json file is invalid *)

val get_trigger : t -> entity_interaction
(** [get_trigger n] returns what interaction triggers [n] *)

val get_orientation : t -> orientation
(** [get_orientation n] is the current direction [n] is facing *)

val get_position : t -> coord
(** [get_position n] returns [n]'s current position *)

val get_sprite : t -> sprite
(** [get_sprite n] returns [n]'s current sprite representation *)

val get_dialogue : t -> string
(** [get_dialogue n] returns the dialogue [n] gives *)

val is_obstacle : t -> bool
(** [is_obstacle e] *)

val update : t -> unit
(** [update npc] increments [npc]'s movement loop by one step *)

val interact : t -> unit
(** [interact n] is what happens when the player interacts with n *)

(* val go : t -> orientation -> int -> unit
(** [go e d n] moves n tiles in direction [d]. In addition, this stops
    [n]'s movement loop until it is restarted with [restart_loop n] *)

val turn : t -> orientation -> unit
(** [turn e d] turns [e] to face direction [d] *)

val wait : t -> int -> unit
(** [wait e t] makes [e] wait for [t] intervals *)

val is_static : t -> bool
(** [is_static e] returns whether e has a preset movement loop or not *) *)