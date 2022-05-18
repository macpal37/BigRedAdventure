(** Module that reprsent all data for entities. *)

open Creature

type coord = int * int
(** type representing the coordinate of a tile on a map. (0, 0) is the
    bottom left corner of the map *)

type item_props = {
  mutable name : string;
  mutable given : bool;
  mutable disappear : bool;
}

(* [item_props]: record that stores all the properties of an item
   entity.*)

type trainer_props = {
  name : string;
  alt_dialogue : string;
  party : creature list;
  mutable sight : coord list;
}
(* [trainer_props]: record that stores all the properties of a trainer
   entity.*)

type entity_interaction =
  | Trainer of trainer_props (* trainer_props *)
  | Sign
  | Item of item_props
  | Grass
  | Heal
  | Merchant
  | Door of string * coord
  | NoEntity
(* [entity_interaction]: variant that describes all interactable
   code. *)

type orientation =
  | N
  | S
  | E
  | W  (** Type representing the four orientations *)

type entity = {
  e_type : entity_interaction;
  mutable orie : orientation;
  mutable pos : coord;
  mutable dialogue : string;
  sprite : Draw.sprite;
  mutable state : int;
  obstacle : bool;
}

(* val load_entity : string -> entity *)
(** [load_entity file] is the entity represented by [file]. Raises
    [Malformed_Json] if the json file is invalid *)

val get_trigger : entity -> entity_interaction
(** [get_trigger n] returns what interaction triggers [n] *)

val get_orientation : entity -> orientation
(** [get_orientation n] is the current direction [n] is facing *)

val get_position : entity -> coord
(** [get_position n] returns [n]'s current position *)

val get_sprite : entity -> Draw.sprite
(** [get_sprite n] returns [n]'s current sprite representation *)

val get_dialogue : entity -> string
(** [get_dialogue n] returns the dialogue [n] gives *)

val is_visible : entity -> bool

val is_obstacle : entity -> bool
(** [is_obstacle e] *)

(* val update : entity -> unit *)
(** [update npc] increments [npc]'s movement loop by one step *)

val interact : entity -> (unit -> Player.player) -> Ui.draw_func -> unit
(** [interact n] is what happens when the player interacts with n *)

val get_state : entity -> int
val set_state : entity -> int -> unit
val set_sight : entity -> coord list -> unit
val has_changed : entity -> bool

(* val go : entity -> orientation -> int -> unit (** [go e d n] moves n
   tiles in direction [d]. In addition, this stops [n]'s movement loop
   until it is restarted with [restart_loop n] *)

   val turn : entity -> orientation -> unit (** [turn e d] turns [e] to
   face direction [d] *)

   val wait : entity -> int -> unit (** [wait e t] makes [e] wait for
   [t] intervals *)

   val is_static : entity -> bool (** [is_static e] returns whether e
   has a preset movement loop or not *) *)
