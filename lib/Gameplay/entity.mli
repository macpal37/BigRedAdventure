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
  | Win
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
(** [is_visible e] returns true if entity [e] is visible. *)

val is_obstacle : entity -> bool
(** [is_obstacle e] returns true if entity [e] is an obstacle. *)

(* val update : entity -> unit *)
(** [update npc] increments [npc]'s movement loop by one step *)

val interact : entity -> (unit -> Player.player) -> Ui.draw_func -> unit
(** [interact n] is what happens when the player interacts with n *)

val get_state : entity -> int
(** [get_state e] returns the state of the entity. *)

val set_state : entity -> int -> unit
(** [set_state e i] sets the state of entity [e] to [i]. This mutates
    entity. *)

val set_sight : entity -> coord list -> unit
(** [set_sight e cl] sets the sight of entity [e] to [cl]. This mutates
    entity. *)

val has_changed : entity -> bool
(** [has_changed e ] returns true if entity has changed.*)
