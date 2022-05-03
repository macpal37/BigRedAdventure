open Creature
open Draw

type coord = float * float

type orientation =
  | N
  | E
  | S
  | W

type step =
  | Step of orientation * int
  | Pause of int
  | Turn of orientation
      (** Type representing an instruction for moving the entity *)

type movement =
  | Area of (coord, unit) Hashtbl.t
      (** replace this with a set later on *)
  | Path of step Loopq.t
  | None

type entity_type =
  | Trainer of creature list
  | KeyItem of string
  | Button of string

type t = {
  handler_id : string;
  e_type : entity_type;
  mutable orie : orientation;
  mutable pos : coord;
  dialogue : string;
  active : bool;
  (* fields for movement/animation *)
  animations : (step, sprite Loopq.t) Hashtbl.t;
  priority_queue : step Queue.t;
  path : movement;
}

(* [increment] is how far this entity moves when update is called *)
let increment = 0.1
let load_entity = failwith "Unimplemented"
let get_type entity = entity.e_type
let get_orientation entity = entity.orie
let get_position e = e.pos
let get_handler = failwith "Unimplemented"
let get_sprite = failwith "Unimplemented"
let get_dialogue n = n.dialogue
let update = failwith "Unimplemented"
let go = failwith "Unimplemented"
let turn = failwith "Unimplemented"
let in_motion = failwith "Unimplemented"
let restart_loop = failwith "Unimplemented"
let is_static entity = entity.path = None