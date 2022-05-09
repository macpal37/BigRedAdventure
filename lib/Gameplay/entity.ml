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

type entity_trigger =
  | Battle of creature list
  | KeyItem of string
  | Button of string
  | None
      (** If more types are needed, maybe polymorphism would be a better
          option? dunno how viable parsing a json would be then tho *)

type t = {
  handler_id : string;
  e_type : entity_trigger;
  mutable orie : orientation;
  mutable pos : coord;
  dialogue : string;
  mutable active : bool;
  mutable interval_frac : float;
  (* fields for movement/animation *)
  animations : (step, sprite Loopq.t) Hashtbl.t;
  priority_queue : step Queue.t;
  path : movement;
}

(* [increment] is how far this entity moves when update is called, also
   determines the duration of a pause and turns *)
let increment = 0.1
let load_entity = failwith "Unimplemented"
let get_trigger entity = entity.e_type
let get_orientation entity = entity.orie
let get_position entity = entity.pos
let get_handler entity = entity.handler_id
let get_sprite = failwith "Unimplemented"
(* Each entity must at least have a forward facing sprite *)

let get_dialogue n = n.dialogue
let update = failwith "Unimplemented"

let correct_step d n =
  let opposite_dir = function
    | N -> S
    | E -> W
    | S -> N
    | W -> E
  in
  if n < 0 then Step (opposite_dir d, ~-n)
  else if n = 0 then Turn d
  else Step (d, n)

let go e d n = e.priority_queue |> Queue.push (correct_step d n)
let turn = failwith "Unimplemented"
let wait = failwith "Unimplemented"
let in_motion = failwith "Unimplemented"
let restart_loop = failwith "Unimplemented"
let is_static entity = entity.path = None