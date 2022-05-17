open Draw
open Creature
(* open Yojson.Basic.Util *)

type coord = int * int

module Coord = struct
  type t = int * int

  let compare (x1, y1) (x2, y2) =
    if x1 > x2 || (x1 = x2 && y1 > y2) then 1
    else if x1 = x2 && y1 = y2 then 0
    else -1
end

module CoordSet = Set.Make (Coord)

type orientation =
  | N
  | S
  | E
  | W

(* type step = | Step of orientation * int | Pause of int | Turn of
   orientation (** Type representing an instruction for moving the
   entity *) *)

(** Variant type representing the type of sprite *)
(* type sprite_step = | SStep of orientation | STurn of orientation

   type stop_time = | TPause of bool | TInactive

   type movement = | Area of (coord, unit) Hashtbl.t | Path of step
   Loopq.t | MNone *)

type item_props = {
  mutable name : string;
  mutable given : bool;
  mutable disappear : bool;
}

type trainer_props = {
  name:string;
  alt_dialogue:string;
  party:creature list;
  mutable sight:coord list;
}

type entity_interaction =
  | Trainer of string (* trainer_props *)
  | Sign
  | Item of item_props
  | Grass
  | Heal
  | Merchant
  | Door of string * coord
  | NoEntity

type entity = {
  e_type : entity_interaction;
  mutable orie : orientation;
  mutable pos : coord;
  dialogue : string;
  sprite : sprite;
  mutable state : int;
  (* path : movement; *)
  (* animations : (sprite_step, sprite array) Hashtbl.t;*)
  obstacle : bool;
      (* fields for movement/animation *)
      (*mutable interval_frac : float; priority_queue : step Queue.t; *)
}

(* [increment] is how far this entity moves when update is called, also
   determines the duration of a pause and turns *)
(* let increment = 0.1 *)
(* let load_entity = failwith "Unimplemented" *)
let get_trigger entity = entity.e_type
let get_orientation entity = entity.orie
let get_position entity = entity.pos
let is_obstacle e = if e.state = 0 then e.obstacle 
  else match e.e_type with
  | Item i -> not i.disappear
  | _ -> true

let is_visible e = if e.state = 0 then true 
  else match e.e_type with
  | Item i -> not i.disappear
  | _ -> true

(** 0 = show, 1 = invisible *)
let get_state e = e.state

let set_state e i = e.state <- i

(* (** [step_to_sprite s o] is what type of sprite to render based on
   the type of step occuring [s] and the entity's current orientation
   [o] *) let step_to_sprite step orie = match step with | Step (o, _)
   -> SStep o | Pause _ -> STurn orie | Turn o -> STurn o

   let get_adjacent_coords (x, y) = [(x +. 1.0, y); (x -. 1.0, y); (x, y
   +. 1.0); (x, y -. 1.0)]

   let randomelement arr = let n = Random.int (Array.length arr) in
   Array.get arr n;;

   let random_coord s pos = get_adjacent_coords pos |> List.filter
   (Hashtbl.mem s) |> Array.of_list |> randomelement

   let coord_to_step (posx, posy) (x, y) = if posx -. 1.0 = Step ()

   let get_step e = let open Queue in let p_queue = e.priority_queue in
   if not (is_empty p_queue) then peek p_queue else match e.path with |
   Path loop -> Loopq.peek loop | Area a -> push (random_coord a e.pos)
   p_queue; peek p_queue | MNone -> Pause 1

   (** [get_step_sprite e s] is the sprite representing the step [s] of
   entity [e]. Raises [Not_found] if the sprite doesn't exist *) let
   get_step_sprite e s = let index = int_of_float (e.interval_frac /.
   increment) in Hashtbl.find e.animations s |> Array.get index *)

let get_sprite e = e.sprite
(* try step_to_sprite (get_step e) e.orie |> get_step_sprite e with
   Not_found -> ( try get_step_sprite e (STurn e.orie) with Not_found ->
   get_step_sprite e (STurn S)) *)
(* Each entity must at least have a forward (south) facing sprite *)

let get_dialogue n = n.dialogue

let give_item e item p =
  (if item.given = false then
   let item = Item.get_item item.name in
   let inventory = p |> Player.inventory in
   Inventory.add_item inventory item);

  item.given <-
    true; if item.disappear then e.state <- 1

let heal c =
  let max_hp = (Creature.get_stats c).max_hp in
  let status = Creature.get_status c in
  Creature.set_current_hp c max_hp;
  if status <> Healthy then Creature.remove_status c status

let heal_party p () =
  let party = p |> Player.party in
  List.iter heal party

let interact e p redraw =
  if String.length e.dialogue > 0 then
  Animation.display_text_box e.dialogue false redraw ();
  match e.e_type with
  | Sign -> ()
  | Item a -> 
    if e.state = 0 then give_item e a p
  | Heal -> heal_party p ()
  | _ -> ()

let has_changed e = e.state <> 0

(* let set_sight e l = 
  match e.e_type with
  | Trainer i -> i.sight <- l
  | _ -> () *)
  
(* let update e p redraw = failwith "Unimplemented" *)

(* let update = failwith "Unimplemented" *)

(* let pop e = let open Queue in if not (is_empty e.priority_queue) then
   ignore (pop e.priority_queue) else match e.path with | MNone -> () |
   Area _ -> () | Path p -> ignore (Loopq.pop p)

   let move_coord o (x, y) = match o with | N -> (x, y +. increment) | S
   -> (x, y -. increment) | E -> (x +. increment, y) | W -> (x -.
   increment, y)

   let update_coord e = match get_step e with | Pause _ -> () | Turn _
   -> () | Step (o, _) -> e.pos <- move_coord o e.pos

   let incr_int e = e.interval_frac <- e.interval_frac +. increment let
   reset_int e = e.interval_frac <- 0.0

   let update e = match get_step e with | Step (_, i) | Pause i -> if
   e.interval_frac > float_of_int i then ( pop e; reset_int e) else
   update_coord e | Turn _ -> if e.interval_frac > 1.0 then pop e;
   incr_int e

   let correct_step d n = let opposite_dir = function | N -> S | E -> W
   | S -> N | W -> E in if n < 0 then Step (opposite_dir d, ~-n) else if
   n = 0 then Turn d else Step (d, n)

   let go e d n = e.priority_queue |> Queue.push (correct_step d n) let
   turn e d = e.priority_queue |> Queue.push (Turn d) let wait e t =
   e.priority_queue |> Queue.push (Pause t) let is_static entity =
   entity.path = MNone *)
