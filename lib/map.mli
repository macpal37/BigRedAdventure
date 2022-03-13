type t
(** The abstract data type representing a game map *)

type coord = int * int
(** The type representing the coordinate of a tile on the map *)

type encounters
(** type representing the possible encounters on a grass tile *)

type tile_type =
  | Path
  | Grass of encounters
  | Obstacle  (** type representing the type of a tile on the map *)

val init_map : Yojson.Basic.t -> t
(** [init_map j] is the map represented by j *)

val get_dim : t -> int * int
(** [get_dim m] is a tuple [(width, height)] representing the number of
    tiles in the length and height of m *)

val get_type : coord -> tile_type
(** [get_type] is the type ([Path, Grass, Obstacle]) of the tile at [c] *)

val get_graphic_id : coord -> string
(** [get_graphic_id] is the id of the graphic of the tile at [c] *)

val encounter_creature : encounters -> Creature.creature option
(** [encounter_creature] is the random creature that is encountered
    based on the specification in [e] *)

(**val get_events: val set_event: t -> string -> unit val get_event_id:
   t -> int * int -> string option *)
