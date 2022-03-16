exception Out_of_Bounds
(** Raised when accessing a coordinate on the map that is not in range *)

exception Malformed_Json of string
(** Raised when the inputted Yojson.Basic.t is not valid *)

type t
(** abstract data type representing a game map *)

type coord = int * int
(** type representing the coordinate of a tile on a map *)

type encounters
(** type representing the possible encounters on a grass tile *)

type tile_type =
  | Path
  | Grass of encounters
  | Obstacle 
(** type representing the type of a tile on the map *)

val init_map : Yojson.Basic.t -> t
(** [init_map j] is the map represented by j. Raises [Malformed_Json] if the json file is invalid *)

val get_dim : t -> int * int
(** [get_dim m] is a tuple [(width, height)] representing the number of
    tiles in the width and height of m *)

val get_type : t -> coord -> tile_type
(** [get_type m c] is the type ([Path, Grass, Obstacle]) of the tile at
    [c] in map [m]. Raises [Out_of_Bounds] if [c] is not a valid coordinate *)

val get_graphic_id : t -> coord -> string
(** [get_graphic_id m c] is the id of the graphic of the tile at [c] in
    map [m]. Raises [Out_of_Bounds] if [c] is not a valid coordinate *)

val encounter_creature : encounters -> Creature.creature option
(** [encounter_creature e] is the random creature that is encountered
    based on the specification in [e]. Returns None if no creature is present *)