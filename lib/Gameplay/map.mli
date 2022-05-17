exception Out_of_Bounds
(** Raised when accessing a coordinate on the map that is not in range *)

exception Malformed_Json of string
(** Raised when the inputted Yojson.Basic.t is not valid *)

type t
(** abstract data type representing a game map *)

type coord = int * int
(** type representing the coordinate of a tile on a map. (0, 0) is the
    bottom left corner of the map *)

type encounters
(** type representing the possible encounters on a grass tile *)

type tile_type =
  | Path
  | Grass of encounters
  | Obstacle  (** type representing the type of a tile on the map *)

val null_map : t
(** Null map value*)

val load_map : string -> t
(** [load_map file] is the map represented by [file]. Raises
    [Malformed_Json] if the json file is invalid *)

val get_dim : t -> int * int
(** [get_dim m] is a tuple [(width, height)] representing the number of
    tiles in the width and height of m *)

val get_width : t -> int
val get_height : t -> int
val get_entities : t -> (coord * Entity.entity) list

val get_type : t -> coord -> tile_type
(** [get_type m c] is the type ([Path, Grass, Obstacle]) of the tile at
    [c] in map [m]. Raises [Out_of_Bounds] if [c] is not a valid
    coordinate *)

val get_graphic_id : t -> coord -> int
(** [get_graphic_id m c] is the id of the graphic of the tile at [c] in
    map [m]. Raises [Out_of_Bounds] if [c] is not a valid coordinate *)

val get_sprite : t -> coord -> Draw.sprite
(** [get_graphic_id m c] is the sprite of the graphic of the tile at [c]
    in map [m]. Raises [Out_of_Bounds] if [c] is not a valid coordinate *)

val encounter_creature : encounters -> Creature.creature option
(** [encounter_creature e] is the random creature that is encountered
    based on the specification in [e]. Returns None if no creature is
    encountered *)

val get_name : t -> string
(** [get_name t] is the name of [t]*)

val graphics_matrix : t -> string array array
(** Debug utility*)

val string_of_encounters : encounters -> string

val load_maps : unit -> unit
(** Load maps into memory*)

val get_map : string -> t
(** [get_map s] is the map with name [s]*)

val get_map2 : string -> t
(** [get_map2 s] is the map with name [s] (without boilerplate)*)

val get_maps : unit -> (string, t) Hashtbl.t
(** [get_maps] returns the table of all loaded maps*)
