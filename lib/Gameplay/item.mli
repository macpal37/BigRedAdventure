(** Representation of an inventory item

    This module represents inventory items and tracks what class of item
    they are*)

type item_type =
  | Key
  | Ball
  | Medicine
  | Misc

val num_item_types : int
(** Number of item types there are.*)

type item
(** The abstract type that represents an item*)

(** Variants representing the different classes of items*)

val string_of_item_type : item_type -> string
(** [string_of_item_type item_type] returns the string representation of
    the item_type*)

val create_item : string -> Yojson.Basic.t -> item

val new_item : string -> item_type -> int -> string -> int -> item
(** [new_item s t i] is an item with name [s], class [t], id [i]*)

val get_name : item -> string
(** [get_name i] is the name of item [i]*)

val get_type : item -> item_type
(** [get_type i] is the item class of [i]*)

val get_id : item -> int
(** [get_id i] is the item id of [i]*)

val get_description : item -> string
(** [get_description i] is the description of the item [i]*)

val get_cost : item -> int
(** [get_cost i] is the monetary cost of item [i]*)
