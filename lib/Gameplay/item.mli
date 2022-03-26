(** Representation of an inventory item

    This module represents inventory items and tracks what class of item
    they are*)

type item_type =
  | Key
  | Ball
  | Medicine
  | Misc

type item = {
  name : string;
  item_type : item_type;
  description : string;
  cost : int;
  id : int;
}
(** The abstract type that represents an item*)

(** Variants representing the different classes of items*)

val string_of_item_type : item_type -> string
(** [string_of_item_type item_type] returns the string representation of
    the item_type*)

val create_item : string -> item

val new_item : string -> item_type -> int -> string -> int -> item
(** [new_item s t i] is an item with name [s], class [t], id [i]*)

val name : item -> string
(** [name i] is the name of item [i]*)

val classification : item -> item_type
(** [classification i] is the item class of [i]*)

val id : item -> int
(** [id i] is the item id of [i]*)
