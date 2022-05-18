(** Module for the menu that is displayed during events such as
    capturing a creature or evolving one.*)

val init : unit -> unit
(** [init ()] initaliazes the menu for the inventory. *)

val get_item_selected : unit -> Item.item Util.pointer
(** [get_selected_item ()] returns the selected item from invetnroy to
    be used elsewhere.*)

val load_assets : unit -> unit
(** [load_assets ()] loads all the assets for the menu. *)
