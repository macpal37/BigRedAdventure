val selected_item : Item.item option ref
val display_queue : (Item.item * int) list ref
val get_items_from_bag : Inventory.bag -> unit
val init : unit -> unit
val run_tick : unit -> unit
