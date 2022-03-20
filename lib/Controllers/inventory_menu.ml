(* open Item *)
open Draw
open Inventory
open Item

let display_queue = ref []
let item_list_bg = load_sprite "item_list_bg" GUI_Folder 3 ()

let get_items_from_bag (bag : bag) =
  let queue_items = list_items bag in
  let rec get_items_rec num = function
    | [] -> []
    | h :: t -> if num = 0 then [] else h :: get_items_rec (num - 1) t
  in
  display_queue.contents <- get_items_rec 5 queue_items

let draw_bag bag = get_items_from_bag bag

let open_inventory player () =
  (* let player = Player.new_player "Marx" in *)
  let inventory = Player.inventory player in
  let bag = get_bag inventory Medicine in
  add bag (create_item "potion");
  add bag (create_item "potion");
  add bag (create_item "potion");
  add bag (create_item "potion");
  add bag (create_item "super potion");
  get_items bag
