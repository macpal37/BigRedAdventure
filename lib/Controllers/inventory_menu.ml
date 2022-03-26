(* open Item *)
open Draw
open Inventory
open Item
open Graphics

let display_queue = ref []
let item_list_bg = load_sprite "item_bag_list" GUI_Folder 3 ()

let get_items_from_bag (bag : bag) =
  let queue_items = list_items bag in
  let rec get_items_rec num = function
    | [] -> []
    | h :: t -> if num = 0 then [] else h :: get_items_rec (num - 1) t
  in
  display_queue.contents <- get_items_rec 5 queue_items

let draw_list () =
  let sx, sy, dif = (405, 575, 356) in
  let rec draw_list_rec i = function
    | [] -> print_endline ("AMOUNT: " ^ string_of_int i)
    | h :: t ->
        let item, amount = h in
        Ui.add_first_gameplay
          (draw_string_colored sx
             (sy - (i * 40))
             2 36
             (Util.captilize_all_string item.name)
             white);
        let s = string_of_int (amount * 10 / (1 + i)) in
        Ui.add_first_gameplay
          (draw_string_colored
             (sx + dif - (16 * String.length s))
             (sy - (i * 40))
             2 36 (s ^ "x") white);
        draw_list_rec (i + 1) t
  in
  draw_list_rec 0 display_queue.contents

let draw_bag (_ : bag) item_type () =
  let w, h = Draw.get_dimension item_list_bg in

  Ui.add_first_background
    (Draw.draw_sprite item_list_bg (Draw.width - w) (Draw.height - h));
  Ui.add_first_foreground
    (draw_string_colored 428 644 3 60
       (string_of_item_type item_type)
       (rgb 245 190 50));

  draw_list ()

let open_inventory player () =
  (* let player = Player.new_player "Marx" in *)
  let inventory = Player.inventory player in
  let bag = get_bag inventory Medicine in
  add bag (create_item "potion");
  add bag (create_item "potion");
  add bag (create_item "potion");
  add bag (create_item "potion");
  add bag (create_item "super potion");
  get_items_from_bag bag;
  draw_bag bag Medicine ()

let run_tick () = Ui.update_all ()
