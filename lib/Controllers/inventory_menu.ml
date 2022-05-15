(* open Item *)
open Draw
open Inventory
open Item
open Util
open DrawText
open Input

let display_queue = ref []
let max_list_size = 10
let max_items = ref 0
let inventory_menu = Util.null ()
let selected_item = null ()

let load_assets _ =
  inventory_menu
  *= Sprite_assets.get_sprite2 "inventory_menu" GUI_Folder

type menu_mode =
  | Selecting
  | Minimenu

let minimenu_position = Util.new_point ()
let mode = ref Selecting

let get_items_from_bag (bag : bag) =
  let queue_items = list_items bag in
  let rec get_items_rec num = function
    | [] -> []
    | h :: t -> if num = 0 then [] else h :: get_items_rec (num - 1) t
  in
  display_queue := get_items_rec max_list_size queue_items

let draw_list () =
  let sx, sy, dif = (405, 563, 356) in
  let rec draw_list_rec i = function
    | [] -> i
    | h :: t ->
        let item, amount = h in
        Ui.add_first_gameplay
          (draw_string_colored sx
             (sy - (i * 40))
             Medium
             (Util.captilize_all_string (get_name item))
             white text_color);
        let s = string_of_int amount in
        Ui.add_first_gameplay
          (draw_string_colored
             (sx + dif - (16 * String.length s))
             (sy - (i * 40))
             Medium (s ^ "x") white text_color);
        draw_list_rec (i + 1) t
  in
  let index = draw_list_rec 0 !display_queue in
  if index < max_list_size - 1 then
    Ui.add_first_gameplay
      (draw_string_colored sx
         (sy - (index * 40))
         Medium "     - - - - - - -     " white text_color)
  else ()

let inventory_position = Util.new_point ()

let draw_bag item_type () =
  Ui.add_first_background (Draw.draw_sprite ~!inventory_menu 0 0);
  Ui.add_first_foreground
    (draw_string_colored 428 644 Huge
       (string_of_item_type item_type)
       (rgb 245 190 50) text_color);

  draw_list ();
  if !max_items > 0 then
    Ui.add_first_foreground (fun () ->
        set_color red;
        set_line_width 6;
        draw_rect 415 (563 - (40 * inventory_position.y)) 370 40)

let get_selected_item () =
  if List.length !display_queue = 0 then None
  else
    let item, _ = List.nth !display_queue inventory_position.y in
    Some item

let refresh () =
  let bag_type =
    match inventory_position.x with
    | 0 -> Misc
    | 1 -> Medicine
    | 2 -> Ball
    | 3 -> Key
    | _ -> Misc
  in

  let bag = get_bag (Player.inventory (State.player ())) bag_type in
  max_items := List.length (list_items bag);
  get_items_from_bag bag;
  draw_bag bag_type ();
  inventory_position.y <-
    Util.bound inventory_position.y 0 (!max_items - 1);

  if List.length !display_queue > 0 then
    let item, _ = List.nth !display_queue inventory_position.y in

    Ui.add_first_foreground
      (draw_text_string_pos 35 142 Medium 30 (get_description item))

let rec run_tick () =
  Input.sleep 0.016 ();
  let key =
    match Input.pop_key_option () with
    | Some c -> get_ctrl_key c
    | None -> NoKey
  in
  (match !mode with
  | Selecting ->
      let x, y = (inventory_position.x, inventory_position.y) in
      if key = Down && y >= 0 && y < !max_items - 1 then
        inventory_position.y <- y + 1;
      if key = Up && y < !max_items && y > 0 then
        inventory_position.y <- y - 1;
      if key = Right then inventory_position.x <- x + 1;
      if key = Left then inventory_position.x <- x - 1;

      if inventory_position.x >= num_item_types then
        inventory_position.x <- 0
      else if inventory_position.x < 0 then
        inventory_position.x <- num_item_types - 1
      else ();

      if key = Right || key = Left then inventory_position.y <- 0;
      if key = Action then begin
        minimenu_position.y <- 0;
        let item = get_selected_item () in

        (* (match item with | Some i -> consume_item (Player.inventory
           (State.player ())) i | None -> ()); *)
        selected_item := item
      end;
      if key = Back then mode := Minimenu
  | Minimenu ->
      if key = Action then begin
        minimenu_position.y <- 0;
        selected_item := get_selected_item ()
      end;
      if key = Back then begin
        minimenu_position.y <- 0;
        mode := Selecting
      end);
  refresh ();
  Ui.update_all ();
  if key <> Back && !selected_item = None then run_tick ()

let init () =
  mode := Selecting;
  selected_item := None;
  Ui.clear_all ();
  refresh ();
  run_tick ()
