(* open Item *)
open Draw
open Inventory
open Item
open Graphics

let display_queue = ref []
let max_list_size = 10
let max_items = ref 0
let item_list_bg = load_sprite "item_bag_list" GUI_Folder 3 ()

let get_items_from_bag (bag : bag) =
  let queue_items = list_items bag in
  let rec get_items_rec num = function
    | [] -> []
    | h :: t -> if num = 0 then [] else h :: get_items_rec (num - 1) t
  in
  display_queue.contents <- get_items_rec max_list_size queue_items

let draw_list () =
  let sx, sy, dif = (405, 575, 356) in
  let rec draw_list_rec i = function
    | [] -> i
    | h :: t ->
        let item, amount = h in
        Ui.add_first_gameplay
          (draw_string_colored sx
             (sy - (i * 40))
             2 36
             (Util.captilize_all_string (get_name item))
             white);
        let s = string_of_int amount in
        Ui.add_first_gameplay
          (draw_string_colored
             (sx + dif - (16 * String.length s))
             (sy - (i * 40))
             2 36 (s ^ "x") white);
        draw_list_rec (i + 1) t
  in
  let index = draw_list_rec 0 display_queue.contents in
  if index < max_list_size - 1 then
    Ui.add_first_gameplay
      (draw_string_colored sx
         (sy - (index * 40))
         2 36 "     - - - - - - -     " white)
  else ()

let inventory_position = Util.new_point ()

let draw_bag item_type x y () =
  let w, h = Draw.get_dimension item_list_bg in

  Ui.add_first_background
    (Draw.draw_sprite item_list_bg (Draw.width - w) (Draw.height - h));
  Ui.add_first_foreground
    (draw_string_colored 428 644 3 60
       (string_of_item_type item_type)
       (rgb 245 190 50));

  draw_list ();
  Ui.add_first_foreground (fun () ->
      set_color red;
      set_line_width 6;
      draw_rect x (y - (40 * inventory_position.y)) 370 40)

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
  max_items.contents <- List.length (list_items bag);
  get_items_from_bag bag;
  if List.length display_queue.contents > 0 then
    let item, _ =
      List.nth display_queue.contents inventory_position.y
    in
    Ui.add_first_foreground (draw_text_string (get_description item))
  else Ui.add_first_foreground clear_text;

  draw_bag bag_type 415 575 ()

let inventory_text_bg = load_sprite "inventory_text_bg" GUI_Folder 3 ()

let rec run_tick () =
  Input.poll ();
  let key =
    match Input.key_option () with
    | Some c -> c
    | None -> '#'
  in
  let x, y = (inventory_position.x, inventory_position.y) in
  if key = 's' && y >= 0 && y < max_items.contents - 1 then
    inventory_position.y <- y + 1;
  if key = 'w' && y < max_items.contents && y > 0 then
    inventory_position.y <- y - 1;
  if key = 'd' then inventory_position.x <- x + 1;
  if key = 'a' then inventory_position.x <- x - 1;

  if inventory_position.x >= num_item_types then
    inventory_position.x <- 0
  else if inventory_position.x < 0 then
    inventory_position.x <- num_item_types - 1
  else ();

  if key = 'd' || key = 'a' then begin
    inventory_position.y <- 0;
    refresh ()
  end;

  if key = 'w' || key = 's' then begin
    print_endline ("Y: " ^ string_of_int inventory_position.y);
    refresh ()
  end;
  Ui.update_all ();
  print_endline (String.make 1 key);
  Unix.sleepf 0.016;
  if key <> 'q' then run_tick () else Draw.set_synced_mode true

let init () =
  Draw.set_synced_mode false;

  set_text_bg inventory_text_bg empty_sprite;
  refresh ();
  Ui.add_first_background clear_screen;
  run_tick ()
