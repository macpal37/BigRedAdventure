open OUnit2
open CreatureGame

let inventory_list_items_test
    (name : string)
    (b : Inventory.bag Lazy.t)
    (expected_output : (Item.item * int) list) : test =
  name >:: fun _ ->
  let b = Lazy.force b in
  assert_equal expected_output (Inventory.list_items b)

let inventory_consume_insufficient_test
    (name : string)
    (b : Inventory.bag Lazy.t)
    (c : int)
    (i : Item.item)
    (ex : exn) : test =
  name >:: fun _ ->
  let b = Lazy.force b in
  assert_raises ex (fun () -> Inventory.consume b ~count:c i)

let ball_inventory _ =
  let i = Inventory.new_inventory () in
  Inventory.add (Inventory.get_bag i Item.Ball) Itemtest.item_ball;
  Inventory.add
    (Inventory.get_bag i Item.Ball)
    ~count:2 Itemtest.item_great_ball;
  i

let medicine_inventory _ =
  let i = Inventory.new_inventory () in
  Inventory.add (Inventory.get_bag i Item.Medicine) Itemtest.item_potion;
  Inventory.add
    (Inventory.get_bag i Item.Medicine)
    ~count:2 Itemtest.item_super_potion;
  i

let tests =
  [
    inventory_list_items_test
      "items in bag of balls 1xball 2xgreat ball is 1xball 2xgreat ball"
      (lazy (Inventory.get_bag (ball_inventory ()) Item.Ball))
      [ (Itemtest.item_ball, 1); (Itemtest.item_great_ball, 2) ];
    inventory_consume_insufficient_test
      "consume ball from empty bag raises Insufficient 0"
      (lazy (Inventory.get_bag (Inventory.new_inventory ()) Item.Ball))
      1 Itemtest.item_ball (Inventory.Insufficient 0);
    inventory_list_items_test
      "items in bag of balls 1xball 2xgreat ball consume 1xball is \
       2xgreat ball"
      (lazy
        (let b = Inventory.get_bag (ball_inventory ()) Item.Ball in
         Inventory.consume b Itemtest.item_ball;
         b))
      [ (Itemtest.item_great_ball, 2) ];
    inventory_list_items_test
      "items in bag of balls 1xball 2xgreat ball consume 1xball \
       1xgreat ball is 1xgreat ball"
      (lazy
        (let b = Inventory.get_bag (ball_inventory ()) Item.Ball in
         Inventory.consume b Itemtest.item_ball;
         Inventory.consume b Itemtest.item_great_ball;
         b))
      [ (Itemtest.item_great_ball, 1) ];
    inventory_consume_insufficient_test
      "items in bag of balls 1xball 2xgreat ball consume 2xball raises \
       Insufficient 1"
      (lazy (Inventory.get_bag (ball_inventory ()) Item.Ball))
      2 Itemtest.item_ball (Inventory.Insufficient 1);
    inventory_list_items_test
      "items in bag of balls 1xball 2xgreat ball add another 1xball is \
       2xball 2xgreat ball"
      (lazy
        (let b = Inventory.get_bag (ball_inventory ()) Item.Ball in
         Inventory.add b Itemtest.item_ball;
         b))
      [ (Itemtest.item_ball, 2); (Itemtest.item_great_ball, 2) ];
    inventory_list_items_test
      "items in bag of balls 1xball 2xgreat ball add_item another \
       1xball is 2xball 2xgreat ball"
      (lazy
        (let i = ball_inventory () in
         Inventory.add_item i Itemtest.item_ball;
         Inventory.get_bag i Item.Ball))
      [ (Itemtest.item_ball, 2); (Itemtest.item_great_ball, 2) ];
    inventory_list_items_test
      "items in bag of medicine 1xpotion 2xsuper potion"
      (lazy (Inventory.get_bag (medicine_inventory ()) Item.Medicine))
      [ (Itemtest.item_potion, 1); (Itemtest.item_super_potion, 2) ];
    inventory_list_items_test
      "items in bag of medicine 1xpotion 2xsuper potion consume \
       1xpotion is 2xsuper potion"
      (lazy
        (let b =
           Inventory.get_bag (medicine_inventory ()) Item.Medicine
         in
         Inventory.consume b Itemtest.item_potion;
         b))
      [ (Itemtest.item_super_potion, 2) ];
    inventory_list_items_test
      "items in bag of medicine 1xpotion 2xsuper potion consume potion \
       1xsuper potion is 1x super potion"
      (lazy
        (let b =
           Inventory.get_bag (medicine_inventory ()) Item.Medicine
         in
         Inventory.consume b Itemtest.item_potion;
         Inventory.consume b Itemtest.item_super_potion;
         b))
      [ (Itemtest.item_super_potion, 1) ];
    inventory_consume_insufficient_test
      "items in bag of medicine 1xpotion 2xsuper potion consume \
       2xpotion raises Insufficient 1"
      (lazy (Inventory.get_bag (medicine_inventory ()) Item.Medicine))
      2 Itemtest.item_potion (Inventory.Insufficient 1);
    inventory_list_items_test
      "items in bag of medicine 1xpotion 2xsuper potion add another \
       1xpotion is 2x potion 2x super potion"
      (lazy
        (let b =
           Inventory.get_bag (medicine_inventory ()) Item.Medicine
         in
         Inventory.add b Itemtest.item_potion;
         b))
      [ (Itemtest.item_potion, 2); (Itemtest.item_super_potion, 2) ];
    inventory_list_items_test
      "items in bag of medicine 1xpotion 2xsuper potion add_item \
       another 1xpotion is 2xpotion 2xsuper potion"
      (lazy
        (let i = medicine_inventory () in
         Inventory.add_item i Itemtest.item_potion;
         Inventory.get_bag i Item.Medicine))
      [ (Itemtest.item_potion, 2); (Itemtest.item_super_potion, 2) ];
  ]
