open OUnit2
open CreatureGame

let inventory_list_items_test
    (name : string)
    (b : Inventory.bag)
    (expected_output : (Item.item * int) list) : test =
  name >:: fun _ ->
  assert_equal expected_output (Inventory.list_items b)

let inventory_consume_insufficient_test
    (name : string)
    (b : Inventory.bag)
    (c : int)
    (i : Item.item)
    (ex : exn) : test =
  name >:: fun _ ->
  assert_raises ex (fun () -> Inventory.consume b ~count:c i)

let ball_inventory _ =
  let i = Inventory.new_inventory () in
  Inventory.add (Inventory.get_bag i Item.Ball) Itemtest.item_ball;
  Inventory.add
    (Inventory.get_bag i Item.Ball)
    ~count:2 Itemtest.item_great_ball;
  i

let tests =
  [
    inventory_list_items_test
      "items in bag of balls 1xball 2xgreat ball"
      (Inventory.get_bag (ball_inventory ()) Item.Ball)
      [ (Itemtest.item_ball, 1); (Itemtest.item_great_ball, 2) ];
    inventory_consume_insufficient_test
      "consume ball from empty bag raises Insufficient 0"
      (Inventory.get_bag (Inventory.new_inventory ()) Item.Ball)
      1 Itemtest.item_ball (Inventory.Insufficient 0);
    inventory_list_items_test
      "items in bag of balls 1xball 2xgreat ball consume 1xball"
      (let b = Inventory.get_bag (ball_inventory ()) Item.Ball in
       Inventory.consume b Itemtest.item_ball;
       b)
      [ (Itemtest.item_great_ball, 2) ];
    inventory_list_items_test
      "items in bag of balls 1xball 2xgreat ball consume 1xball \
       1xgreat ball"
      (let b = Inventory.get_bag (ball_inventory ()) Item.Ball in
       Inventory.consume b Itemtest.item_ball;
       Inventory.consume b Itemtest.item_great_ball;
       b)
      [ (Itemtest.item_great_ball, 1) ];
    inventory_consume_insufficient_test
      "items in bag of balls 1xball 2xgreat ball consume 2xball raises \
       Insufficient 1"
      (Inventory.get_bag (ball_inventory ()) Item.Ball)
      2 Itemtest.item_ball (Inventory.Insufficient 1);
    inventory_list_items_test
      "items in bag of balls 1xball 2xgreat ball add another 1xball"
      (let b = Inventory.get_bag (ball_inventory ()) Item.Ball in
       Inventory.add b Itemtest.item_ball;
       b)
      [ (Itemtest.item_ball, 2); (Itemtest.item_great_ball, 2) ];
  ]
