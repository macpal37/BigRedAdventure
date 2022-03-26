open OUnit2
open CreatureGame

let item_name_test
    (name : string)
    (i : Item.item)
    (expected_output : string) : test =
  name >:: fun _ -> assert_equal expected_output (Item.get_name i)

let item_classification_test
    (name : string)
    (i : Item.item)
    (expected_output : Item.item_type) : test =
  name >:: fun _ -> assert_equal expected_output (Item.get_type i)

let item_id_test (name : string) (i : Item.item) (expected_output : int)
    : test =
  name >:: fun _ -> assert_equal expected_output (Item.get_id i)

let item_yeet = Item.new_item "YEET" Item.Misc 69 "" 0
let item_weed = Item.new_item "WEED" Item.Medicine 420 "" 0
let item_potion = Item.new_item "Potion" Item.Medicine 0 "" 0
let item_bike = Item.new_item "Bike" Item.Key 1 "" 0
let item_ball = Item.new_item "Ball" Item.Ball 2 "" 0
let item_great_ball = Item.new_item "Great Ball" Item.Ball 3 "" 0

let tests =
  [
    item_name_test "name of item YEET is YEET" item_yeet "YEET";
    item_name_test "name of item Potion is Potion" item_potion "Potion";
    item_name_test "name of item Great Ball is Great Ball"
      item_great_ball "Great Ball";
    item_classification_test "type of item Potion is Medicine"
      item_potion Item.Medicine;
    item_classification_test "type of item Bike is Key" item_bike
      Item.Key;
    item_id_test "id of item YEET is 69" item_yeet 69;
    item_id_test "id of item WEED is 420" item_weed 420;
  ]
