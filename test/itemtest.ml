open OUnit2
open CreatureGame

let item_name_test
    (name : string)
    (i : Item.item)
    (expected_output : string) : test =
  name >:: fun _ -> assert_equal expected_output (Item.name i)

let tests =
  [
    item_name_test "name of item YEET"
      (Item.new_item "YEET" Item.Key 69)
      "YEET";
  ]
