open OUnit2
open CreatureGame

let player_name_test
    (name : string)
    (p : Player.player)
    (expected_output : string) : test =
  name >:: fun _ -> assert_equal expected_output (Player.name p)

let player_money_test
    (name : string)
    (p : Player.player)
    (expected_output : int) : test =
  name >:: fun _ -> assert_equal expected_output (Player.money p)

let player_inventory_test
    (name : string)
    (p : Player.player)
    (expected_output : Inventory.inventory) : test =
  name >:: fun _ -> assert_equal expected_output (Player.inventory p)

let player_time_played_test
    (name : string)
    (p : Player.player)
    (expected_output : int) : test =
  name >:: fun _ -> assert_equal expected_output (Player.time_played p)

let player_badges_test
    (name : string)
    (p : Player.player)
    (expected_output : string list) : test =
  name >:: fun _ ->
  assert_equal ~cmp:Testutil.cmp_set_like_lists expected_output
    (Player.badges p)

let player_has_badge_test
    (name : string)
    (p : Player.player)
    (b : string)
    (expected_output : bool) : test =
  name >:: fun _ -> assert_equal expected_output (Player.has_badge b p)

let player_party_test
    (name : string)
    (p : Player.player)
    (expected_output : Creature.creature list) : test =
  name >:: fun _ -> assert_equal expected_output (Player.party p)

let player_creatures_test
    (name : string)
    (p : Player.player)
    (expected_output : Creature.creature list) : test =
  name >:: fun _ ->
  assert_equal ~cmp:Testutil.cmp_set_like_lists expected_output
    (Player.creatures p)

let player_remove_creature_failure_test
    (name : string)
    (p : Player.player)
    (c : Creature.creature)
    (ex : exn) : test =
  name >:: fun _ ->
  assert_raises ex (fun () -> Player.remove_creature c p)

let player_x_test
    (name : string)
    (p : Player.player)
    (expected_output : int) : test =
  name >:: fun _ -> assert_equal expected_output (Player.x p)

let player_y_test
    (name : string)
    (p : Player.player)
    (expected_output : int) : test =
  name >:: fun _ -> assert_equal expected_output (Player.y p)

let player_orie_test
    (name : string)
    (p : Player.player)
    (expected_output : Player.orientations) : test =
  name >:: fun _ -> assert_equal expected_output (Player.orie p)

let creature_clefairy = Creature.create_creature "clefairy" 60
let creature_clefa = Creature.create_creature "clefa" 50
let badge_boulder = "Boulder"
let badge_cascade = "Cascade"

let tests =
  [
    player_name_test "New player name Red"
      (Player.new_player "Red")
      "Red";
    player_money_test "New player money 0" (Player.new_player "Red") 0;
    player_inventory_test "New player empty inventory"
      (Player.new_player "Red")
      (Inventory.new_inventory ());
    player_time_played_test "New player time played 0"
      (Player.new_player "Red")
      0;
    player_badges_test "New player no badges"
      (Player.new_player "Red")
      [];
    player_has_badge_test "New player does not have Boulder badge"
      (Player.new_player "Red")
      badge_boulder false;
    player_party_test "New player empty party"
      (Player.new_player "Red")
      [];
    player_creatures_test "New player no creatures"
      (Player.new_player "Red")
      [];
    player_remove_creature_failure_test
      "New player remove Clefairy raises Failure"
      (Player.new_player "Red")
      creature_clefairy (Failure "No such creature");
    player_money_test "New player add money 100"
      (let p = Player.new_player "Red" in
       Player.add_money 100 p;
       p)
      100;
    player_money_test "New player add money 100 x2"
      (let p = Player.new_player "Red" in
       Player.add_money 100 p;
       Player.add_money 100 p;
       p)
      200;
    player_inventory_test "New player added 1xball 2xgreat ball"
      (let p = Player.new_player "Red" in
       let i = Player.inventory p in
       let b = Inventory.get_bag i Item.Ball in
       Inventory.add b Itemtest.item_ball;
       Inventory.add b ~count:2 Itemtest.item_great_ball;
       p)
      (Inventorytest.ball_inventory ());
    player_time_played_test "New player add time played 100"
      (let p = Player.new_player "Red" in
       Player.add_time_played 100 p;
       p)
      100;
    player_time_played_test "New player add time played 100 200"
      (let p = Player.new_player "Red" in
       Player.add_time_played 100 p;
       Player.add_time_played 200 p;
       p)
      300;
    player_badges_test "New player added Boulder badge"
      (let p = Player.new_player "Red" in
       Player.add_badge badge_boulder p;
       p)
      [ badge_boulder ];
    player_badges_test "New player added Boulder badge Cascade badge"
      (let p = Player.new_player "Red" in
       Player.add_badge badge_boulder p;
       Player.add_badge badge_cascade p;
       p)
      [ badge_boulder; badge_cascade ];
    player_has_badge_test
      "New player added Boulder badge has Boulder badge"
      (let p = Player.new_player "Red" in
       Player.add_badge badge_boulder p;
       p)
      badge_boulder true;
    player_has_badge_test
      "New player added Boulder badge Cascade badge has Boulder badge"
      (let p = Player.new_player "Red" in
       Player.add_badge badge_boulder p;
       Player.add_badge badge_cascade p;
       p)
      badge_boulder true;
    player_party_test "New player set party to Clefairy"
      (let p = Player.new_player "Red" in
       Player.set_party [ creature_clefairy ] p;
       p)
      [ creature_clefairy ];
    player_creatures_test "New player add Clefairy add Clefa"
      (let p = Player.new_player "Red" in
       Player.add_creature creature_clefairy p;
       Player.add_creature creature_clefa p;
       p)
      [ creature_clefairy; creature_clefa ];
    player_creatures_test
      "New player add Clefairy add Clefa remove Clefairy"
      (let p = Player.new_player "Red" in
       Player.add_creature creature_clefairy p;
       Player.add_creature creature_clefa p;
       Player.remove_creature creature_clefairy p;
       p)
      [ creature_clefa ];
    player_x_test "New player x pos 0" (Player.new_player "Red") 0;
    player_y_test "New player y pos 0" (Player.new_player "Red") 0;
    player_orie_test "New player orie N"
      (Player.new_player "Red")
      Player.N;
    player_x_test "New player set x pos 100"
      (let p = Player.new_player "Red" in
       Player.set_x 100 p;
       p)
      100;
    player_y_test "New player set y pos 100"
      (let p = Player.new_player "Red" in
       Player.set_y 100 p;
       p)
      100;
    player_orie_test "New player set orie E"
      (let p = Player.new_player "Red" in
       Player.set_orie Player.E p;
       p)
      Player.E;
  ]
