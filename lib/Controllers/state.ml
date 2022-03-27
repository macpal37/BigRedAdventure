open Item
open Inventory

type state = { player : Player.player }

let current_state = ref { player = Player.new_player "Red" }
let get_state _ = !current_state
let player _ = !current_state.player

let adhoc_init () =
  let rafu = Creature.create_creature "rafu" 30 in
  let rafu2 = Creature.create_creature "rafu" 30 in
  let rafu3 = Creature.create_creature "rafu" 30 in
  let rafu4 = Creature.create_creature "rafu" 30 in
  Player.add_creature rafu current_state.contents.player;
  Player.add_creature rafu2 current_state.contents.player;
  Player.add_creature rafu3 current_state.contents.player;
  Player.add_creature rafu4 current_state.contents.player;
  Creature.set_nickname rafu "Llama Baby";
  Creature.set_nickname rafu2 "Ron";
  Creature.set_nickname rafu3 "Lucky";
  Creature.set_nickname rafu4 "Chubby Bunny";
  let inventory = Player.inventory current_state.contents.player in

  add_item inventory (create_item "repel");
  add_item inventory (create_item "super repel");
  add_item inventory (create_item "max repel");

  add_item inventory (create_item "potion");
  add_item inventory (create_item "potion");
  add_item inventory (create_item "potion");
  add_item inventory (create_item "potion");
  add_item inventory (create_item "super potion");
  add_item inventory (create_item "hyper potion");
  add_item inventory (create_item "max potion");
  add_item inventory (create_item "ether");
  add_item inventory (create_item "max ether");
  add_item inventory (create_item "revive");
  add_item inventory (create_item "max revive")
