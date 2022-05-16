open Inventory

type state = {
  player : Player.player;
  mutable map : Map.t;
}

let current_state =
  ref { player = Player.new_player "Red"; map = Map.null_map }

let get_state _ = !current_state
let player _ = !current_state.player
let player_x _ = Player.x !current_state.player
let player_y _ = Player.y !current_state.player
let map _ = !current_state.map

let adhoc_init () =
  Player.set_x 4 (player ());
  Player.set_y 4 (player ());
  !current_state.map <- Map.get_map "test_map.json";
  (* let nuxel = Creature.create_creature "nuxel" 23 in *)
  let chumpi = Creature.create_creature "chumpi" 10 in
  let rafu = Creature.create_creature "rafu" 10 in
  Creature.set_current_hp chumpi 20.;
  (* Player.add_creature nuxel !current_state.player; *)
  Player.add_creature chumpi !current_state.player;
  Player.add_creature rafu !current_state.player;

  (* Creature.set_nickname nuxel "Sonic"; *)
  Creature.set_nickname chumpi "Lucky";
  Creature.set_nickname rafu "Ya Boi";

  let inventory = Player.inventory !current_state.player in

  add_item inventory (Item.get_item "repel");
  add_item inventory (Item.get_item "super repel");
  add_item inventory (Item.get_item "max repel");
  for _ = 1 to 20 do
    add_item inventory (Item.get_item "poke ball")
  done;

  for _ = 1 to 20 do
    add_item inventory (Item.get_item "great ball")
  done;
  for _ = 1 to 20 do
    add_item inventory (Item.get_item "ultra ball")
  done;
  add_item inventory (Item.get_item "potion");
  add_item inventory (Item.get_item "potion");
  add_item inventory (Item.get_item "potion");
  add_item inventory (Item.get_item "potion");
  add_item inventory (Item.get_item "super potion");
  add_item inventory (Item.get_item "hyper potion");
  add_item inventory (Item.get_item "max potion");
  add_item inventory (Item.get_item "ether");
  add_item inventory (Item.get_item "max ether");
  add_item inventory (Item.get_item "revive");
  add_item inventory (Item.get_item "max revive")
