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
let set_player p = current_state := { !current_state with player = p }
let set_map m = !current_state.map <- m

let new_game () =
  (* Player.set_x 4 (player ()); Player.set_y 4 (player ()); *)
  Player.set_x 6 (player ());
  Player.set_y 6 (player ());
  !current_state.map <- Map.get_map "starting_scene.json";
  let clefairy = Creature.create_creature "clefairy" 5 in
  (* let chumpi = Creature.create_creature "chumpi" 10 in *)
  (* let rafu = Creature.create_creature "rafu" 10 in *)
  (* Creature.set_current_hp chumpi 20.; *)
  (* Player.add_creature nuxel !current_state.player; *)
  (* Player.add_creature chumpi !current_state.player; *)
  Player.add_creature clefairy !current_state.player;

  (* Creature.set_nickname nuxel "Sonic"; *)
  (* Creature.set_nickname chumpi "Lucky"; *)
  Creature.set_nickname clefairy "Ya Boi";

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
