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
  Player.set_x 6 (player ());
  Player.set_y 6 (player ());
  !current_state.map <- Map.get_map "dungeon.json";
  let clefairy = Creature.create_creature "clefairy" 20 in
  Player.add_creature clefairy !current_state.player;
  Creature.set_nickname clefairy "Ya Boi";
  let inventory = Player.inventory !current_state.player in

  for _ = 1 to 10 do
    add_item inventory (Item.get_item "poke ball")
  done;
  add_item inventory (Item.get_item "potion");
  add_item inventory (Item.get_item "potion");
  add_item inventory (Item.get_item "revive")
