open Item
open Inventory

type state = {
  player : Player.player;
  map : Map.t;
}

let current_state =
  ref
    { player = Player.new_player "Red"; map = Map.load_map "test_map" }

let get_state _ = !current_state
let player _ = !current_state.player
let player_x _ = Player.x !current_state.player
let player_y _ = Player.y !current_state.player
let map _ = !current_state.map

let adhoc_init () =
  Player.set_x 4 (player ());
  Player.set_y 4 (player ());
  let rafu = Creature.create_creature "chumpi" 20 in
  let clefairy = Creature.create_creature "rafu" 40 in

  Player.add_creature rafu current_state.contents.player;
  Player.add_creature clefairy current_state.contents.player;

  Creature.set_nickname rafu "Llama Baby";
  Creature.set_nickname rafu "Lucky";
  (* Creature.set_nickname rafu2 "Ron"; Creature.set_nickname rafu3
     "Lucky"; Creature.set_nickname rafu4 "Chubby Bunny"; *)
  let inventory = Player.inventory current_state.contents.player in

  add_item inventory (create_item "repel");
  add_item inventory (create_item "super repel");
  add_item inventory (create_item "max repel");
  for _ = 1 to 20 do
    add_item inventory (create_item "pokeball")
  done;

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
