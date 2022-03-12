type player = {
  name : string;
  mutable money : int;
  inventory : Inventory.inventory;
  mutable time_played : int;
  mutable badges : string list;
  mutable party : Creature.creature list;
  mutable creatures : Creature.creature list;
}

let new_player s =
  {
    name = s;
    money = 0;
    inventory = Inventory.new_inventory ();
    time_played = 0;
    badges = [];
    party = [];
    creatures = [];
  }

let name p = p.name
let money p = p.money
let inventory p = p.inventory
let time_played p = p.time_played
let badges p = p.badges
let party p = p.party
let set_party party player = player.party <- party
let creatures player = player.creatures
let add_creature c p = p.creatures <- c :: p.creatures

let rec remove_creature_rec c l =
  match l with
  | [] -> raise (Failure "No such creature")
  | h :: t -> if h = c then t else h :: remove_creature_rec c t

let remove_creature c p =
  p.creatures <- remove_creature_rec c p.creatures

let has_badge b p = List.mem b p.badges
let add_money i p = p.money <- p.money + i
let add_time_played i p = p.time_played <- p.time_played + i
let add_badge b p = p.badges <- b :: p.badges
