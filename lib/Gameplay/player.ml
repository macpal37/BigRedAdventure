type orientations =
  | N
  | E
  | S
  | W

type player = {
  name : string;
  mutable money : int;
  inventory : Inventory.inventory;
  mutable time_played : int;
  mutable badges : string list;
  mutable party : Creature.creature list;
  mutable creatures : Creature.creature list;
  mutable x : int;
  mutable y : int;
  mutable orie : orientations;
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
    x = 0;
    y = 0;
    orie = N;
  }

let name p = p.name
let money p = p.money
let inventory p = p.inventory
let time_played p = p.time_played
let badges p = p.badges
let party p = p.party
let party_i p i = List.nth p.party i
let set_party party player = player.party <- party
let creatures player = player.creatures

let add_creature c p =
  if List.length p.party < 6 then p.party <- p.party @ [ c ];
  p.creatures <- c :: p.creatures

let rec remove_creature_rec c l =
  match l with
  | [] -> raise (Failure "No such creature")
  | h :: t -> if h = c then t else h :: remove_creature_rec c t

let remove_creature c p =
  p.creatures <- remove_creature_rec c p.creatures;
  try p.party <- remove_creature_rec c p.party with _ -> ()

let has_badge b p = List.mem b p.badges
let add_money i p = p.money <- p.money + i
let add_time_played i p = p.time_played <- p.time_played + i
let add_badge b p = p.badges <- b :: p.badges
let x p = p.x
let y p = p.y
let orie p = p.orie
let set_x x p = p.x <- x
let set_y y p = p.y <- y

let set_coord x y p =
  set_x x p;
  set_y y p

let set_orie o p = p.orie <- o
let get_orie p = p.orie
