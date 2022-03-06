open Yojson.Basic.Util

type stats = {
  mutable max_hp : int;
  mutable attack : int;
  mutable defense : int;
  mutable sp_attack : int;
  mutable sp_defense : int;
  mutable speed : int;
}

type stat =
  | HP
  | Attack
  | Defense
  | Sp_Attack
  | Sp_Defense
  | Speed

type learnset_moves = {
  move : string;
  level : int;
}

type status = Healthy

type etype =
  | Normal
  | Fire
  | Water
  | Grass
  | Fairy
  | None

type leveling_rate =
  | Fast
  | MediumFast
  | MediumSlow
  | Slow

type nature = {
  name : string;
  buff : stat;
  nerf : stat;
  id : int;
}

type creature = {
  name : string;
  mutable level : int;
  mutable current_hp : int;
  mutable exp : int;
  base_stats : stats;
  mutable current_stats : stats;
  iv_stats : stats;
  mutable ev_stats : stats;
  mutable current_status : status;
  etypes : etype * etype;
  nature : nature;
  leveling_rate : leveling_rate;
  ev_gain : string * int;
  poke_id : int;
  catch_rate : int;
  base_exp : int;
  mutable friendship : int;
  learnset : learnset_moves list;
  mutable moves : string list;
}

let get_moves creature = creature.moves

let stats_of_json json =
  {
    max_hp = json |> member "hp" |> to_int;
    attack = json |> member "attack" |> to_int;
    defense = json |> member "defense" |> to_int;
    sp_attack = json |> member "sp_attack" |> to_int;
    sp_defense = json |> member "sp_defense" |> to_int;
    speed = json |> member "speed" |> to_int;
  }

let stat_to_string stat_var =
  match stat_var with
  | HP -> "HP"
  | Attack -> "Attack"
  | Defense -> "Defense"
  | Sp_Attack -> "Sp_Attack"
  | Sp_Defense -> "Sp_Defense"
  | Speed -> "Speed"

(* let string_to_stat stat_string = match stat_string with | "HP" -> HP
   | "Attack" -> Attack | "Defense" -> Defense | "Sp_Attack" ->
   Sp_Attack | "Sp_Defense" -> Sp_Defense | "Speed" -> Speed | _ ->
   HP *)

let etype_to_string etype_var =
  match etype_var with
  | Normal -> "Normal"
  | Fire -> "Fire"
  | Water -> "Water"
  | Grass -> "Grass"
  | Fairy -> "Fairy"
  | _ -> "None"

let string_to_etype etype_string =
  match etype_string with
  | "Normal" -> Normal
  | "Fire" -> Fire
  | "Water" -> Water
  | "Grass" -> Grass
  | "Fairy" -> Fairy
  | _ -> None

let etype_from_json json num =
  json
  |> member ("type" ^ string_of_int num)
  |> to_string |> string_to_etype

let level_rate_from_json json =
  let rate_strint = json |> member "level_rate" |> to_string in
  match rate_strint with
  | "Fast" -> Fast
  | "MediumFast" -> MediumFast
  | "MediumSlow" -> MediumSlow
  | "Slow" -> Slow
  | _ -> Fast

let rand max () = Random.int max

let mod_stat stats stat_name pow =
  let d a b c =
    let x = (a * b) + 50 in
    x / c
  in
  let pow = int_of_float (100. *. pow) in
  match stat_name with
  | Attack -> d stats.attack pow 100
  | Defense -> d stats.defense pow 100
  | Sp_Attack -> d stats.sp_attack pow 100
  | Sp_Defense -> d stats.sp_defense pow 100
  | Speed -> d stats.speed pow 100
  | _ -> -1

let mod_statc stats stat_name pow =
  let d a b c =
    let x = (a * b) + 50 in
    x / c
  in
  let pow = int_of_float (100. *. pow) in
  match stat_name with
  | Attack -> stats.attack <- d stats.attack pow 100
  | Defense -> stats.defense <- d stats.defense pow 100
  | Sp_Attack -> stats.sp_attack <- d stats.sp_attack pow 100
  | Sp_Defense -> stats.sp_defense <- d stats.sp_defense pow 100
  | Speed -> stats.speed <- d stats.speed pow 100
  | _ -> ()

let calculate_stats level bstats ivs evs nature =
  let un_mod_stats =
    {
      max_hp =
        ((2 * bstats.max_hp) + ivs.max_hp + (evs.max_hp / 4))
        * level / 100
        + level + 10;
      attack =
        ((2 * bstats.attack) + ivs.attack + (evs.attack / 4))
        * level / 100
        + 5;
      defense =
        ((2 * bstats.defense) + ivs.defense + (evs.defense / 4))
        * level / 100
        + 5;
      sp_attack =
        ((2 * bstats.sp_attack) + ivs.sp_attack + (evs.sp_attack / 4))
        * level / 100
        + 5;
      sp_defense =
        ((2 * bstats.sp_defense) + ivs.sp_defense + (evs.sp_defense / 4))
        * level / 100
        + 5;
      speed =
        ((2 * bstats.speed) + ivs.speed + (evs.speed / 4))
        * level / 100
        + 5;
    }
  in
  mod_statc un_mod_stats nature.nerf 0.90;
  mod_statc un_mod_stats nature.buff 1.1;
  un_mod_stats

(** Generate Indiviudal Values (IVs) for the creature, randomizng the
    seed each time*)
let generate_ivs rand_func =
  Random.init (rand 107374184 ());
  {
    max_hp = rand_func 32 ();
    attack = rand_func 32 ();
    defense = rand_func 32 ();
    sp_attack = rand_func 32 ();
    sp_defense = rand_func 32 ();
    speed = rand_func 32 ();
  }

let blank_evs =
  {
    max_hp = 0;
    attack = 0;
    defense = 0;
    sp_attack = 0;
    sp_defense = 0;
    speed = 0;
  }

let generate_nature nat_rand =
  let nat_list =
    [
      "Hardy";
      "Lonely";
      "Adamant";
      "Naughty";
      "Brave";
      "Bold";
      "Docile";
      "Impish";
      "Lax";
      "Relaxed";
      "Modest";
      "Mild";
      "Bashful";
      "Rash";
      "Quiet";
      "Calm";
      "Gentle";
      "Careful";
      "Quirky";
      "Sassy";
      "Timid";
      "Hasty";
      "Jolly";
      "Naive";
      "Serious";
    ]
  in
  let stat_list = [ Attack; Defense; Sp_Attack; Sp_Defense; Speed ] in
  {
    name = List.nth nat_list nat_rand;
    buff = List.nth stat_list (nat_rand / 5);
    nerf = List.nth stat_list (nat_rand mod 5);
    id = nat_rand;
  }

let exp_calc level rate =
  match rate with
  | Fast -> 4 * level * level * level / 5
  | MediumFast -> level * level * level
  | MediumSlow ->
      (6 * level * level * level / 5)
      - (15 * level * level)
      + (100 * level) - 140
  | Slow -> 5 * level * level * level / 4

let creature_from_json json level =
  let bstats = stats_of_json (json |> member "base_stats") in
  let ivs = generate_ivs rand in
  let random_nature = generate_nature (rand 25 ()) in
  let curr_stats =
    calculate_stats level bstats ivs blank_evs random_nature
  in
  let lev_rate = level_rate_from_json json in
  {
    name = json |> member "name" |> to_string;
    level;
    current_hp = curr_stats.max_hp;
    exp = exp_calc level lev_rate;
    base_stats = bstats;
    current_stats = curr_stats;
    iv_stats = ivs;
    ev_stats = blank_evs;
    current_status = Healthy;
    etypes = (etype_from_json json 1, etype_from_json json 2);
    leveling_rate = lev_rate;
    nature = random_nature;
    ev_gain =
      ( json |> member "ev_stat" |> to_string,
        json |> member "ev_amount" |> to_int );
    poke_id = json |> member "poke_id" |> to_int;
    catch_rate = json |> member "catch_rate" |> to_int;
    base_exp = json |> member "base_exp" |> to_int;
    friendship = 70;
    learnset = [];
    moves = [];
  }

let create_creature name level =
  let json = Yojson.Basic.from_file "assets/util/creature_list.json" in
  creature_from_json (json |> member name) level

let get_nature creature =
  (creature.nature.name, creature.nature.buff, creature.nature.nerf)

let get_types creature = creature.etypes
let get_stats creature = creature.current_stats
let get_current_hp creature = creature.current_hp
let set_current_hp creature amount = creature.current_hp <- amount

let get_type_mod etype_var defender =
  let etype = etype_to_string etype_var in
  let json = Yojson.Basic.from_file "assets/util/type_chart.json" in
  let effective =
    json |> member etype |> member "Supereffective" |> to_list
    |> List.map to_string
  in
  let noteffective =
    json |> member etype |> member "Resists" |> to_list
    |> List.map to_string
  in
  let noeffect =
    json |> member etype |> member "Immune" |> to_list
    |> List.map to_string
  in
  let type_mod def_type =
    if List.mem def_type effective then 2.
    else if List.mem def_type noteffective then 0.5
    else if List.mem def_type noeffect then 0.
    else 1.
  in
  match defender.etypes with
  | t1, t2 ->
      type_mod (etype_to_string t1) *. type_mod (etype_to_string t2)

let get_stab_mod creature etype =
  let e1, e2 = creature.etypes in
  if e1 = etype then 1.5 else if e2 = etype then 1.5 else 1.0
