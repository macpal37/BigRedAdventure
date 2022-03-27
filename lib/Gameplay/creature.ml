open Yojson.Basic.Util
open Util

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
  level_learned : int;
}

type status =
  | Healthy
  | Sleep of int
  | Freeze of int
  | Poison of int
  | Confusion of int
  | Paralyze of int
  | Burn of int
  | Fainted

type etype =
  | Normal
  | Fire
  | Water
  | Grass
  | Fairy
  | Rock
  | Ghost
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

(**=========== Moves===========**)

type move_catgeory =
  | Physical
  | Special
  | Status

type move = {
  move_name : string;
  power : int;
  accuracy : float;
  mutable curr_pp : int;
  mutable max_pp : int;
  etype : etype;
  category : move_catgeory;
  description : string;
  effect_id : int;
}

let empty_move =
  {
    move_name = " ";
    power = 0;
    accuracy = 0.0;
    curr_pp = 0;
    max_pp = 0;
    etype = None;
    category = Status;
    description = "";
    effect_id = 0;
  }

type creature = {
  mutable nickname : string;
  species : string;
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
  ev_gain : stat * int;
  poke_id : int;
  catch_rate : int;
  base_exp : int;
  mutable friendship : int;
  learnset : learnset_moves list;
  mutable moves : move list;
  mutable front_sprite : Draw.sprite;
  mutable back_sprite : Draw.sprite;
}

let get_front_sprite creature = creature.front_sprite
let set_front_sprite creature sprite = creature.front_sprite <- sprite
let get_back_sprite creature = creature.back_sprite
let set_back_sprite creature sprite = creature.back_sprite <- sprite
let set_status crtr stat = crtr.current_status <- stat

let string_of_status stat_var =
  match stat_var with
  | Sleep _ -> "Sleep"
  | Poison _ -> "Posion"
  | Burn _ -> "Burn"
  | Freeze _ -> "Freeze"
  | Paralyze _ -> "Paralyze"
  | Healthy -> "Healthy"
  | Confusion _ -> "Confusion"
  | Fainted -> "Fainted"

let get_moves creature = creature.moves

let add_pp creature move_name amount =
  let move =
    List.nth
      (List.filter (fun x -> x.move_name = move_name) creature.moves)
      0
  in
  let total_pp = bound (move.curr_pp + amount) 0 move.max_pp in
  move.curr_pp <- total_pp

let get_status creature = creature.current_status

let string_of_stat stat_var =
  match stat_var with
  | HP -> "HP"
  | Attack -> "Attack"
  | Defense -> "Defense"
  | Sp_Attack -> "Sp_Attack"
  | Sp_Defense -> "Sp_Defense"
  | Speed -> "Speed"

let stat_of_string stat_var =
  match stat_var with
  | "HP" -> HP
  | "Attack" -> Attack
  | "Defense" -> Defense
  | "Sp_Attack" -> Sp_Attack
  | "Sp_Defense" -> Sp_Defense
  | "Speed" -> Speed
  | _ -> HP

let string_of_etype etype_var =
  match etype_var with
  | Normal -> "Normal"
  | Fire -> "Fire"
  | Water -> "Water"
  | Grass -> "Grass"
  | Fairy -> "Fairy"
  | Rock -> "Rock"
  | Ghost -> "Ghost"
  | _ -> "None"

let etype_of_string etype_string =
  match etype_string with
  | "Normal" -> Normal
  | "Fire" -> Fire
  | "Water" -> Water
  | "Grass" -> Grass
  | "Fairy" -> Fairy
  | "Rock" -> Rock
  | "Ghost" -> Ghost
  | _ -> None

let category_of_string cat_string =
  match cat_string with
  | "Physical" -> Physical
  | "Special" -> Special
  | "Status" -> Status
  | _ -> Status

(**=============== JSON Parsing ============**)
let parse_learn_set json =
  {
    move = json |> member "move" |> to_string;
    level_learned = json |> member "level" |> to_int;
  }

let stats_of_json json =
  {
    max_hp = json |> member "hp" |> to_int;
    attack = json |> member "attack" |> to_int;
    defense = json |> member "defense" |> to_int;
    sp_attack = json |> member "sp_attack" |> to_int;
    sp_defense = json |> member "sp_defense" |> to_int;
    speed = json |> member "speed" |> to_int;
  }

let etype_from_json json num =
  json
  |> member ("type" ^ string_of_int num)
  |> to_string |> etype_of_string

let level_rate_from_json json =
  let rate_strint = json |> member "level_rate" |> to_string in
  match rate_strint with
  | "Fast" -> Fast
  | "MediumFast" -> MediumFast
  | "MediumSlow" -> MediumSlow
  | "Slow" -> Slow
  | _ -> Fast

let parse_move name json =
  {
    move_name = name;
    power = json |> member "power" |> to_int;
    accuracy = float_of_int (json |> member "accuracy" |> to_int);
    curr_pp = json |> member "pp" |> to_int;
    max_pp = json |> member "pp" |> to_int;
    etype = etype_of_string (json |> member "type" |> to_string);
    category =
      category_of_string (json |> member "category" |> to_string);
    description = json |> member "description" |> to_string;
    effect_id = json |> member "effect_ids" |> to_int;
  }

(**=============== Moves ============**)
let get_move name =
  let move_json = Yojson.Basic.from_file "assets/util/move_list.json" in
  parse_move name (move_json |> member name)

let generate_moves learnset level =
  let possible_moves =
    List.rev (List.filter (fun a -> a.level_learned <= level) learnset)
  in
  let rec get_four_moves moves count =
    match moves with
    | [] -> []
    | h :: t ->
        if count < 4 then get_move h.move :: get_four_moves t (count + 1)
        else []
  in
  List.rev (get_four_moves possible_moves 0)

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

let empty_stats =
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

let curr_hp_cache = ref 0

let creature_from_json json level =
  Random.self_init ();
  let name = json |> member "name" |> to_string in
  let bstats = stats_of_json (json |> member "base_stats") in
  let ivs = generate_ivs rand in
  let random_nature = generate_nature (rand 25 ()) in
  let curr_stats =
    calculate_stats level bstats ivs empty_stats random_nature
  in
  let lev_rate = level_rate_from_json json in
  let learnset =
    json |> member "learnset" |> to_list |> List.map parse_learn_set
  in
  let shiny_chance = Random.int 4 = 0 in
  curr_hp_cache.contents <- curr_stats.max_hp;
  {
    nickname = name;
    species = name;
    level;
    current_hp = curr_stats.max_hp;
    exp = exp_calc level lev_rate;
    base_stats = bstats;
    current_stats = curr_stats;
    iv_stats = ivs;
    ev_stats = empty_stats;
    current_status = Healthy;
    etypes = (etype_from_json json 1, etype_from_json json 2);
    leveling_rate = lev_rate;
    nature = random_nature;
    ev_gain =
      ( stat_of_string (json |> member "ev_stat" |> to_string),
        json |> member "ev_amount" |> to_int );
    poke_id = json |> member "poke_id" |> to_int;
    catch_rate = json |> member "catch_rate" |> to_int;
    base_exp = json |> member "base_exp" |> to_int;
    friendship = 70;
    learnset;
    moves = generate_moves learnset level;
    front_sprite =
      (if name = "#" then Draw.empty_sprite
      else if shiny_chance = false then
        Draw.load_creature (String.lowercase_ascii name ^ "_front") ()
      else
        Draw.load_creature
          (String.lowercase_ascii name ^ "_front_shiny")
          ());
    (* Draw.load_creature ("jollitriks" ^ "_front") ()); *)
    back_sprite =
      (if name = "#" then Draw.empty_sprite
      else if shiny_chance = false then
        Draw.load_creature (String.lowercase_ascii name ^ "_back") ()
      else
        Draw.load_creature
          (String.lowercase_ascii name ^ "_back_shiny")
          ());
  }

let create_creature name level =
  let json = Yojson.Basic.from_file "assets/util/creature_list.json" in
  creature_from_json (json |> member name) level

let get_nature creature =
  (creature.nature.name, creature.nature.buff, creature.nature.nerf)

let get_types creature = creature.etypes
let get_stats creature = creature.current_stats
let get_ivs creature = creature.iv_stats
let get_evs creature = creature.ev_stats
let get_ev_gain creature = creature.ev_gain
let get_exp_gain creature = creature.level * creature.base_exp
let get_current_hp creature = creature.current_hp
let get_specias creature = creature.species

let set_current_hp creature amount =
  curr_hp_cache.contents <- creature.current_hp;
  creature.current_hp <- amount

let get_hp_status creature =
  ( creature.current_stats.max_hp,
    curr_hp_cache.contents,
    creature.current_hp )

let get_catch_rate creature = float_of_int creature.catch_rate

let get_type_mod etype_var defender =
  let etype = string_of_etype etype_var in
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
      type_mod (string_of_etype t1) *. type_mod (string_of_etype t2)

let get_stab_mod creature etype =
  let e1, e2 = creature.etypes in
  if e1 = etype then 1.5 else if e2 = etype then 1.5 else 1.0

(** [get_level creature] returns a [creature]'s current level*)
let get_level creature = creature.level

(** [get_exp creature] returns a [creature]'s current exp*)
let get_exp creature =
  ( creature.exp,
    exp_calc creature.level creature.leveling_rate,
    exp_calc (creature.level + 1) creature.leveling_rate )

let print_level_up creature () =
  let old_stats = creature.current_stats in
  let new_stats =
    calculate_stats (creature.level + 1) creature.base_stats
      creature.iv_stats creature.ev_stats creature.nature
  in
  print_endline
    "=======================================================";
  print_endline
    ("LEVELUP: Lvl: " ^ string_of_int (get_level creature + 1));
  print_endline
    ("HP:\t"
    ^ string_of_int old_stats.max_hp
    ^ "\t--> "
    ^ string_of_int new_stats.max_hp
    ^ "\t+"
    ^ string_of_int (new_stats.max_hp - old_stats.max_hp));
  print_endline
    ("ATK:\t"
    ^ string_of_int old_stats.attack
    ^ "\t--> "
    ^ string_of_int new_stats.attack
    ^ "\t+"
    ^ string_of_int (new_stats.attack - old_stats.attack));
  print_endline
    ("DEF:\t"
    ^ string_of_int old_stats.defense
    ^ "\t--> "
    ^ string_of_int new_stats.defense
    ^ "\t+"
    ^ string_of_int (new_stats.defense - old_stats.defense));
  print_endline
    ("SPATK:\t"
    ^ string_of_int old_stats.sp_attack
    ^ "\t--> "
    ^ string_of_int new_stats.sp_attack
    ^ "\t+"
    ^ string_of_int (new_stats.sp_attack - old_stats.sp_attack));
  print_endline
    ("SPDEF:\t"
    ^ string_of_int old_stats.sp_defense
    ^ "\t--> "
    ^ string_of_int new_stats.sp_defense
    ^ "\t+"
    ^ string_of_int (new_stats.sp_defense - old_stats.sp_defense));
  print_endline
    ("SPD:\t"
    ^ string_of_int old_stats.speed
    ^ "\t--> "
    ^ string_of_int new_stats.speed
    ^ "\t+"
    ^ string_of_int (new_stats.speed - old_stats.speed));
  print_endline
    "======================================================="

(** [add_exp creature amount] add [amount] to the current exp of
    [creature]*)
let add_exp creature amount =
  let cap_exp = exp_calc (creature.level + 1) creature.leveling_rate in
  let exp_dif = cap_exp - creature.exp in

  let rec levelup_check amount dif =
    if amount > dif then begin
      print_level_up creature ();
      creature.level <- creature.level + 1;
      creature.exp <- creature.exp + dif;
      creature.current_stats <-
        calculate_stats creature.level creature.base_stats
          creature.iv_stats creature.ev_stats creature.nature;
      let new_dif =
        exp_calc (creature.level + 1) creature.leveling_rate
        - creature.exp
      in
      levelup_check (amount - dif) new_dif
    end
    else creature.exp <- creature.exp + amount
  in
  levelup_check amount exp_dif

(** [get_nickname creature] returns a [creature]'s nickname*)
let get_nickname creature = creature.nickname

let set_nickname creature nickname = creature.nickname <- nickname
