open Yojson.Basic.Util
open Util
open Graphics

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
  | Paralyze
  | Burn
  | Fainted

type etype =
  | Normal
  | Fire
  | Water
  | Grass
  | Fairy
  | Rock
  | Ghost
  | Dark
  | Steel
  | Electric
  | Poison
  | Psychic
  | Ground
  | Dragon
  | Bug
  | Ice
  | Fighting
  | None
(* type etype = | Neutral | Fire | Water | Air | Earth | Electric | Ice
   | Metal | Acid | Light | Shadow | Specter | Nature | Cosmic | None *)

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
  effect_ids : int list;
  effect_chance : int;
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
    effect_ids = [ -1 ];
    effect_chance = 0;
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
  shiny : bool;
}

let get_front_sprite creature = creature.front_sprite
let set_front_sprite creature sprite = creature.front_sprite <- sprite
let get_back_sprite creature = creature.back_sprite
let set_back_sprite creature sprite = creature.back_sprite <- sprite

let string_of_status stat_var =
  match stat_var with
  | Sleep _ -> "Sleep"
  | Poison _ -> "Posion"
  | Burn -> "Burn"
  | Freeze _ -> "Freeze"
  | Paralyze -> "Paralyze"
  | Healthy -> "Healthy"
  | Fainted -> "Fainted"

let get_moves creature = creature.moves
let set_moves creature moves = creature.moves <- moves

let get_move_i creature i =
  let size = List.length creature.moves in
  if size <= i then List.nth creature.moves (size - 1)
  else if i < 0 then List.nth creature.moves 0
  else List.nth creature.moves i

let get_move_description_i creature i =
  if List.length creature.moves <= i || i < 0 then ""
  else (List.nth creature.moves i).description

let add_pp creature move_name amount =
  let move =
    List.nth
      (List.filter (fun x -> x.move_name = move_name) creature.moves)
      0
  in
  let total_pp = bound (move.curr_pp + amount) 0 move.max_pp in
  move.curr_pp <- total_pp

let get_status creature = creature.current_status

let string_of_stat_short stat_var =
  match stat_var with
  | HP -> "HP"
  | Attack -> "ATK"
  | Defense -> "DEF"
  | Sp_Attack -> "S.ATK"
  | Sp_Defense -> "S.DEF"
  | Speed -> "SPD"

let string_of_stat stat_var =
  match stat_var with
  | HP -> "HP"
  | Attack -> "Attack"
  | Defense -> "Defense"
  | Sp_Attack -> "Sp.Attack"
  | Sp_Defense -> "Sp.Defense"
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
  | Dark -> "Dark"
  | Steel -> "Steel"
  | Electric -> "Electric"
  | Poison -> "Poison"
  | Psychic -> "Psychic"
  | Ground -> "Ground"
  | Dragon -> "Dragon"
  | Bug -> "Bug"
  | Ice -> "Ice"
  | Fighting -> "Fighting"
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
  | "Dark" -> Dark
  | "Steel" -> Steel
  | "Electric" -> Electric
  | "Poison" -> Poison
  | "Psychic" -> Psychic
  | "Ground" -> Ground
  | "Dragon" -> Dragon
  | "Bug" -> Bug
  | "Ice" -> Ice
  | "Fighting" -> Fighting
  | _ -> None

(* let string_of_etype etype_var = match etype_var with | Neutral ->
   "Neutral" | Fire -> "Fire" | Water -> "Water" | Air -> "Air" | Nature
   -> "Nature" | Light -> "Light" | Earth -> "Earth" | Specter ->
   "Specter" | Shadow -> "Shadow" | Metal -> "Metal" | Electric ->
   "Electric" | Acid -> "Acid" | Cosmic -> "Cosmic" | Ice -> "Ice" | _
   -> "None" *)

(* let etype_of_string etype_string = match etype_string with |
   "Neutral" -> Neutral | "Fire" -> Fire | "Water" -> Water | "Nature"
   -> Nature | "Light" -> Light | "Earth" -> Earth | "Specter" ->
   Specter | "Shadow" -> Shadow | "Metal" -> Metal | "Electric" ->
   Electric | "Acid" -> Acid | "Cosmic" -> Cosmic | "Ice" -> Ice | _ ->
   None *)

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
    effect_ids =
      json |> member "effect_ids" |> to_list |> List.map to_int;
    effect_chance = json |> member "effect_chance" |> to_int;
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

let create_creature name level =
  let json =
    Yojson.Basic.from_file "assets/util/creature_list.json"
    |> member name
  in
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
  let shiny_chance = Random.int 100 = 0 in
  let sprite_sheet =
    Spritesheet.init_spritesheet
      ("assets/creature_sprites/" ^ name ^ ".png")
      80 80 3
  in
  curr_hp_cache := curr_stats.max_hp;
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
      Spritesheet.get_sprite sprite_sheet
        (if shiny_chance then 1 else 0);
    back_sprite =
      Spritesheet.get_sprite sprite_sheet
        (if shiny_chance then 3 else 2);
    shiny = shiny_chance;
  }

let get_nature creature =
  (creature.nature.name, creature.nature.buff, creature.nature.nerf)

let get_types creature = creature.etypes
let get_stats creature = creature.current_stats

let get_stat2 stats stat =
  match stat with
  | HP -> stats.max_hp
  | Attack -> stats.attack
  | Defense -> stats.defense
  | Sp_Attack -> stats.sp_attack
  | Sp_Defense -> stats.sp_defense
  | Speed -> stats.speed

let get_stat creature stat =
  match stat with
  | HP -> creature.current_stats.max_hp
  | Attack -> creature.current_stats.attack
  | Defense -> creature.current_stats.defense
  | Sp_Attack -> creature.current_stats.sp_attack
  | Sp_Defense -> creature.current_stats.sp_defense
  | Speed -> creature.current_stats.speed

let get_ivs creature = creature.iv_stats
let get_evs creature = creature.ev_stats

let get_ev_gain creature =
  let a, b = creature.ev_gain in
  (a, b * 2)

exception NoEffect

let add_hp creature amount =
  if
    creature.current_hp <> creature.current_stats.max_hp
    && creature.current_status <> Fainted
  then
    creature.current_hp <-
      Util.bound
        (creature.current_hp + amount)
        0 creature.current_stats.max_hp
  else raise NoEffect

let apply_status c s =
  if s <> Fainted then
    if c.current_status = Healthy then c.current_status <- s
    else raise NoEffect
  else c.current_status <- s

let remove_status c s =
  if c.current_status <> Healthy && c.current_status = s then
    c.current_status <- Healthy
  else raise NoEffect

let add_ev_gain creature (stat, amount) =
  match stat with
  | Attack ->
      creature.ev_stats.attack <- creature.ev_stats.attack + amount
  | Defense ->
      creature.ev_stats.defense <- creature.ev_stats.defense + amount
  | Sp_Attack ->
      creature.ev_stats.sp_attack <-
        creature.ev_stats.sp_attack + amount
  | Sp_Defense ->
      creature.ev_stats.sp_defense <-
        creature.ev_stats.sp_defense + amount
  | Speed -> creature.ev_stats.speed <- creature.ev_stats.speed + amount
  | _ -> ()

let get_exp_gain creature = (creature.level * creature.base_exp / 5) + 1
let get_current_hp creature = creature.current_hp
let get_specias creature = creature.species

let set_current_hp creature amount =
  curr_hp_cache := creature.current_hp;
  creature.current_hp <- amount

let get_hp_status creature =
  (creature.current_stats.max_hp, !curr_hp_cache, creature.current_hp)

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
    ^ "\t-->\n   "
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

let level_up creature () =
  let before_hp = creature.current_stats.max_hp in
  print_level_up creature ();
  creature.level <- creature.level + 1;
  creature.current_stats <-
    calculate_stats creature.level creature.base_stats creature.iv_stats
      creature.ev_stats creature.nature;
  creature.current_hp <-
    creature.current_hp + (creature.current_stats.max_hp - before_hp)

(** [add_exp creature amount] add [amount] to the current exp of
    [creature]*)
let add_exp creature amount =
  let cap_exp = exp_calc (creature.level + 1) creature.leveling_rate in
  let exp_dif = cap_exp - creature.exp in

  let rec levelup_check amount dif lst lvl =
    if amount > dif then begin
      let c, min, max =
        ( creature.exp,
          exp_calc lvl creature.leveling_rate,
          exp_calc (lvl + 1) creature.leveling_rate )
      in

      let lvl = lvl + 1 in
      (* print_endline ("LEVEL: " ^ string_of_int lvl); *)
      creature.exp <- creature.exp + dif;

      (* level_up creature (); *)
      let new_dif =
        exp_calc (lvl + 1) creature.leveling_rate - creature.exp
      in
      levelup_check (amount - dif) new_dif
        ((max - min, c - min, max - min, lvl) :: lst)
        lvl
    end
    else
      let c, min, max =
        ( creature.exp,
          exp_calc lvl creature.leveling_rate,
          exp_calc (lvl + 1) creature.leveling_rate )
      in
      creature.exp <- creature.exp + amount;
      (max - min, c - min, c - min + amount, lvl) :: lst
  in
  levelup_check amount exp_dif [] creature.level

(** [get_nickname creature] returns a [creature]'s nickname*)
let get_nickname creature = creature.nickname

let set_nickname creature nickname = creature.nickname <- nickname

let get_color_from_etype etype =
  match etype with
  | Normal -> rgb 196 196 196
  | Fire -> rgb 239 128 48
  | Water -> rgb 103 144 240
  | Grass -> rgb 120 200 79
  | Fairy -> rgb 238 153 172
  | Ghost -> rgb 102 46 145
  | Rock -> rgb 184 160 56
  | Dark -> rgb 112 88 72
  | Steel -> rgb 184 184 208
  | Electric -> rgb 248 207 48
  | Poison -> rgb 160 64 159
  | Psychic -> rgb 248 87 135
  | Ground -> rgb 224 192 104
  | Dragon -> rgb 112 56 248
  | Bug -> rgb 168 184 31
  | Ice -> rgb 152 216 216
  | Fighting -> rgb 192 48 40
  | _ -> rgb 0 0 0

(* let get_color_from_etype etype = match etype with | Neutral -> rgb
   196 196 196 | Fire -> rgb 239 128 48 | Water -> rgb 103 144 240 |
   Nature -> rgb 120 200 79 | Light -> rgb 238 153 172 | Specter -> rgb
   102 46 145 | Earth -> rgb 184 160 56 | Shadow -> rgb 112 88 72 |
   Metal -> rgb 184 184 208 | Electric -> rgb 248 207 48 | Acid -> rgb
   160 64 159 | Air -> rgb 224 192 104 | Cosmic -> rgb 112 56 248 | Ice
   -> rgb 152 216 216 | _ -> rgb 0 0 0 *)
