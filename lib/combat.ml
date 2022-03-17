open Creature
open Draw

exception Empty
(*exception OutOfPokemon exception IncorrectTurnPos*)

(*BRECORD VARIANTS*)
type bstatus =
  | Victory
  | Loss
  | Flee
  | Catch
  | Ongoing

type btype =
  | Trainer
  | Wild

type move_status =
  | None of status
  | Move of move

type turn_status =
  | Choosing
  | Pending
  | Halfway
  | Finished
(*BRECORD VARIANTS END*)

type battle_creature = {
  mutable creature : creature;
  mutable current_move : move_status;
  mutable stat_changes : stats;
}

type battle_record = {
  mutable player_creatures : creature list;
  mutable enemy_creatures : creature list;
  battle_type : btype;
  mutable battle_status : bstatus;
  escape_attempts : int;
  mutable player_battler : battle_creature;
  mutable enemy_battler : battle_creature;
  mutable turn_counter : int;
  mutable turn_pos : turn_status;
}

let player_first = ref false
let is_player_first () = player_first.contents

let empty_battle =
  {
    player_creatures = [];
    enemy_creatures = [];
    battle_type = Wild;
    battle_status = Ongoing;
    escape_attempts = 0;
    player_battler =
      {
        creature = create_creature "missingno" 1;
        current_move = None Fainted;
        stat_changes = empty_stats;
      };
    enemy_battler =
      {
        creature = create_creature "missingno" 1;
        current_move = None Fainted;
        stat_changes = empty_stats;
      };
    turn_counter = 0;
    turn_pos = Choosing;
  }

(*might create active creatures and inactive creature for each?*)
let wild_init plist elist =
  let player, enemy = (List.nth plist 0, List.nth elist 0) in

  {
    player_creatures = plist;
    enemy_creatures = elist;
    battle_type = Wild;
    battle_status = Ongoing;
    escape_attempts = 0;
    player_battler =
      {
        creature = player;
        current_move = None (get_status player);
        stat_changes = empty_stats;
      };
    enemy_battler =
      {
        creature = enemy;
        current_move = None (get_status enemy);
        stat_changes = empty_stats;
      };
    turn_counter = 0;
    turn_pos = Choosing;
  }

let trainer_init plist elist =
  let player, enemy = (List.nth plist 0, List.nth elist 0) in
  {
    player_creatures = plist;
    enemy_creatures = elist;
    battle_type = Trainer;
    battle_status = Ongoing;
    escape_attempts = 0;
    player_battler =
      {
        creature = player;
        current_move = None (get_status player);
        stat_changes = empty_stats;
      };
    enemy_battler =
      {
        creature = enemy;
        current_move = None (get_status enemy);
        stat_changes = empty_stats;
      };
    turn_counter = 0;
    turn_pos = Choosing;
  }

(*HELPERS FOR TURN_BUILDER*)
let rand_move brecord =
  let possible_moves = get_moves (List.nth brecord.enemy_creatures 0) in
  let random_pos = Random.int (List.length possible_moves - 1) in
  List.nth possible_moves random_pos

let handle_battler_status battler move =
  match get_status battler.creature with
  | Healthy -> battler.current_move <- Move move
  | _ -> battler.current_move <- None Healthy

(*HELPERS FOR TURN_BUILDER END*)

let turn_builder brecord player_move =
  handle_battler_status brecord.player_battler player_move;
  handle_battler_status brecord.enemy_battler (rand_move brecord)

(*HELPERS FOR BATTLE SIM FNS*)
let get_crit () =
  let x = Util.rand 16 () in
  if x = 0 then 2. else 1.

let stat_modified stat stages =
  if stages > 0 then stat * (stages + 2) * 100 / 200
  else if stages < 0 then stat * 200 / (((-1 * stages) + 2) * 100)
  else stat

let damage_calc move attacker defender =
  let a = get_stats attacker.creature in
  let b = get_stats defender.creature in
  let d a b c =
    let x = a * b in
    x / c
  in
  print_endline
    ("ATK TEST: "
    ^ string_of_int
        (stat_modified a.attack attacker.stat_changes.attack));
  let x =
    match move.category with
    | Physical ->
        d
          (stat_modified a.attack attacker.stat_changes.attack)
          move.power
          (stat_modified b.defense defender.stat_changes.defense)
    | Special ->
        d
          (stat_modified a.sp_attack attacker.stat_changes.sp_attack)
          move.power
          (stat_modified b.sp_defense defender.stat_changes.sp_defense)
    | _ -> 0
  in
  let base_damage =
    float_of_int (d (d 2 (get_level attacker.creature) 5 + 2) x 50 + 2)
  in
  let total_damage =
    base_damage
    *. get_stab_mod attacker.creature move.etype
    *. get_type_mod move.etype defender.creature
    *. get_crit ()
    *. (float_of_int (Util.rand 16 ()) +. 85.0)
    /. 100.0
  in
  total_damage

let active_crtr_filter crtrlist =
  List.filter
    (fun x -> if get_current_hp x > 0 then true else false)
    crtrlist

let inactive_crtr_filter crtrlist =
  List.filter
    (fun x -> if get_current_hp x > 0 then false else true)
    crtrlist

let updated_player_creatures brecord =
  match brecord.player_creatures with
  | [ _ ] -> brecord.battle_status <- Loss
  | _ :: _ ->
      if active_crtr_filter brecord.player_creatures <> [] then
        brecord.player_creatures <-
          active_crtr_filter brecord.player_creatures
          @ inactive_crtr_filter brecord.player_creatures
      else brecord.battle_status <- Loss
  | [] -> raise Empty

let updated_enemy_creatures brecord =
  match brecord.enemy_creatures with
  | [ _ ] -> brecord.battle_status <- Victory
  | _ :: _ ->
      if active_crtr_filter brecord.enemy_creatures <> [] then
        brecord.enemy_creatures <-
          active_crtr_filter brecord.enemy_creatures
          @ inactive_crtr_filter brecord.enemy_creatures
      else brecord.battle_status <- Victory
  | [] -> raise Empty

let player_faster brecord =
  let enemy_speed =
    (get_stats (List.nth brecord.enemy_creatures 0)).speed
  in
  let player_speed =
    (get_stats (List.nth brecord.player_creatures 0)).speed
  in
  if player_speed > enemy_speed then true
  else if player_speed < enemy_speed then false
  else if Random.int 2 = 1 then true
  else false

let stat_bound stat_val name stat_name =
  if stat_val > 6 then begin
    draw_text
      (name ^ "'s " ^ string_of_stat stat_name ^ " can't go any higher.")
      40 ();
    false
  end
  else if stat_val < -6 then begin
    draw_text (name ^ "'s ATK can't go any higher.") 40 ();
    false
  end
  else true

let handle_stat_changes battler stat stages =
  match stat with
  | HP -> ()
  | Attack ->
      let stat_change = battler.stat_changes.attack + stages in
      if stat_bound stat_change (get_nickname battler.creature) stat
      then battler.stat_changes.attack <- stat_change
  | Defense ->
      let stat_change = battler.stat_changes.attack + stages in
      if stat_bound stat_change (get_nickname battler.creature) stat
      then battler.stat_changes.defense <- stat_change
  | Sp_Attack ->
      let stat_change = battler.stat_changes.attack + stages in
      if stat_bound stat_change (get_nickname battler.creature) stat
      then battler.stat_changes.sp_attack <- stat_change
  | Sp_Defense ->
      let stat_change = battler.stat_changes.attack + stages in
      if stat_bound stat_change (get_nickname battler.creature) stat
      then battler.stat_changes.sp_defense <- stat_change
  | Speed ->
      let stat_change = battler.stat_changes.attack + stages in
      if stat_bound stat_change (get_nickname battler.creature) stat
      then battler.stat_changes.speed <- stat_change

let handle_effects move attacker defender () =
  match move.effect_id with
  | 1 ->
      draw_text (get_nickname defender.creature ^ "'s ATK fell") 40 ();
      handle_stat_changes defender Attack (-1)
  | 7 ->
      draw_text (get_nickname attacker.creature ^ "'s ATK rose") 40 ();
      handle_stat_changes defender Attack 1
  | _ -> ()

let exec_turn attacker defender brecord =
  let damage_pte =
    match attacker.current_move with
    | None _ -> 0.0
    | Move m ->
        draw_text
          (get_nickname attacker.creature ^ " used " ^ m.move_name)
          40 ();
        handle_effects m attacker defender ();
        if m.power > 0 then begin
          print_endline ("EFFECT: " ^ string_of_int m.effect_id);

          damage_calc m attacker defender
        end
        else 0.0
  in

  set_current_hp defender.creature
    (get_current_hp defender.creature - int_of_float damage_pte);

  if get_current_hp defender.creature > 0 then ()
  else if defender.creature = brecord.player_battler.creature then
    updated_player_creatures brecord
  else updated_enemy_creatures brecord

let execute_turn brecord =
  if brecord.turn_pos = Pending then
    if player_first.contents then
      exec_turn brecord.player_battler brecord.enemy_battler brecord
    else exec_turn brecord.enemy_battler brecord.player_battler brecord
  else
    match brecord.player_battler.current_move with
    | None _ ->
        exec_turn brecord.enemy_battler brecord.player_battler brecord
    | Move _ ->
        exec_turn brecord.player_battler brecord.enemy_battler brecord
(*BATTLE SIM HELPERS END so many damn*)

let battle_sim_fh brecord =
  let res = player_faster brecord in
  player_first.contents <- res;
  execute_turn brecord;

  if player_faster brecord then begin
    brecord.player_battler.current_move <-
      None (get_status brecord.player_battler.creature);
    brecord.turn_pos <- Halfway
  end
  else begin
    brecord.enemy_battler.current_move <-
      None (get_status brecord.enemy_battler.creature);
    brecord.turn_pos <- Halfway
  end

let battle_sim_sh brecord =
  if brecord.battle_status <> Victory then begin
    execute_turn brecord;
    brecord.player_battler.current_move <-
      None (get_status brecord.player_battler.creature);
    brecord.enemy_battler.current_move <-
      None (get_status brecord.enemy_battler.creature);
    brecord.turn_pos <- Finished
  end

(*IGNORE THESE FOR NOW, WILL POLISH IMPLEMENTATION LATER*)

let run_away brecord =
  let pspeed =
    (get_stats (List.nth brecord.player_creatures 0)).speed
  in
  let espeed = (get_stats (List.nth brecord.enemy_creatures 0)).speed in
  let odds_escape =
    ((pspeed * 32 / (espeed / 4 mod 256)) + 30)
    * brecord.escape_attempts
  in
  if
    pspeed >= espeed || odds_escape > 255
    || Random.int 256 > odds_escape
  then { brecord with battle_status = Flee }
  else { brecord with escape_attempts = brecord.escape_attempts + 1 }

(*let status_multiplier stt = match stt with | Sleep -> 4 | Freeze -> 4
  | Paralyze -> 3 | Poison -> 3 | Burn -> 3 | _ -> 2*)

let capture brecord =
  let e_currHP = get_current_hp (List.nth brecord.enemy_creatures 0) in
  let e_maxHP =
    (get_stats (List.nth brecord.enemy_creatures 0)).max_hp
  in
  let e_rate = 150 in
  let e_status = 1 (*Get status effect here*) in
  let ball_bonus = 1 in
  let odds_catch =
    ((3 * e_maxHP) - (2 * e_currHP * e_rate * ball_bonus))
    / (3 * e_maxHP) * e_status / 2
    (*might adjust to do floats instead*)
  in
  if odds_catch >= 255 then { brecord with battle_status = Catch }
  else { brecord with battle_status = Loss }
(*Filler, implement rest here*)
