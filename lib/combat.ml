open Creature

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

type battle_record = {
  player_creatures : creature list;
  enemy_creatures : creature list;
  battle_type : btype;
  battle_status : bstatus;
  escape_attempts : int;
  player_move : move_status;
  enemy_move : move_status;
  turn_counter : int;
  turn_pos : turn_status;
}

let player_first = ref false
let is_player_first = player_first.contents

let empty_battle =
  {
    player_creatures = [];
    enemy_creatures = [];
    battle_type = Wild;
    battle_status = Ongoing;
    escape_attempts = 0;
    player_move = None Healthy;
    enemy_move = None Healthy;
    turn_counter = 0;
    turn_pos = Choosing;
  }

(*might create active creatures and inactive creature for each?*)
let wild_init plist elist =
  {
    player_creatures = plist;
    enemy_creatures = elist;
    battle_type = Wild;
    battle_status = Ongoing;
    escape_attempts = 0;
    player_move = None (get_status (List.nth plist 0));
    enemy_move = None Healthy;
    turn_counter = 0;
    turn_pos = Choosing;
  }

let trainer_init plist elist =
  {
    player_creatures = plist;
    enemy_creatures = elist;
    battle_type = Trainer;
    battle_status = Ongoing;
    escape_attempts = 0;
    player_move = None (get_status (List.nth plist 0));
    enemy_move = None Healthy;
    turn_counter = 0;
    turn_pos = Choosing;
  }

(*HELPERS FOR TURN_BUILDER*)
let rand_move brecord =
  let possible_moves = get_moves (List.nth brecord.enemy_creatures 0) in
  let random_pos = Random.int (List.length possible_moves - 1) in
  List.nth possible_moves random_pos

let player_input_move brecord move_id =
  if get_status (List.nth brecord.player_creatures 0) = Healthy then
    { brecord with player_move = Move move_id }
  else
    {
      brecord with
      player_move =
        None (get_status (List.nth brecord.player_creatures 0));
    }

let enemy_choose_move brecord =
  let pokestatus = get_status (List.nth brecord.enemy_creatures 0) in
  if pokestatus = Healthy then
    { brecord with enemy_move = Move (rand_move brecord) }
  else { brecord with enemy_move = None pokestatus }
(*HELPERS FOR TURN_BUILDER END*)

let turn_builder brecord player_move =
  let pmove = player_input_move brecord player_move in
  let emove = enemy_choose_move brecord in
  {
    brecord with
    player_move = pmove.player_move;
    enemy_move = emove.enemy_move;
    turn_pos = Pending;
  }

(*HELPERS FOR BATTLE SIM FNS*)
let get_crit () =
  let x = Util.rand 16 () in
  if x = 0 then 2. else 1.

let damage_calc move attacker defender =
  let a = get_stats attacker in
  let b = get_stats defender in
  let d a b c =
    let x = a * b in
    x / c
  in
  let x =
    match move.category with
    | Physical -> d a.attack move.power b.defense
    | Special -> d a.sp_attack move.power b.sp_defense
    | _ -> 0
  in
  let base_damage =
    float_of_int (d (d 2 (get_level attacker) 5 + 2) x 50 + 2)
  in
  let total_damage =
    base_damage
    *. get_stab_mod attacker move.etype
    *. get_type_mod move.etype defender
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
  | [ _ ] -> { brecord with battle_status = Loss }
  | _ :: _ ->
      if active_crtr_filter brecord.player_creatures <> [] then
        {
          brecord with
          player_creatures =
            active_crtr_filter brecord.player_creatures
            @ inactive_crtr_filter brecord.player_creatures;
        }
      else { brecord with battle_status = Loss }
  | [] -> raise Empty

let updated_enemy_creatures brecord =
  match brecord.enemy_creatures with
  | [ _ ] -> { brecord with battle_status = Victory }
  | _ :: _ ->
      if active_crtr_filter brecord.enemy_creatures <> [] then
        {
          brecord with
          enemy_creatures =
            active_crtr_filter brecord.enemy_creatures
            @ inactive_crtr_filter brecord.enemy_creatures;
        }
      else { brecord with battle_status = Victory }
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

let exec_turn_pte brecord =
  let player, enemy =
    ( List.nth brecord.player_creatures 0,
      List.nth brecord.enemy_creatures 0 )
  in
  let damage_pte =
    match brecord.player_move with
    | None _ -> 0.0
    | Move m -> if m.power > 0 then damage_calc m player enemy else 0.0
  in
  set_current_hp enemy (get_current_hp enemy - int_of_float damage_pte);

  if get_current_hp enemy > 0 then
    (* let damage_etp = match brecord.enemy_move with | None _ -> 0.0 |
       Move m -> damage_calc m enemy player in set_current_hp player
       (get_current_hp player - int_of_float damage_etp); *)
    brecord
  else updated_enemy_creatures brecord

let exec_turn_etp brecord =
  let player, enemy =
    ( List.nth brecord.player_creatures 0,
      List.nth brecord.enemy_creatures 0 )
  in
  let damage_etp =
    match brecord.enemy_move with
    | None _ -> 0.0
    | Move m -> if m.power > 0 then damage_calc m enemy player else 0.0
  in
  set_current_hp player (get_current_hp player - int_of_float damage_etp);
  if get_current_hp (List.nth brecord.player_creatures 0) > 0 then
    brecord
  else updated_player_creatures brecord

let execute_turn brecord =
  if brecord.turn_pos = Pending then
    if player_first.contents then exec_turn_pte brecord
    else exec_turn_etp brecord
  else
    match brecord.player_move with
    | None _ -> exec_turn_etp brecord
    | Move _ -> exec_turn_pte brecord
(*BATTLE SIM HELPERS END so many damn*)

let battle_sim_fh brecord =
  player_first.contents <- player_faster brecord;
  let turn_exec = execute_turn brecord in
  if player_faster brecord then
    {
      turn_exec with
      player_move =
        None (get_status (List.nth turn_exec.player_creatures 0));
      turn_pos = Halfway;
    }
  else
    {
      turn_exec with
      enemy_move =
        None (get_status (List.nth turn_exec.enemy_creatures 0));
      turn_pos = Halfway;
    }

let battle_sim_sh brecord =
  let turn_exec2 = execute_turn brecord in
  {
    turn_exec2 with
    player_move =
      None (get_status (List.nth turn_exec2.player_creatures 0));
    enemy_move =
      None (get_status (List.nth turn_exec2.enemy_creatures 0));
    turn_pos = Finished;
  }

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
