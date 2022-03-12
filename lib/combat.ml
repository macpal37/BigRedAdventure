open Creature

type bstatus =
  | Victory
  | Loss
  | Flee
  | Catch
  | Ongoing

type btype =
  | Trainer
  | Wild

type battle_record = {
  player_creatures : creature list;
  enemy_creatures : creature list;
  battle_type : btype;
  battle_status : bstatus;
  escape_attempts : int;
}

let wild_init plist elist =
  {
    player_creatures = plist;
    enemy_creatures = elist;
    battle_type = Wild;
    battle_status = Ongoing;
    escape_attempts = 0;
  }

let trainer_init plist elist =
  {
    player_creatures = plist;
    enemy_creatures = elist;
    battle_type = Trainer;
    battle_status = Ongoing;
    escape_attempts = 0;
  }

exception Empty

let reset_clist_player cpair brecord =
  match brecord.player_creatures with
  | _ :: t -> fst cpair :: t
  | _ -> raise Empty

let reset_clist_enemy cpair brecord =
  match brecord.enemy_creatures with
  | _ :: t -> snd cpair :: t
  | _ -> raise Empty

let execute_move player_or_enemy attack_id brecord =
  if player_or_enemy then
    let out_pair =
      Move.execute_move attack_id
        (List.nth brecord.player_creatures 0)
        (List.nth brecord.enemy_creatures 0)
    in

    {
      brecord with
      player_creatures = reset_clist_player out_pair brecord;
      enemy_creatures = reset_clist_enemy out_pair brecord;
    }
  else
    let out_pair =
      Move.execute_move attack_id
        (List.nth brecord.enemy_creatures 0)
        (List.nth brecord.player_creatures 0)
    in

    {
      brecord with
      player_creatures = reset_clist_player out_pair brecord;
      enemy_creatures = reset_clist_enemy out_pair brecord;
    }

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

let status_multiplier stt =
  match stt with
  | Sleep -> 4
  | Freeze -> 4
  | Paralyze -> 3
  | Poison -> 3
  | Burn -> 3
  | _ -> 2

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
