open Creature
open Creature.Move
open Util

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

type damage_type =
  | SuperEffective
  | Effective
  | NotEffective
  | Immune

(*BRECORD VARIANTS END*)

type combat_status =
  | Status of status
  | Confused of int ref * combat_status
  | Flinch of combat_status
  | LeechSeed of combat_status

type battle_creature = {
  mutable creature : creature;
  mutable current_move : move option;
  mutable stat_changes : stats;
  mutable status_cond : combat_status;
  mutable active : bool;
  is_player : bool;
}

(** Type of Action taken by a creature *)
type action =
  | ChooseMove of move
  | Damage of float * float * float * damage_type * bool
  | Heal of float
  | StatusGain of bool * combat_status
  | StatusEffect of combat_status * float * float * float
  | MaxStat
  | StatGain of int
  | Switch of creature
  | Fainted

type battle_record = {
  mutable player_creatures : creature list;
  mutable enemy_creatures : creature list;
  battle_type : btype;
  mutable battle_status : bstatus;
  mutable catch_attempts : int;
  mutable escape_attempts : int;
  mutable player_battler : battle_creature;
  mutable enemy_battler : battle_creature;
  mutable turn_counter : int;
  mutable creatures_switched : creature list;
}

type battle_action = battle_creature * action * string

let battle_actions : battle_action list ref = ref []

let add_action (action : battle_action) =
  battle_actions := action :: !battle_actions

(** Records all the events occurred in battle. *)

(* let player_first = ref false *)

let generate_battler creature player =
  {
    creature;
    current_move = None;
    stat_changes = empty_stats;
    status_cond = Status (get_status creature);
    is_player = player;
    active = true;
  }

(*might create active creatures and inactive creature for each?*)
let wild_init plist elist =
  let player, enemy =
    ( List.nth (List.filter (fun c -> get_status c <> Fainted) plist) 0,
      List.nth elist 0 )
  in
  {
    player_creatures = plist;
    enemy_creatures = elist;
    battle_type = Wild;
    battle_status = Ongoing;
    escape_attempts = 0;
    catch_attempts = 0;
    player_battler = generate_battler player true;
    enemy_battler = generate_battler enemy false;
    turn_counter = 0;
    creatures_switched = [ player ];
  }

let trainer_init plist elist =
  let player, enemy = (List.nth plist 0, List.nth elist 0) in
  {
    player_creatures = plist;
    enemy_creatures = elist;
    battle_type = Trainer;
    battle_status = Ongoing;
    escape_attempts = 0;
    catch_attempts = 0;
    player_battler = generate_battler player true;
    enemy_battler = generate_battler enemy false;
    turn_counter = 0;
    creatures_switched = [ player ];
  }

(*HELPERS FOR TURN_BUILDER*)
let rand_move brecord =
  let rec rand_move_rec () =
    match get_move_i brecord.enemy_battler.creature (Random.int 4) with
    | None -> rand_move_rec ()
    | m -> m
  in
  rand_move_rec ()

(*HELPERS FOR BATTLE SIM FNS*)
let get_crit () =
  let x = Util.rand 16 () in
  if x = 0 then 2. else 1.

let stat_modified (stat : float) (stages : float) =
  if stages > 0. then stat *. (stages +. 2.) *. 0.5
  else if stages < 0. then stat *. 2. /. ((-1. *. stages) +. 2.)
  else stat

let damage_calc move attacker defender =
  let a = get_stats attacker.creature in
  let b = get_stats defender.creature in
  let crit_damage = get_crit () in
  let x =
    match move.category with
    | Physical ->
        stat_modified a.attack attacker.stat_changes.attack
        *. move.power
        /. stat_modified b.defense defender.stat_changes.defense
    | Special ->
        stat_modified a.sp_attack attacker.stat_changes.sp_attack
        *. move.power
        /. stat_modified b.sp_defense defender.stat_changes.sp_defense
    | _ -> 0.
  in
  let base_damage =
    ((2. *. float_of_int (get_level attacker.creature) /. 5.) +. 2.)
    *. x /. 50.
    +. 2.
  in
  let type_mod = get_type_mod move.etype defender.creature in
  let total_damage =
    base_damage
    *. get_stab_mod attacker.creature move.etype
    *. type_mod *. crit_damage
    *. (float_of_int (Util.rand 16 ()) +. 85.0)
    /. 100.0
  in
  let dmg_type =
    if type_mod = 0. then Immune
    else if type_mod < 1. then NotEffective
    else if type_mod > 1. then SuperEffective
    else Effective
  in
  (total_damage, dmg_type, crit_damage = 2.)

let active_crtr_filter crtrlist =
  List.filter
    (fun x -> if get_current_hp x > 0. then true else false)
    crtrlist

let inactive_crtr_filter crtrlist =
  List.filter
    (fun x -> if get_current_hp x > 0. then false else true)
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

let player_first brecord =
  let enemy_speed = (get_stats brecord.enemy_battler.creature).speed in
  let player_speed =
    (get_stats brecord.player_battler.creature).speed
  in
  if player_speed > enemy_speed then true
  else if player_speed < enemy_speed then false
  else if Util.rand 2 () = 1 then true
  else false

(* ==============================================================*)
(* ================ Stat Changes Handler BEGIN===================*)
(* ==============================================================*)
let stat_bound (stat_val : float) stat_name battler : float =
  let name = get_nickname battler.creature in
  if stat_val > 6. then
    add_action
      ( battler,
        MaxStat,
        name ^ "'s "
        ^ string_of_stat stat_name
        ^ " can't go any higher." )
  else if stat_val < -6. then
    add_action
      ( battler,
        MaxStat,
        name ^ "'s "
        ^ string_of_stat stat_name
        ^ " can't go any\n    lower." );
  Util.boundf stat_val (-6.) 6.

let handle_stat_changes battler stat (stages : float) =
  match stat with
  | HP -> ()
  | Attack ->
      let sc = battler.stat_changes.attack +. stages in
      battler.stat_changes.attack <- stat_bound sc stat battler
  | Defense ->
      let sc = battler.stat_changes.defense +. stages in
      battler.stat_changes.defense <- stat_bound sc stat battler
  | Sp_Attack ->
      let sc = battler.stat_changes.sp_attack +. stages in
      battler.stat_changes.sp_attack <- stat_bound sc stat battler
  | Sp_Defense ->
      let sc = battler.stat_changes.sp_defense +. stages in
      battler.stat_changes.sp_defense <- stat_bound sc stat battler
  | Speed ->
      let sc = battler.stat_changes.speed +. stages in
      battler.stat_changes.speed <- stat_bound sc stat battler

let handle_effects move attacker defender () =
  add_pp attacker.creature move.move_name (-1);
  let rec handle_effects_rec = function
    | [] -> ()
    | h :: t ->
        (let digit1 = h mod 10 in
         let digit2 = h mod 100 / 10 in
         let digit3 = h / 100 in

         match digit2 with
         | 0 | 1 ->
             let stat_lst =
               [ Attack; Defense; Sp_Attack; Sp_Defense; Speed ]
             in
             let target = if digit2 = 0 then defender else attacker in

             let stat, stages, state =
               if digit1 < 5 then
                 ( List.nth stat_lst digit1,
                   -(digit3 + 1),
                   match digit3 with
                   | 0 -> "fell!"
                   | 1 -> "sharply fell!"
                   | _ -> "severly fell!!" )
               else
                 ( List.nth stat_lst (digit1 - 5),
                   digit3 + 1,
                   match digit3 with
                   | 0 -> "rose!"
                   | 1 -> "rose sharply!"
                   | _ -> "rose drastically!" )
             in
             handle_stat_changes target stat (float_of_int stages);
             add_action
               ( target,
                 StatGain stages,
                 get_nickname target.creature
                 ^ "'s " ^ string_of_stat stat ^ " " ^ state )
         | _ -> ());
        handle_effects_rec t
  in
  Random.self_init ();
  if Random.int 100 + 1 < int_of_float move.effect_chance then
    handle_effects_rec move.effect_ids
  else ()

(* ==============================================================*)
(* ================ Stat Changes Handler END=====================*)
(* ==============================================================*)

(* ==============================================================*)
(* ================ Status Effects Handler BEGIN=================*)
(* ==============================================================*)

(* ==============================================================*)
(* ================ Status Changes Handler END=====================*)
(* ==============================================================*)
let check_active_status brecord =
  if get_current_hp brecord.player_battler.creature <= 0. then begin
    updated_player_creatures brecord;

    apply_status brecord.player_battler.creature Fainted;
    add_action
      ( brecord.player_battler,
        Fainted,
        get_nickname brecord.player_battler.creature ^ " fainted!" )
  end;
  if get_current_hp brecord.enemy_battler.creature <= 0. then begin
    updated_enemy_creatures brecord;
    apply_status brecord.enemy_battler.creature Fainted;
    add_action (brecord.enemy_battler, Fainted, "")
  end

let exec_turn attacker defender brecord =
  if
    get_status attacker.creature <> Fainted
    && get_status defender.creature <> Fainted
  then begin
    let damage_pte =
      match attacker.current_move with
      | None -> 0.0
      | Some m ->
          add_action
            ( attacker,
              ChooseMove m,
              get_nickname attacker.creature ^ " used " ^ m.move_name );
          let dmg =
            if m.power > 0. then begin
              let dmg, d_mod, crit = damage_calc m attacker defender in

              let str =
                match d_mod with
                | SuperEffective -> "It was super-effective!"
                | NotEffective -> "It was not very effective!"
                | Immune ->
                    "It does not affect "
                    ^ get_nickname defender.creature
                | _ -> ""
              in
              let max, curr = get_hp_status defender.creature in
              add_action
                (defender, Damage (dmg, max, curr, d_mod, crit), str);
              dmg
            end
            else 0.0
          in
          handle_effects m attacker defender ();
          dmg
    in
    set_current_hp defender.creature
      (get_current_hp defender.creature -. damage_pte);

    check_active_status brecord
  end

(* let exec_resolution _ = failwith "" *)

(* let execute_turn brecord = if brecord.turn_pos = Pending then if
   !player_first then exec_turn brecord.player_battler
   brecord.enemy_battler brecord else exec_turn brecord.enemy_battler
   brecord.player_battler brecord else match
   brecord.player_battler.current_move with | None -> exec_turn
   brecord.enemy_battler brecord.player_battler brecord | Some _ ->
   exec_turn brecord.player_battler brecord.enemy_battler brecord *)

(*BATTLE SIM HELPERS END so many damn*)

let rec handle_status battler status =
  match status with
  | Status s -> (
      match s with
      | Sleep t ->
          if !t - 1 > 0 then begin
            t := !t - 1;
            battler.current_move <- None
          end
          else print_endline "He woke up!"
      | Freeze -> ()
      | Paralyze -> ()
      | _ -> ())
  | Confused (_, s) ->
      print_endline "Confused!";
      handle_status battler s
  | Flinch _ -> ()
  | _ -> ()

let battle_sim brecord player_move =
  brecord.player_battler.current_move <- player_move;
  brecord.enemy_battler.current_move <- rand_move brecord;
  let first, second =
    if player_first brecord then
      (brecord.player_battler, brecord.enemy_battler)
    else (brecord.enemy_battler, brecord.player_battler)
  in
  handle_status first first.status_cond;
  exec_turn first second brecord;
  handle_status second second.status_cond;
  exec_turn second first brecord;
  (* exec_resolution brecord; *)
  ()

let run_away brecord =
  let pspeed = (get_stats brecord.player_battler.creature).speed in
  let espeed = (get_stats brecord.enemy_battler.creature).speed in
  let odds_escape =
    int_of_float
      ((pspeed *. 128. /. espeed)
      +. (30. *. float_of_int brecord.escape_attempts))
    mod 256
  in
  Random.self_init ();
  let chance = Random.int 256 in
  if pspeed > espeed || odds_escape > 255 || chance > odds_escape then
    brecord.battle_status <- Flee
  else brecord.escape_attempts <- brecord.escape_attempts + 1

let status_bonus st =
  match st with
  | Sleep _ -> 2.0
  | Freeze -> 2.0
  | Paralyze -> 1.5
  | Poison _ -> 1.5
  | Burn -> 1.5
  | _ -> 1.0

(* let rec pow a b = match b with | 0. -> 1.0 | 1. -> a | c -> a *. pow
   a (c -. 1.) *)

let capture brecord modifier =
  if brecord.battle_type = Trainer then [ false; false; false; false ]
    (*Marco print a message about not being able to capture in a trainer
      battle here :D *)
  else
    let e_currhp = get_current_hp brecord.enemy_battler.creature in
    let e_maxhp = (get_stats brecord.enemy_battler.creature).max_hp in
    let e_rate = get_catch_rate brecord.enemy_battler.creature in
    let catch_rate1 =
      ((3.0 *. e_maxhp) -. (2.0 *. e_currhp))
      *. e_rate *. modifier
      *. status_bonus (get_status brecord.enemy_battler.creature)
    in
    let catch_rate = catch_rate1 /. (3.0 *. e_maxhp) in

    let shake_probability =
      (Float.pow 2. 16. -. 1.0)
      *. Float.sqrt (Float.sqrt (catch_rate /. (Float.pow 2. 8. -. 1.)))
    in

    Random.self_init ();
    let shake1, shake2, shake3, shake4 =
      ( float_of_int (Random.int 65535),
        float_of_int (Random.int 65535),
        float_of_int (Random.int 65535),
        float_of_int (Random.int 65535) )
    in
    let catch_results =
      [
        shake_probability > shake1;
        shake_probability > shake2;
        shake_probability > shake3;
        shake_probability > shake4;
      ]
    in
    for i = 0 to 3 do
      print_endline
        ("Shake" ^ string_of_int i ^ ": "
        ^ string_of_bool (List.nth catch_results i))
    done;

    if
      List.fold_left
        (fun a b -> if b then a else false)
        true catch_results
    then brecord.battle_status <- Catch;
    catch_results
(* else if brecord.catch_attempts >= 3 then brecord.battle_status <-
   Flee else brecord.escape_attempts <- brecord.escape_attempts + 1 *)

(* ==============================================================*)
(* =============== Switchig Party Members===================*)
(* ==============================================================*)

let switching_pending = null ()

let switch_player brecord (switch_in : creature) _ =
  add_action (brecord.player_battler, Switch switch_in, "");
  (* Ui.add_first_gameplay (Animation.switch_out (get_back_sprite
     brecord.player_battler.creature) (get_back_sprite creature) true
     (get_nickname brecord.player_battler.creature) (get_nickname
     creature) (!refresh_battle (get_current_hp creature)
     (get_current_hp brecord.enemy_battler.creature) 1)); *)
  if List.mem switch_in brecord.creatures_switched = false then
    brecord.creatures_switched <-
      switch_in :: brecord.creatures_switched;
  let new_battler = generate_battler switch_in true in
  (* brecord.player_creatures <- new_party; *)
  brecord.player_battler <- new_battler
