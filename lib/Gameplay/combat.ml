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
  mutable status_effect : status;
  mutable active : bool;
  is_player : bool;
}

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
  mutable turn_pos : turn_status;
  mutable creatures_switched : creature list;
}

let refresh_battle = ref (fun _ _ _ () -> ())
let health_bar = ref (fun _ _ _ _ _ () -> ())
let player_first = ref false

let empty_battle =
  {
    player_creatures = [];
    enemy_creatures = [];
    battle_type = Wild;
    battle_status = Ongoing;
    catch_attempts = 0;
    escape_attempts = 0;
    player_battler =
      {
        creature = create_creature "missingno" 1;
        current_move = None Fainted;
        stat_changes = empty_stats;
        status_effect = Healthy;
        is_player = true;
        active = false;
      };
    enemy_battler =
      {
        creature = create_creature "missingno" 1;
        current_move = None Fainted;
        stat_changes = empty_stats;
        status_effect = Healthy;
        is_player = false;
        active = false;
      };
    turn_counter = 0;
    turn_pos = Choosing;
    creatures_switched = [];
  }

let generate_battler creature player =
  {
    creature;
    current_move = None (get_status creature);
    stat_changes = empty_stats;
    status_effect = get_status creature;
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
    turn_pos = Choosing;
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
    turn_pos = Choosing;
    creatures_switched = [ player ];
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
  let crit_damage = get_crit () in
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
  let type_mod = get_type_mod move.etype defender.creature in
  let total_damage =
    base_damage
    *. get_stab_mod attacker.creature move.etype
    *. type_mod *. crit_damage
    *. (float_of_int (Util.rand 16 ()) +. 85.0)
    /. 100.0
  in
  (total_damage, crit_damage = 2., type_mod)

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
  let enemy_speed = (get_stats brecord.enemy_battler.creature).speed in
  let player_speed =
    (get_stats brecord.player_battler.creature).speed
  in
  if player_speed > enemy_speed then true
  else if player_speed < enemy_speed then false
  else if Random.int 2 = 1 then true
  else false

(* ==============================================================*)
(* ================ Stat Changes Handler BEGIN===================*)
(* ==============================================================*)
let stat_bound stat_val name stat_name =
  if stat_val > 6 then begin
    Ui.add_last_gameplay
      (draw_text
         (name ^ "'s "
         ^ string_of_stat stat_name
         ^ " can't go any higher.")
         40 true false);
    false
  end
  else if stat_val < -6 then begin
    Ui.add_last_gameplay
      (draw_text (name ^ "'s ATK can't go any lower.") 40 true false);
    false
  end
  else true

let handle_stat_changes battler stat stages =
  if battler.is_player then
    if stages < 0 then
      Ui.add_last_gameplay
        (Animation.lower_stat_effect
           (get_back_sprite battler.creature)
           true)
    else
      Ui.add_last_gameplay
        (Animation.raise_stat_effect
           (get_back_sprite battler.creature)
           true)
  else if stages < 0 then
    Ui.add_last_gameplay
      (Animation.lower_stat_effect
         (get_front_sprite battler.creature)
         false)
  else
    Ui.add_last_gameplay
      (Animation.raise_stat_effect
         (get_front_sprite battler.creature)
         false);

  match stat with
  | HP -> ()
  | Attack ->
      let stat_change = battler.stat_changes.attack + stages in
      if stat_bound stat_change (get_nickname battler.creature) stat
      then
        battler.stat_changes <-
          { battler.stat_changes with attack = stat_change }
  | Defense ->
      let stat_change = battler.stat_changes.attack + stages in
      if stat_bound stat_change (get_nickname battler.creature) stat
      then
        battler.stat_changes <-
          { battler.stat_changes with defense = stat_change }
  | Sp_Attack ->
      let stat_change = battler.stat_changes.attack + stages in
      if stat_bound stat_change (get_nickname battler.creature) stat
      then
        battler.stat_changes <-
          { battler.stat_changes with sp_attack = stat_change }
  | Sp_Defense ->
      let stat_change = battler.stat_changes.attack + stages in
      if stat_bound stat_change (get_nickname battler.creature) stat
      then
        battler.stat_changes <-
          { battler.stat_changes with sp_defense = stat_change }
  | Speed ->
      let stat_change = battler.stat_changes.attack + stages in
      if stat_bound stat_change (get_nickname battler.creature) stat
      then
        battler.stat_changes <-
          { battler.stat_changes with speed = stat_change }

let handle_effects move attacker defender () =
  add_pp attacker.creature move.move_name (-1);

  (* Ui.add_last_gameplay (clear_text Draw.battle_bot); *)
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

             Ui.add_last_gameplay
               (draw_text
                  (get_nickname target.creature
                  ^ "'s " ^ string_of_stat stat ^ " " ^ state)
                  40 true true);
             handle_stat_changes target stat stages
         | _ -> ());
        handle_effects_rec t
  in
  Random.self_init ();
  if Random.int 100 + 1 < move.effect_chance then
    handle_effects_rec move.effect_ids
  else ()

(* ==============================================================*)
(* ================ Stat Changes Handler END=====================*)
(* ==============================================================*)

(* ==============================================================*)
(* ================ Status Effects Handler BEGIN=================*)
(* ==============================================================*)

(* ==============================================================*)
(* ================ Stat Changes Handler END=====================*)
(* ==============================================================*)
let exec_turn attacker defender brecord =
  if attacker.active && defender.active then begin
    let damage_pte =
      match attacker.current_move with
      | None _ -> 0.0
      | Move m ->
          if m <> empty_move then begin
            Ui.add_last_gameplay (fun () ->
                Graphics.auto_synchronize false);
            Ui.add_last_gameplay
              (draw_text
                 (get_nickname attacker.creature
                 ^ " used " ^ m.move_name)
                 40 true false);

            let dmg =
              if m.power > 0 then begin
                if attacker.creature = brecord.player_battler.creature
                then
                  Ui.add_last_gameplay
                    (Draw.damage_render
                       (get_front_sprite defender.creature)
                       false
                       (!refresh_battle
                          (get_current_hp attacker.creature)
                          (get_current_hp defender.creature)
                          0))
                else
                  Ui.add_last_gameplay
                    (Draw.damage_render
                       (get_back_sprite defender.creature)
                       true
                       (!refresh_battle
                          (get_current_hp defender.creature)
                          (get_current_hp attacker.creature)
                          1));

                let damage, crit, mult =
                  damage_calc m attacker defender
                in

                if mult <> 1.0 then
                  if mult < 1. then
                    Ui.add_last_gameplay
                      (draw_text "It was not very effective!" 40 true
                         true)
                  else if mult > 1. then
                    Ui.add_last_gameplay
                      (draw_text "It was super-effective!" 40 true true)
                  else if mult = 0.0 then
                    Ui.add_last_gameplay
                      (draw_text
                         ("It does not affect "
                         ^ get_nickname defender.creature
                         ^ ".")
                         40 true true);
                Ui.add_last_gameplay
                  (!health_bar
                     (get_stat attacker.creature HP)
                     (get_current_hp attacker.creature)
                     (get_current_hp attacker.creature)
                     attacker.is_player true);
                Ui.add_last_gameplay
                  (!health_bar
                     (get_stat defender.creature HP)
                     (get_current_hp defender.creature)
                     (get_current_hp defender.creature
                     - int_of_float damage)
                     defender.is_player true);
                if crit then
                  Ui.add_last_gameplay
                    (draw_text "Critical Hit!" 40 true false);
                damage
              end
              else 0.0
            in
            handle_effects m attacker defender ();
            dmg
          end
          else 0.0
    in

    set_current_hp defender.creature
      (get_current_hp defender.creature - int_of_float damage_pte);

    if get_current_hp defender.creature > 0 then ()
    else if defender.creature = brecord.player_battler.creature then
      updated_player_creatures brecord
    else updated_enemy_creatures brecord
  end
  else begin
    if attacker.active = false then
      Ui.add_last_gameplay
        (draw_text
           (get_nickname attacker.creature ^ " fainted!")
           40 true true);
    if defender.active = false then
      Ui.add_last_gameplay
        (draw_text
           (get_nickname defender.creature ^ " fainted!")
           40 true true)
  end

let execute_turn brecord =
  if brecord.turn_pos = Pending then
    if !player_first then
      exec_turn brecord.player_battler brecord.enemy_battler brecord
    else exec_turn brecord.enemy_battler brecord.player_battler brecord
  else
    match brecord.player_battler.current_move with
    | None _ ->
        exec_turn brecord.enemy_battler brecord.player_battler brecord
    | Move _ ->
        exec_turn brecord.player_battler brecord.enemy_battler brecord

let check_active_status brecord =
  if get_current_hp brecord.player_battler.creature <= 0 then begin
    brecord.player_battler.active <- false;
    apply_status brecord.player_battler.creature Fainted
  end;
  if get_current_hp brecord.enemy_battler.creature <= 0 then begin
    brecord.enemy_battler.active <- false;
    apply_status brecord.enemy_battler.creature Fainted
  end

(*BATTLE SIM HELPERS END so many damn*)

let battle_sim_fh brecord =
  let res = player_faster brecord in
  player_first := res;
  brecord.turn_pos <- Pending;

  execute_turn brecord;

  if res then begin
    brecord.player_battler.current_move <-
      None (get_status brecord.player_battler.creature);
    brecord.turn_pos <- Halfway
  end
  else begin
    brecord.enemy_battler.current_move <-
      None (get_status brecord.enemy_battler.creature);
    brecord.turn_pos <- Halfway
  end;
  check_active_status brecord

let battle_sim_sh brecord =
  if brecord.battle_status <> Victory then begin
    execute_turn brecord;

    brecord.player_battler.current_move <-
      None (get_status brecord.player_battler.creature);

    brecord.enemy_battler.current_move <-
      None (get_status brecord.enemy_battler.creature);
    brecord.turn_pos <- Finished
  end;
  check_active_status brecord

(*IGNORE THESE FOR NOW, WILL POLISH IMPLEMENTATION LATER*)

let run_away brecord =
  let pspeed = (get_stats brecord.player_battler.creature).speed in
  let espeed = (get_stats brecord.enemy_battler.creature).speed in
  let odds_escape =
    ((pspeed * 128 / espeed) + (30 * brecord.escape_attempts)) mod 256
  in
  Random.self_init ();
  let chance = Random.int 256 in
  if pspeed > espeed || odds_escape > 255 || chance > odds_escape then
    brecord.battle_status <- Flee
  else brecord.escape_attempts <- brecord.escape_attempts + 1

let status_bonus st =
  match st with
  | Sleep _ -> 2.0
  | Freeze _ -> 2.0
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
    let e_currhp =
      float_of_int (get_current_hp brecord.enemy_battler.creature)
    in
    let e_maxhp =
      float_of_int (get_stats brecord.enemy_battler.creature).max_hp
    in
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

let switching_pending = ref Option.None

let switch_player brecord creature _ =
  Ui.add_first_gameplay
    (Animation.switch_out
       (get_back_sprite brecord.player_battler.creature)
       (get_back_sprite creature)
       true
       (get_nickname brecord.player_battler.creature)
       (get_nickname creature)
       (!refresh_battle
          (get_current_hp creature)
          (get_current_hp brecord.enemy_battler.creature)
          1));
  if List.mem creature brecord.creatures_switched = false then
    brecord.creatures_switched <- creature :: brecord.creatures_switched;
  let new_battler = generate_battler creature true in
  (* brecord.player_creatures <- new_party; *)
  brecord.player_battler <- new_battler
