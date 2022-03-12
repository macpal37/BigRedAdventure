open Creature

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
