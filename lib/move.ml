open Yojson.Basic.Util
open Creature

type move_catgeory =
  | Physical
  | Special
  | Status

type move = {
  move_name : string;
  power : int;
  accuracy : float;
  mutable pp : int;
  etype : etype;
  category : move_catgeory;
  description : string;
  effect_ids : int list;
}

let string_to_category cat_string =
  match cat_string with
  | "Physical" -> Physical
  | "Special" -> Special
  | "Status" -> Status
  | _ -> Status

let parse_move name json =
  {
    move_name = name;
    power = json |> member "power" |> to_int;
    accuracy = float_of_int (json |> member "accuracy" |> to_int);
    pp = json |> member "pp" |> to_int;
    etype = string_to_etype (json |> member "type" |> to_string);
    category =
      string_to_category (json |> member "category" |> to_string);
    description = json |> member "description" |> to_string;
    effect_ids =
      json |> member "effect_ids" |> to_list |> List.map to_int;
  }

let get_move name =
  let move_json = Yojson.Basic.from_file "assets/util/move_list.json" in
  parse_move name (move_json |> member name)

let rand max () = Random.int max

let get_crit () =
  let x = rand 16 () in
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
    *. (float_of_int (rand 16 ()) +. 85.0)
    /. 100.0
  in
  total_damage

let handle_effect id move attacker defender =
  Random.init (rand 107374184 ());
  match id with
  | 0 ->
      let damage = int_of_float (damage_calc move attacker defender) in
      set_current_hp defender (get_current_hp defender - damage)
  | _ -> ()

let execute_move move attacker defender =
  let rec handle_all_effects = function
    | [] -> (attacker, defender)
    | h :: t ->
        handle_effect h move attacker defender;
        handle_all_effects t
  in
  handle_all_effects move.effect_ids
