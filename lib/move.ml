open Yojson.Basic.Util
open Creature

type move_catgeory =
  | Physical
  | Special
  | Status

type move = {
  name : string;
  power : int;
  accuracy : int;
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
    name;
    power = json |> member "power" |> to_int;
    accuracy = json |> member "accuracy" |> to_int;
    pp = json |> member "pp" |> to_int;
    etype = string_to_etype (json |> member "type" |> to_string);
    category =
      string_to_category (json |> member "category" |> to_string);
    description = json |> member "description" |> to_string;
    effect_ids =
      json |> member "effect_ids" |> to_list |> List.map to_int;
  }

let get_move name =
  let move_json = Yojson.Basic.from_file "assets/util/move_list" in
  parse_move name (move_json |> member name)

let execute_move name attacker defender =
  if name = "" then (attacker, defender) else (attacker, defender)
