open Yojson.Basic.Util

type move = {
  name : string;
  power : int;
  accuracy : int;
  mutable pp : int;
  effect_ids : int list;
  description : string;
}

let parse_move name json =
  {
    name;
    power = json |> member "power" |> to_int;
    accuracy = json |> member "accuracy" |> to_int;
    pp = json |> member "pp" |> to_int;
    effect_ids =
      json |> member "effect_ids" |> to_list |> List.map to_int;
    description = json |> member "description" |> to_string;
  }

let get_move name =
  let move_json = Yojson.Basic.from_file "assets/util/move_list" in
  parse_move name (move_json |> member name)

let execute_move name attacker defender =
  if name = "" then (attacker, defender) else (attacker, defender)
