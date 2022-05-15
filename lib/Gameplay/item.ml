open Yojson.Basic.Util

let num_item_types = 4

type item_type =
  | Key
  | Ball
  | Medicine
  | Misc

type item = {
  name : string;
  item_type : item_type;
  description : string;
  cost : int;
  id : int;
}

let string_of_item_type item_type =
  match item_type with
  | Key -> "Key"
  | Ball -> "Ball"
  | Medicine -> "Medicine"
  | Misc -> "Misc"

let item_type_of_string item_string =
  match item_string with
  | "Key" -> Key
  | "Ball" -> Ball
  | "Medicine" -> Medicine
  | "Misc" -> Misc
  | _ -> Misc

let new_item name item_type id description cost =
  { name; item_type; description; cost; id }

let create_item name json =
  {
    name;
    id = json |> member "id" |> to_int;
    item_type =
      json |> member "type" |> to_string |> item_type_of_string;
    description = json |> member "description" |> to_string;
    cost = json |> member "cost" |> to_int;
  }

let get_name i = i.name
let get_type i = i.item_type
let get_id i = i.id
let get_description i = i.description
let get_cost i = i.cost
