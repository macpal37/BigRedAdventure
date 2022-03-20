open Yojson.Basic.Util

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

let create_item name =
  let json =
    Yojson.Basic.from_file "assets/util/item_list.json" |> member name
  in
  {
    name;
    id = json |> member "id" |> to_int;
    item_type =
      json |> member "type" |> to_string |> item_type_of_string;
    description = json |> member "description" |> to_string;
    cost = json |> member "cost" |> to_int;
  }

let name i = i.name
let classification i = i.item_type
let id i = i.id
