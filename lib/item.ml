type item_type =
  | Key
  | Ball
  | Medicine
  | Misc

type item = {
  name : string;
  item_type : item_type;
  id : int;
}

let new_item name item_type id = { name; item_type; id }
let name i = i.name
let classification i = i.item_type
let id i = i.id
