exception Insufficient of int

module ItemTypeMap = Stdlib.Map.Make (struct
  type t = Item.item_type

  let compare = compare
end)

module ItemMap = Stdlib.Map.Make (struct
  type t = Item.item

  let compare e1 e2 = Item.id e1 - Item.id e2
end)

type bag = int ItemMap.t ref
type inventory = bag ItemTypeMap.t ref

let new_inventory _ = ref ItemTypeMap.empty

let get_bag (i : inventory) (t : Item.item_type) =
  match ItemTypeMap.find_opt t !i with
  | Some b -> b
  | None ->
      let b = ref ItemMap.empty in
      i := ItemTypeMap.add t b !i;
      b

let list_items b = ItemMap.bindings !b

let add b ?(count = 1) i =
  b :=
    ItemMap.update i
      (fun k ->
        match k with
        | Some v -> Some (v + count)
        | None -> Some count)
      !b

let consume b ?(count = 1) i =
  b :=
    ItemMap.update i
      (fun k ->
        match k with
        | Some v ->
            let new_count = v - count in
            if new_count >= 0 then Some new_count
            else raise (Insufficient v)
        | None -> raise (Insufficient 0))
      !b
