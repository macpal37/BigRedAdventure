open Item

exception Insufficient of int

module ItemTypeMap = Stdlib.Map.Make (struct
  type t = Item.item_type

  let compare = compare
end)

module ItemMap = Stdlib.Map.Make (struct
  type t = Item.item

  let compare = compare
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

let list_items b = ItemMap.to_seq !b

let add b i ?(c = 1) =
  b :=
    ItemMap.update i
      (fun k ->
        match k with
        | Some v -> Some (v + c)
        | None -> Some c)
      !b

let consume b i ?(c = 1) =
  b :=
    ItemMap.update i
      (fun k ->
        match k with
        | Some v ->
            let new_count = v - c in
            if new_count >= 0 then Some new_count
            else raise (Insufficient v)
        | None -> raise (Insufficient 0))
      !b
