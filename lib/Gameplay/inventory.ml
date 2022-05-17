open Yojson.Basic.Util

exception Insufficient of int

module ItemTypeMap = Stdlib.Map.Make (struct
  type t = Item.item_type

  let compare = compare
end)

module ItemMap = Stdlib.Map.Make (struct
  type t = Item.item

  let compare e1 e2 = Item.get_id e1 - Item.get_id e2
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
            if new_count > 0 then Some new_count
            else if new_count = 0 then None
            else raise (Insufficient v)
        | None -> raise (Insufficient 0))
      !b

let add_item (i : inventory) (item : Item.item) =
  let bag = get_bag i (Item.get_type item) in
  add bag item

let consume_item (i : inventory) (item : Item.item) =
  let bag = get_bag i (Item.get_type item) in
  consume bag item

let serialize i =
  `Assoc
    ([
       ("Key", Item.Key);
       ("Ball", Ball);
       ("Medicine", Medicine);
       ("Misc", Misc);
     ]
    |> List.map (fun (ts, tv) ->
           ( ts,
             `List
               (list_items (get_bag i tv)
               |> List.map (fun (i, c) ->
                      `List [ `String (Item.get_name i); `Int c ])) )))

let deserialize j =
  let i = new_inventory () in
  [
    ("Key", Item.Key);
    ("Ball", Ball);
    ("Medicine", Medicine);
    ("Misc", Misc);
  ]
  |> List.iter (fun (ts, tv) ->
         let b = get_bag i tv in
         j |> member ts |> to_list
         |> List.iter (fun l ->
                match l |> to_list with
                | n :: c :: _ ->
                    add b ~count:(c |> to_int)
                      (Item.get_item (n |> to_string))
                | _ -> failwith "Deserialization error"));
  i
