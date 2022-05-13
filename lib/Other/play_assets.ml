open Yojson.Basic.Util

type loaded_assets = {
  maps : (string, Map.t) Hashtbl.t;
  items : (string, Item.item) Hashtbl.t;
      (* creatures : (string, Creature.creature) Hashtbl.t; *)
}

let assets_ref = ref None

let assets _ =
  match !assets_ref with
  | Some a -> a
  | None -> failwith "Assets not loaded yet"

let get_map s = Hashtbl.find (assets ()).maps s
let get_item s = Hashtbl.find (assets ()).items s

let load_maps _ =
  let t = Hashtbl.create 16 in
  Yojson.Basic.from_file "assets/maps/maps.json"
  |> to_list
  |> List.iter (fun j ->
         let file = j |> to_string in
         Hashtbl.add t file (Map.load_map file));
  t

let load_items _ =
  let t = Hashtbl.create 16 in
  Yojson.Basic.from_file "assets/util/item_list.json"
  |> to_assoc
  |> List.iter (fun (name, j) ->
         Hashtbl.add t name (Item.create_item name j));
  t

let load _ =
  assets_ref := Some { maps = load_maps (); items = load_items () }
