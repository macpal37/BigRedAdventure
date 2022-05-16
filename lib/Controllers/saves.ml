open Yojson.Basic.Util

type save_preview = {
  name : string;
  money : int;
  time : int;
  id : int;
}

let preview_filepath i = ".saves/" ^ string_of_int i ^ "preview.json"

let write_preview s =
  Yojson.to_file (preview_filepath s.id)
    (`Assoc
      [
        ("name", `String s.name);
        ("money", `Int s.money);
        ("time", `Int s.time);
        ("id", `Int s.id);
      ])

let read_preview i =
  let s = preview_filepath i in
  if Sys.file_exists s then
    let j = Yojson.Basic.from_file s in
    Some
      {
        name = j |> member "name" |> to_string;
        money = j |> member "money" |> to_int;
        time = j |> member "time" |> to_int;
        id = j |> member "id" |> to_int;
      }
  else None

let get_previews _ =
  Util.list_index_fun ([ 0; 1; 2 ] |> List.map (fun i -> read_preview i))

let save_game save_preview =
  write_preview save_preview;
  let save_file_path =
    ".saves/" ^ string_of_int save_preview.id ^ ".json"
  in
  Yojson.Basic.to_file save_file_path
    (`Assoc
      [
        ("player", Player.serialize (State.player ()));
        ("map", `String (Map.get_name (State.map ())));
        ( "entities",
          `Assoc
            (Hashtbl.fold
               (fun s m l ->
                 ( s,
                   `List
                     (ignore m;
                      []
                      (*Map.get_entities m*)
                      |> List.map (fun ((x, y), r) ->
                             `List
                               [
                                 `Int x;
                                 `Int y;
                                 `Int
                                   (ignore r;
                                    69)
                                 (*Entity.get_state r*);
                               ])) )
                 :: l)
               (Map.get_maps ()) []) );
      ])

let load_game id =
  let save_file_path = ".saves/" ^ string_of_int id ^ ".json" in
  let j = Yojson.Basic.from_file save_file_path in
  State.set_player (j |> member "player" |> Player.deserialize);
  State.set_map (j |> member "map" |> to_string |> Map.get_map);
  j |> member "entities" |> to_assoc
  |> List.iter (fun (s, d) ->
         let m = Map.get_map s in
         d |> to_list
         |> List.iter (fun d ->
                match d |> to_list with
                | x :: y :: s :: _ ->
                    (*Entity.set_state*)
                    ignore
                      ((*Map.get_entities m*)
                       (ignore m;
                        [])
                      |> List.assoc (x |> to_int, y |> to_int));
                    ignore (s |> to_int)
                | _ -> failwith "Loading error"))
