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

let new_game _ _ =
  State.adhoc_init ();
  Overworld.run_overworld ()

let load_game _ =
  State.adhoc_init ();
  Overworld.run_overworld ()

let save_game _ =
  if false then
    write_preview { name = "RED"; money = 420; time = 69; id = 0 }
