module KeyMapping = Stdlib.Map.Make (Char)
open KeyMapping

let key_map = ref empty
let keys = ref []

let keymap_init list_of_keys =
  keys.contents <- list_of_keys;
  key_map.contents <- empty;
  let rec mapping_keys k =
    match k with
    | [] -> ()
    | h :: t ->
        key_map.contents <- key_map.contents |> add h false;
        mapping_keys t
  in
  mapping_keys list_of_keys

let reset_keys () =
  key_map.contents <-
    map (fun a -> if a = true then false else a) key_map.contents

let key_press c =
  reset_keys ();
  key_map.contents <- add c true key_map.contents

let get_key c = find c key_map.contents
let e () = find 'e' key_map.contents
let q () = find 'q' key_map.contents
let w () = find 'w' key_map.contents
let a () = find 'a' key_map.contents
let s () = find 's' key_map.contents
let d () = find 'd' key_map.contents
let f () = find 'f' key_map.contents
let c () = find 'c' key_map.contents
