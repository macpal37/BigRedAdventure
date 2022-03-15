let key : char option ref = ref None
let nil = char_of_int 0

let key_char _ =
  match !key with
  | Some k -> k
  | None -> nil

let key_option _ = !key

let poll _ =
  key :=
    (fun _ ->
      if Graphics.key_pressed () then Some (Graphics.read_key ())
      else None)
      ()
