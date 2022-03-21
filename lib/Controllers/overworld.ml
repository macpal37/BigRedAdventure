let draw _ = ()

let run_tick _ =
  (match Input.key_option () with
  | Some 'w' -> ()
  | Some 'a' -> ()
  | Some 's' -> ()
  | Some 'd' -> ()
  | Some k -> ignore k
  | None -> ());
  draw ()
