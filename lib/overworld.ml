let run_tick c =
  let _ =
    match c with
    | Some k -> if k = 'A' then () else ()
    | None -> ()
  in
  raise (Failure "dummy")
