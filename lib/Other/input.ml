let key : Sdlkeycode.t option ref = ref None
(* let nil = Sdlkeycode.Unknown *)

(* let key_char _ = match !key with | Some k -> k | None ->
   Sdlkeycode.Unknown *)

let poll_key_option _ = !key

let pop_key_option _ =
  let k = !key in
  key := None;
  k

let rec input_poll _ =
  match Sdlevent.poll_event () with
  | Some (Sdlevent.Quit _) ->
      Sdl.quit ();
      exit 0
  | Some (KeyDown e) ->
      key := Some e.keycode;
      input_poll ()
  | Some (KeyUp _) ->
      key := None;
      input_poll ()
  | None -> ()
  | _ -> input_poll ()

let sleep time () =
  Unix.sleepf time;
  input_poll ()
