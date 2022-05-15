let key : Sdlkeycode.t list ref = ref []
(* let nil = Sdlkeycode.Unknown *)

(* let key_char _ = match !key with | Some k -> k | None ->
   Sdlkeycode.Unknown *)
type control_key =
  | Up
  | Down
  | Left
  | Right
  | Action
  | Back
  | Start
  | Select
  | Debug
  | NoKey

let ( => ) (sdl : Sdlkeycode.t) (ctrl : control_key) : bool =
  let c =
    match sdl with
    | Sdlkeycode.Up -> Up
    | Sdlkeycode.Down -> Down
    | Sdlkeycode.Left -> Left
    | Sdlkeycode.Right -> Right
    | Sdlkeycode.X | Sdlkeycode.Return | Sdlkeycode.E -> Action
    | Sdlkeycode.Z | Sdlkeycode.Q | Sdlkeycode.Backspace -> Back
    | Sdlkeycode.Slash -> Start
    | Sdlkeycode.Period -> Select
    | Sdlkeycode.D | Sdlkeycode.S -> Debug
    | _ -> NoKey
  in
  c = ctrl

let get_ctrl_key (sdl : Sdlkeycode.t) : control_key =
  match sdl with
  | Sdlkeycode.Up -> Up
  | Sdlkeycode.Down -> Down
  | Sdlkeycode.Left -> Left
  | Sdlkeycode.Right -> Right
  | Sdlkeycode.X -> Action
  | Sdlkeycode.Z -> Back
  | Sdlkeycode.Slash -> Start
  | Sdlkeycode.D | Sdlkeycode.S -> Debug
  | Sdlkeycode.Period -> Select
  | _ -> NoKey

let get_ctrl_option (sdl : Sdlkeycode.t option) : control_key option =
  match sdl with
  | Some k -> Some (get_ctrl_key k)
  | None -> None

let poll_key_option _ =
  match !key with
  | h :: _ -> Some h
  | [] -> None

let pop_key_option _ =
  let k = !key in
  key := [];
  match k with
  | h :: _ -> Some h
  | [] -> None

let rec input_poll _ =
  match Sdlevent.poll_event () with
  | Some (Sdlevent.Quit _) ->
      Sdl.quit ();
      exit 0
  | Some (KeyDown e) ->
      if not (List.mem e.keycode !key) then key := e.keycode :: !key;
      input_poll ()
  | Some (KeyUp e) ->
      key := List.filter (fun x -> x <> e.keycode) !key;
      input_poll ()
  | None -> ()
  | _ -> input_poll ()

let sleep time () =
  Unix.sleepf time;
  input_poll ()
