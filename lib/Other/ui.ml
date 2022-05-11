open Draw

type draw_func = unit -> unit

(* FALSE: Continue Animation TRUE: End Animation *)

type layer =
  | Background
  | Gameplay
  | Foreground

type render_view = {
  mutable foreground : draw_func list;
  mutable gameplay : draw_func list;
  mutable background : draw_func list;
}

let renderer = ref { foreground = []; gameplay = []; background = [] }

let clear_all () =
  !renderer.background <- [];
  !renderer.gameplay <- [];
  !renderer.foreground <- []

let clear_ui layer =
  match layer with
  | Background -> !renderer.background <- []
  | Gameplay -> !renderer.gameplay <- []
  | Foreground -> !renderer.foreground <- []

let add_first_foreground draw_func =
  !renderer.foreground <- draw_func :: !renderer.foreground

let add_first_gameplay draw_func =
  !renderer.gameplay <- draw_func :: !renderer.gameplay

let add_first_background draw_func =
  !renderer.background <- draw_func :: !renderer.background

let add_last_foreground draw_func =
  !renderer.foreground <- !renderer.foreground @ [ draw_func ]

let add_last_gameplay draw_func =
  !renderer.gameplay <- !renderer.gameplay @ [ draw_func ]

let add_last_background draw_func =
  !renderer.background <- !renderer.background @ [ draw_func ]

let rec draw_all_background = function
  | [] -> ()
  | h :: t ->
      h ();
      draw_all_background t

let rec draw_all_gameplay = function
  | [] -> ()
  | h :: t ->
      h ();
      draw_all_gameplay t

let rec draw_all_foreground = function
  | [] -> ()
  | h :: t ->
      h ();
      draw_all_foreground t

let update_background () =
  draw_all_background !renderer.background;
  present ()

let update_gameplay () =
  draw_all_gameplay !renderer.gameplay;
  present ()

let update_foreground () =
  draw_all_foreground !renderer.foreground;
  present ()

let update_all () =
  draw_all_background !renderer.background;
  !renderer.background <- [];

  draw_all_gameplay !renderer.gameplay;
  !renderer.gameplay <- [];

  draw_all_foreground !renderer.foreground;
  !renderer.foreground <- [];

  present ()
