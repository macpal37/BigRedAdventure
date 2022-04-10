open Draw

let tail () = ()

type layer =
  | Background
  | Gameplay
  | Foreground

type render_view = {
  mutable foreground : (unit -> unit) list;
  mutable gameplay : (unit -> unit) list;
  mutable background : (unit -> unit) list;
}

let renderer =
  ref
    {
      foreground = [ tail ];
      gameplay = [ tail ];
      background = [ tail ];
    }

let clear_all () =
  renderer.contents.background <- [];
  renderer.contents.gameplay <- [];
  renderer.contents.foreground <- []

let clear_ui layer =
  match layer with
  | Background -> renderer.contents.background <- []
  | Gameplay -> renderer.contents.gameplay <- []
  | Foreground -> renderer.contents.foreground <- []

let add_first_foreground draw_func =
  renderer.contents.foreground <-
    draw_func :: renderer.contents.foreground

let add_first_gameplay draw_func =
  renderer.contents.gameplay <- draw_func :: renderer.contents.gameplay

let add_first_background draw_func =
  renderer.contents.background <-
    draw_func :: renderer.contents.background

let add_last_foreground draw_func =
  renderer.contents.foreground <-
    renderer.contents.foreground @ [ draw_func ]

let add_last_gameplay draw_func =
  renderer.contents.gameplay <-
    renderer.contents.gameplay @ [ draw_func ]

let add_last_background draw_func =
  renderer.contents.background <-
    renderer.contents.background @ [ draw_func ]

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
  sync true ();
  draw_all_background renderer.contents.background;
  sync false ()

let update_gameplay () =
  sync true ();
  draw_all_gameplay renderer.contents.gameplay;
  sync false ()

let update_foreground () =
  sync true ();
  draw_all_foreground renderer.contents.foreground;
  sync false ()

let update_all () =
  sync false ();
  draw_all_background renderer.contents.background;
  renderer.contents.background <- [];

  draw_all_gameplay renderer.contents.gameplay;
  renderer.contents.gameplay <- [];

  draw_all_foreground renderer.contents.foreground;
  renderer.contents.foreground <- [];
  sync true ()
