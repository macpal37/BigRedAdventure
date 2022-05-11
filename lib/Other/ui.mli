type render_view
type draw_func = unit -> unit

type layer =
  | Background
  | Gameplay
  | Foreground

val add_first_foreground : draw_func -> unit
val add_first_gameplay : draw_func -> unit
val add_first_background : draw_func -> unit
val clear_ui : layer -> unit
val clear_all : unit -> unit
val add_last_foreground : draw_func -> unit
val add_last_gameplay : draw_func -> unit
val add_last_background : draw_func -> unit
val update_background : unit -> unit
val update_gameplay : unit -> unit
val update_foreground : unit -> unit
val update_all : unit -> unit
