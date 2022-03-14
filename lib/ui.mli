type render_view
(* type draw_mode = | Background | Gameplay | Foreground *)

val add_first_foreground : (unit -> unit) -> unit
val add_first_gameplay : (unit -> unit) -> unit
val add_first_background : (unit -> unit) -> unit

(* val last_first_foreground : (unit -> unit) -> unit val
   last_first_gameplay : (unit -> unit) -> unit val
   last_first_background : (unit -> unit) -> unit *)

val update_background : unit -> unit
val update_gameplay : unit -> unit
val update_foreground : unit -> unit
val update_all : unit -> unit
