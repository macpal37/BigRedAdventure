(** The module that handles the queue of draw functions needed to drawn
    and rendered onto the screen.*)

type render_view
(**Abstract type that stores all the draw function queues for each
   layer.*)

type draw_func = unit -> unit
(** [draw_func] type that reprsents a draw function*)

(** [layer] drawing layer. Determines order of how things are drawn.*)
type layer =
  | Background  (** First layer: Back*)
  | Gameplay  (** Second layer: Mid *)
  | Foreground  (** Third layer: Front *)

val add_first_foreground : draw_func -> unit
(** [add_first_foreground df] adds the draw function [df] onto the
    foreground layer.*)

val add_first_gameplay : draw_func -> unit
(** [add_first_gameplay df] adds the draw function [df] onto the
    gameplay layer.*)

val add_first_background : draw_func -> unit
(** [add_first_gameplay df] adds the draw function [df] onto the
    background layer.*)

val add_last_foreground : draw_func -> unit
(** [add_last_foreground df] adds the draw function [df] onto the
    foregournd layer.*)

val add_last_gameplay : draw_func -> unit
(** [add_last_gameplay df] adds the draw function [df] onto the gameplay
    layer.*)

val add_last_background : draw_func -> unit
(** [add_last_background df] adds the draw function [df] onto the
    background layer.*)

val clear_all : unit -> unit
(** [clear_all ()] Clears queue of draw functions for all layers.*)

val update_all : unit -> unit
(** [update_all ()] draws all the draw functions queued in order then
    cears the queues..*)
