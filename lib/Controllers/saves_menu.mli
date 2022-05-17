(** Module for showing the saves menu*)

val load_assets : unit -> unit
(** Loads the assets necessary for the module to function*)

val new_game : unit -> unit
(** Run the saves menu with the context of starting a fresh new game*)

val load_game : unit -> unit
(** Run the saves menu with the context of loading a game*)

val launch_new_game : int -> string -> unit
(** Debugging utility for immediately launching a new game*)
