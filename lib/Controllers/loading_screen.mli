val load : unit -> unit
(** Starts the loading process in a separate thread. Should not be
    called more than once!*)

val await : unit -> unit
(** Shows the loading screen until loading has completed. Returns
    immediately if the loading process is complete. Should be called
    only after calling [load _]*)

val load_assets : unit -> unit
(** Load assets. Directly calling this function may be useful for
    debugging/testing*)
