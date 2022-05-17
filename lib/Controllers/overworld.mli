(** The executor of the overworld

    This module orchestrates the gameplay in the overworld*)

val tile_dpi : int
(** The dpi of the tile sprite*)

val load_assets : unit -> unit
(** Loads the assets necessary for the module to function*)

val run_overworld : Saves.save_preview -> unit
(** [run_overworld _] runs the overworld. *)

(* val respond_to_interact : bool -> int * int option -> unit *)
