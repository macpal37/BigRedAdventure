(** Module for showing the loading screen while waiting for a concurrent
    task to finish*)

val loading_label : string
(** The label corresponding to loading assets*)

val load_assets : unit -> string
(** Runs the loading process and returns [loading_label].*)

val submit_job : (unit -> string) -> unit
(** [submit_job f] prepares a thread that runs [f]. Only one job can be
    run at a time. The string returned by [f] will be added to the set
    of labels of completed jobs.*)

val await : string list -> unit
(** [await j] shows the loading screen until all the labels in [j] are
    in the set of labels of completed jobs. Returns immediately if all
    labels are already in the set of labels of completed jobs.*)

val clear_labels : unit -> unit
(** Clears the set of labels of completed jobs. Blocks if a job is
    currently running. Care should be taken that clearing the set does
    not interfere with other calls.*)
