(** Module for handling save functionality*)

type save_preview = {
  name : string;
  money : int;
  time : int;
  id : int;
}
(** Record with identifying attributes of a save*)

val get_previews : unit -> int -> save_preview option
(** [get_previews _] loads save previews and returns a function that
    returns the save preview, if any, corresponding to an index*)

val save_game : save_preview -> unit
(** Save the game at its current state to the save corresponding to the
    save preview*)

val load_game : int -> unit
(** [load_game i] sets the current state to the save with index [i]*)
