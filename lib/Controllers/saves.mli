type save_preview = {
  name : string;
  money : int;
  time : int;
  id : int;
}

val get_previews : unit -> int -> save_preview option
val new_game : int -> string -> unit
val load_game : save_preview -> unit
val save_game : unit -> unit
