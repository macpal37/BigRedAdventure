type save_preview = {
  name : string;
  money : int;
  time : int;
  id : int;
}

val get_previews : unit -> int -> save_preview option
val save_game : save_preview -> unit
