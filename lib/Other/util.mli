val bound : int -> int -> int -> int
val rand : int -> unit -> int
val captilize_all_string : string -> string

type point = {
  mutable x : int;
  mutable y : int;
}

val new_point : unit -> point
