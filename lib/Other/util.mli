val bound : int -> int -> int -> int
val rand : int -> unit -> int
val captilize_all_string : string -> string

type point = {
  mutable x : int;
  mutable y : int;
}

val new_point : unit -> point

val list_index_fun : 'a list -> int -> 'a
(** [list_index_fun l i] is a the element at index [i] in list [i]. Call
    [list_index_fun l] to build a convenient function that maps an
    integer to the element with that integer index in the list*)

val print_matrix : string array array -> unit
val print_matrix_upside_down : string array array -> unit
