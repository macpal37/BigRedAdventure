(** Widely used helper functions.

    This module contains a number of commonly used helper functions. By
    separating these functions into a separate module, we eliminate the
    need to copy them to each module that needs them. *)

type point = {
  mutable x : int;
  mutable y : int;
}
(** A record thaat represents a point. It contains mutable [x] and [y]
    fields.*)

val new_point : unit -> point
(** [new_point () ] creates a new point initilized at (0,0)*)

val bound : int -> int -> int -> int
(** [bound number min max] bounds the [number] to either the [min] or
    [max] if it goss below or above them respectively.*)

val rand : int -> unit -> int

val captilize_all_string : string -> string
(** [captilize_all_string text ] returns a string with all the words
    separate with a space captilized.*)

val list_index_fun : 'a list -> int -> 'a
(** [list_index_fun l i] is a the element at index [i] in list [i]. Call
    [list_index_fun l] to build a convenient function that maps an
    integer to the element with that integer index in the list*)

val print_matrix : string array array -> unit
val print_matrix_upside_down : string array array -> unit
