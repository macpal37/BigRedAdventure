(** Widely used helper functions.

    This module contains a number of commonly used helper functions. By
    separating these functions into a separate module, we eliminate the
    need to copy them to each module that needs them. *)

exception Out_of_Index
(** Raised whenever an index outside of range of an array or list is
    being accessed. *)

exception Segfault

type 'a pointer = 'a option ref

val null : unit -> 'a pointer
val malloc : 'a -> 'a pointer
val deref : 'a pointer -> 'a
val assign : 'a pointer -> 'a -> unit
val ( ~! ) : 'a pointer -> 'a
val ( *= ) : 'a pointer -> 'a -> unit

exception Found_None

val ( !! ) : 'a option -> 'a
(** Forces the value of an option to be returned. Raises [Found_None] if
    the option is None*)

type point = {
  mutable x : int;
  mutable y : int;
}
(** A record thaat represents a point. It contains mutable [x] and [y]
    fields.*)

val new_point : unit -> point
(** [new_point () ] creates a new point initilized at (0,0)*)

val print_int : string -> int -> unit

val bound : int -> int -> int -> int
(** [bound number min max] bounds the [number] to either the [min] or
    [max] if it goss below or above them respectively.*)

val boundf : float -> float -> float -> float
(** [bound number min max] same as [bound] but for float values.*)

val rand : int -> unit -> int
val randf : float -> unit -> float

val string_of_intf : float -> string
(** [string_of_intf] Returns a string of the float as if itw as an int.*)

val captilize_all_string : string -> string
(** [captilize_all_string text ] returns a string with all the words
    separate with a space captilized.*)

val list_index_fun : 'a list -> int -> 'a
(** [list_index_fun l i] is a the element at index [i] in list [i]. Call
    [list_index_fun l] to build a convenient function that maps an
    integer to the element with that integer index in the list*)

val print_matrix : string array array -> unit
(** Print string matrix*)

val print_matrix_upside_down : string array array -> unit
(** Print string matrix upside down*)
