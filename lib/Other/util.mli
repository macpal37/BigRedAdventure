(** Widely used helper functions.

    This module contains a number of commonly used helper functions. By
    separating these functions into a separate module, we eliminate the
    need to copy them to each module that needs them. *)

exception Out_of_Index
(** Raised whenever an index outside of range of an array or list is
    being accessed. *)

exception Segfault
(** Raised when trying to dereference a null pointer. *)

type 'a pointer = 'a option ref
(** [pointer] A wrapper for an option ref and behaves similar to a null
    pointer in oop language. *)

val null : unit -> 'a pointer
(** [null ()] returns a fresh null. *)

val malloc : 'a -> 'a pointer
(** [malloc x ()] creates a pointer with the value [x] *)

val deref : 'a pointer -> 'a
(** [deref p ] dereferences a pointer [p] to get its vale. raises
    [Segfault] if the [p] is null.*)

val assign : 'a pointer -> 'a -> unit
(** [assign p x ] assigns the value [x] onto pointer [p], modifying it.*)

val ( ~! ) : 'a pointer -> 'a
(** [  ~!p ] Infix for [deref p].*)

val ( *= ) : 'a pointer -> 'a -> unit
(** [  p *= x ] Infix for [assign p x ].*)

exception Found_None
(** Raised when forcing a vue out of a [None].*)

val ( !! ) : 'a option -> 'a
(** [!!p] Forces the value of an option to be returned. Raises
    [Found_None] if the option is None*)

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
(** [rand i ()] Self inits a random seed nd then generates a random int
    from 0-[i].*)

val randf : float -> unit -> float
(** [rand f ()] Self inits a random seed nd then generates a random
    float from 0-[f].*)

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
