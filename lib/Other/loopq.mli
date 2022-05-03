type 'a t
(** The type of looped queue that takes in elements of type ['a] *)

exception Empty
(** Raised when trying to make a [Loopq.t] out of an empty list *)

val make : 'a list -> 'a t
(** [make l] is a looped queue representation of [l]. Raises [Empty] if
    [l] is empty *)

val peek : 'a t -> 'a
(** [peek q] is the first element in [q] without removing it from the
    looped queue *)

val pop : 'a t -> 'a
(** [pop q] returns the first element in [q] and removes it from the
    front of the queue *)