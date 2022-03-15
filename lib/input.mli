val nil : char
(** Char representing no key being pressed*)

val poll : unit -> unit
(** Polls the Graphics for the latest key press.*)

val key_char : unit -> char
(** [key_char _] is the char of the last key press. Equals nil if no key
    pressed*)

val key_option : unit -> char option
(** [key_option _] is the option of the char of the last key press, or
    None if no key pressed*)
