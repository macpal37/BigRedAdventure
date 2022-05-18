(** Module for the menu that is displayed during events such as
    capturing a creature or evolving one.*)

val init_capture : Creature.creature -> unit -> unit
(** [init_capture c ()] initaliazes the menu for when crreature [c] is
    captured. The player can then nickname the captured creature. *)

val init_evolution : Creature.creature -> unit -> unit
(** [init_evolution c ()] initaliazes the menu for when crreature [c]
    evolves. *)

val load_assets : unit -> unit
(** [load_assets ()] loads all the assets for the menu. *)
