(** Module for the menu that displays the player's party.*)

(** [menu] variant that describes the mode the menu is currenty
    executing at.*)
type menu =
  | MainMenu
  | MiniMenu
  | SwitchMode
  | ItemMode
  | Exit

(** [init_mode] variant that represents where this menu is called from,
    and the module handles each specific case.*)
type init_mode =
  | OverworldMode
  | BattleSwitch
  | InventoryMode
  | FaintedSwitch

val init : init_mode -> unit -> unit
(** [init im ()] initializes the party menu, based on the [init_mode]. *)

val set_current_item : Item.item -> unit
(** [set_current_item i ()] sets the current item being used by the menu
    to [i]. *)

val get_current_item : Item.item option
(** [get_current_item ] returns the current item being used by the menu. *)

val load_assets : unit -> unit
(** [load_assets ()] loads all the assets for the menu. *)
