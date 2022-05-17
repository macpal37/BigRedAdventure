type menu =
  | MainMenu
  | MiniMenu
  | SwitchMode
  | ItemMode
  | Exit

type combat_mode =
  | OverworldMode
  | BattleSwitch
  | InventoryMode
  | FaintedSwitch

val run_tick : unit -> unit
val init : combat_mode -> unit -> unit
val set_current_item : Item.item -> unit
val get_current_item : Item.item option
val load_assets : unit -> unit
