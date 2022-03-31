val run_tick : unit -> unit
val init : bool -> unit -> unit

type menu =
  | MainMenu
  | MiniMenu
  | SwitchMode
  | ItemMenu
  | Exit
