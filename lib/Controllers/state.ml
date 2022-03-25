type state = { player : Player.player }

let current_state = ref { player = Player.new_player "Red" }
let get_state _ = !current_state
let player _ = !current_state.player
