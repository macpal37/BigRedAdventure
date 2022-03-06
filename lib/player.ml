type player = {
  name : string;
  money : int;
  time_played : int;
  badges : string list;
}

let new_player s = { name = s; money = 0; time_played = 0; badges = [] }
let name p = p.name
let money p = p.money
let time_played p = p.time_played
let badges p = p.badges
let add_money i p = { p with money = p.money + i }
let add_time_played i p = { p with time_played = p.time_played + i }
let add_badge b p = { p with badges = b :: p.badges }
