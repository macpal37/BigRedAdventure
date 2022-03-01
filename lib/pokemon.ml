open Yojson.Basic.Util
 
type stats = {
  max_hp:int;
  attack:int;
  defense:int;
  sp_attack:int;
  sp_defense:int;
  speed:int;
}

type status = Healthy | Dead
type etype = Normal | Fire | Water | Grass | Fairy | None
type leveling_rate = Fast | MediumFast | MediumSlow | Slow
  
  
 type pokemon = {
  base_stats:stats;
  curr_stats:stats;
  status:status;
  etypes:(etype*etype);
  leveling_rate:leveling_rate;

  level:int;
  exp: int;

  poke_id:int;
  current_hp:int;
  catch_rate: int;
  base_exp:int;
  friendship:int;
 }

 let stats_of_json json =
  {
  max_hp = json |> member "hp" |> to_int;
  attack= json |> member "attack" |> to_int;
  defense= json |> member "defense" |> to_int;
  sp_attack= json |> member "sp_attack" |> to_int;
  sp_defense = json |> member "sp_defense" |> to_int;
  speed = json |> member "speed" |> to_int;
  } 
  type pokemon = {
    x:int
  }
let etype_from_json json num = 
  let etype_string = (json |> member ("type"^(string_of_int num)) |> to_string) in
  match etype_string with 
  | "Normal" -> Normal
  | "Fire" -> Fire
  | "Water" -> Water
  | "Grass" -> Grass
  | "Fairy" -> Fairy
  | _ -> None

  

  let level_rate_from_json json = 
    let rate_strint = (json |> member "level_rate" |> to_string) in
  match rate_strint with 
  | "Fast" -> Fast
  | "MediumFast" -> MediumFast
  | "MediumSlow" -> MediumSlow
  | "Slow" -> Slow
  | _ -> Fast
    
let calculate_stats bstats level ={
  max_hp = (((2 * bstats.max_hp)* level)/100)+level+10;
  attack= (((2 * bstats.attack)* level)/100)+5;
  defense= (((2 * bstats.defense)* level)/100)+5;
  sp_attack= (((2 * bstats.sp_attack)* level)/100)+5;
  sp_defense = (((2 * bstats.sp_defense)* level)/100)+5;
  speed = (((2 * bstats.speed)* level)/100)+5;
}
let exp_calc level rate = 
  match rate with 
|Fast -> (4*level*level*level)/5
|MediumFast ->(level*level*level)
|MediumSlow ->(6*level*level*level)/5 - (15*level*level)+(100*level)-140
|Slow ->(5*level*level*level)/4

let pokemon_from_json json level = let bstats = stats_of_json json in let lev_rate = (level_rate_from_json json) in {
  base_stats = bstats;
  curr_stats = calculate_stats bstats level;
  status = Healthy;
  etypes = ((etype_from_json json 1),(etype_from_json json 2));
  leveling_rate = level_rate_from_json json;

  level = level;
  exp = (exp_calc level lev_rate);

  poke_id =json |> member "poke_id" |> to_int;
  current_hp = bstats.max_hp;
  catch_rate = json |> member "catch_rate" |> to_int;
  base_exp = json |> member "base_exp" |> to_int;
  friendship = 70;
}
  let create_pokemon json name level = 
    pokemon_from_json (json |> member name) level

