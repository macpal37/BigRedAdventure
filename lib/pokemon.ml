 
 
type stats = {
  max_hp:int
}
type status = Dead|Alive
type etype = Normal | Fire | Water | Grass | Fairy | Typeless
type leveling_rate = Fast | MediumFast | MediumSlow | Slow
(* type ability *)
  
  
 type pokemon = {
  stats:stats;
  status:status;
  etypes:(etype*etype);
  leveling_rate:leveling_rate;
  
  poke_id:int;
  current_hp:int;
  catch_rate: int;
  base_exp:int;
  friendship:int;
  
 }