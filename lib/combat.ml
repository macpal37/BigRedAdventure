(* open Creature

   type bstatus = | Victory | Loss | Flee | Catch | Ongoing

   type btype = | Trainer | Wild

   type battle_record = { player_creatures : creature list;
   enemy_creatures : creature list; battle_type : btype; battle_status :
   bstatus; escape_attempts : int; }

   exception Empty

   let reset_clist_player cpair brecord = match brecord.player_creatures
   with | _ :: t -> fst cpair :: t | _ -> raise Empty

   let reset_clist_enemy cpair brecord = match brecord.enemy_creatures
   with | _ :: t -> snd cpair :: t | _ -> raise Empty

   let execute_move player_or_enemy attack_id brecord = if
   player_or_enemy then let out_pair = Move.execute_move attack_id
   (List.nth brecord.player_creatures 0) (List.nth
   brecord.enemy_creatures 0) in

   { brecord with player_creatures = reset_clist_player out_pair
   brecord; enemy_creatures = reset_clist_enemy out_pair brecord; } else
   let out_pair = Move.execute_move attack_id (List.nth
   brecord.enemy_creatures 0) (List.nth brecord.player_creatures 0) in

   { brecord with player_creatures = reset_clist_player out_pair
   brecord; enemy_creatures = reset_clist_enemy out_pair brecord; }

   let run_away brecord = let pspeed = List.nth (get_stats (List.nth
   brecord.player_creatures 0)) 6 in let espeed = List.nth (get_stats
   (List.nth brecord.enemy_creatures 0)) 6 in let odds_escape = ((pspeed
   * 32 / (espeed / 4 mod 256)) + 30) * brecord.escape_attempts in if
   pspeed >= espeed || odds_escape > 255 || Random.int 256 > odds_escape
   then { brecord with battle_status = Flee } else { brecord with
   escape_attempts = brecord.escape_attempts + 1 }

   (*let capture brecord = let e_currHP = List.nth (get_stats (List.nth
   brecord.enemy_creatures 0)) 1 let e_maxHP = List.nth (get_stats
   (List.nth brecord.enemy_creatures 0)) 0 let e_rate = *) *)
