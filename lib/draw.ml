open Yojson.Basic.Util
open Graphics

type pixel = {
  xyrgba2:string*string
}

let pokemon_sprite_size = 240


let pixel_of_json json = 
  let pixels = String.split_on_char ',' (json |> member "xyrgba2" |> to_string) in
  {
  xyrgba2 = ((List.nth pixels 0),(List.nth pixels 1))
}

let get_xyrgba values = 
  let xyrgba = String.split_on_char '|' values in
    let x =  int_of_string (List.nth xyrgba 0) and
    y =  int_of_string (List.nth xyrgba 1) and
    r =  int_of_string (List.nth xyrgba 2) and
    g =  int_of_string (List.nth xyrgba 3) and
    b =  int_of_string (List.nth xyrgba 4) and
    a =  int_of_string (List.nth xyrgba 5)  
  in x,y,r,g,b,a

  let draw_pixel size x y = (fun () -> fill_rect (x - size/2) (y-size/2) size size) 

let draw_xyrgba xyrgba width o_x o_y = let  x,y,r,g,b,a = (get_xyrgba xyrgba) in 
let color = (rgb r g b) in 
set_color color; if a = 1 then draw_pixel 3 (o_x-width/2 + x) (o_y +width/2 -y) ()  


  let rec draw_from_pixels list width o_x o_y = 
    match list with 
    | [] -> print_endline "Done!"
    | h::t ->  match h.xyrgba2 with
    |(pixel1,pixel2) -> 
    (draw_xyrgba pixel1 width o_x o_y); 
    (draw_xyrgba pixel2 width o_x o_y);
    draw_from_pixels t width o_x o_y


let draw_pokemon name o_x o_y = (fun () -> 
  let json = (Yojson.Basic.from_file ("assets/pokemon/"^name^".json") )in
  let pixels = json |> member "pixels" |> to_list |> List.map pixel_of_json in
  draw_from_pixels pixels pokemon_sprite_size o_x o_y)





