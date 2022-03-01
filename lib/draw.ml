open Yojson.Basic.Util


type pixel = {
  x:int;
  y:int;
  r:int;
  g:int;
  b:int;
  a:bool
}
let pixel_of_json json = {
  x =  json |> member "x" |> to_int;
  y =  json |> member "y" |> to_int;
  r =  json |> member "r" |> to_int;
  g =  json |> member "f" |> to_int;
  b =  json |> member "b" |> to_int;
  a =  json |> member "a" |> to_bool;
}


let draw_pokemon name= 
  let json = (Yojson.Basic.from_file ("assets/"^name^".json") )in
  let pixels = json |> member "pixels" |> to_list |> List.map pixel_of_json in pixels

