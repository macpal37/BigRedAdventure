type 'a t = 'a array * int ref
exception Empty
let make l = if List.length l > 0 then (Array.of_list l, ref 0) else raise Empty
let peek (a, index) = a.(!index)

let pop (a, index) =
  let v = peek (a, index) in
  index := (!index + 1) mod Array.length a;
  v