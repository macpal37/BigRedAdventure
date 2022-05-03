type 'a t = 'a array * int ref
let make l = (Array.of_list l, ref 0)
let peek (a, index) = a.(!index)

let pop (a, index) =
  let v = a.(!index) in
  index := (!index + 1) mod Array.length a;
  v