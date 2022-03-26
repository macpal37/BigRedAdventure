let bound num min max =
  if num >= max + 1 then max else if num <= min + 1 then min else num

let rand max () =
  Random.self_init ();
  Random.int max

let captilize_all_string str =
  let lst = String.split_on_char ' ' str in
  List.fold_left (fun x y -> x ^ " " ^ String.capitalize_ascii y) "" lst
