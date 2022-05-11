exception Out_of_Index
exception Segfault

type 'a pointer = 'a option ref

let null () : 'a pointer = ref None
let malloc (x : 'a) : 'a pointer = ref (Some x)

let deref (ptr : 'a pointer) : 'a =
  match !ptr with
  | None -> raise Segfault
  | Some r -> r

let assign (ptr : 'a pointer) x = ptr := Some x
let ( ~! ) = deref
let ( *= ) = assign
let print_int str i = print_endline (str ^ string_of_int i)

let bound num min max =
  if num >= max + 1 then max else if num <= min then min else num

let boundf num min max =
  if num >= max +. 1. then max else if num <= min then min else num

let rand max () =
  Random.self_init ();
  Random.int max

let randf max () =
  Random.self_init ();
  Random.float max

let captilize_all_string str =
  let lst = String.split_on_char ' ' str in
  List.fold_left (fun x y -> x ^ " " ^ String.capitalize_ascii y) "" lst

type point = {
  mutable x : int;
  mutable y : int;
}

let new_point () = { x = 0; y = 0 }

let list_index_fun l =
  let a = Array.of_list l in
  fun i -> Array.get a i

let print_matrix a =
  for i = 0 to Array.length a - 1 do
    let s = Array.get a i in
    print_string "[";
    for j = 0 to Array.length s - 1 do
      print_string (Array.get s j);
      print_string ", "
    done;
    print_endline "]"
  done

let print_matrix_upside_down a =
  for i = 1 to Array.length a do
    let s = Array.get a (Array.length a - i) in
    print_string "[";
    for j = 0 to Array.length s - 1 do
      print_string (Array.get s j);
      print_string ", "
    done;
    print_endline "]"
  done
