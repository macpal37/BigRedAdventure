let bound num min max =
  if num >= max + 1 then max else if num <= min + 1 then min else num

let rand max () =
  Random.self_init ();
  Random.int max
