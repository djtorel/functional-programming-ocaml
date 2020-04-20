let inc_first lst =
  match lst with
  | [] -> []
  | h::t -> (h+1)::t
