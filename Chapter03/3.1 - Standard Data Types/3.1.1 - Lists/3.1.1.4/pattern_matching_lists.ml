match 1::[] with
| [] -> false
| h::t -> (h>=1) && (List.length t = 0)
