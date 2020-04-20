let rec sum lst =
  match lst with
  | [] -> 0
  | h::t -> h + sum t


let rec length lst =
  match lst with
  | [] -> 0
  | _::t -> 1 + length t

let print_result f lst =
  string_of_int (f lst)
  |> print_endline

let print_int_list lst =
  let rec construct_string lst str =
    match lst with
    | [] -> str ^ "]"
    | h::[] -> str ^ string_of_int h ^ "]"
    | h::t -> str ^ construct_string t (string_of_int h ^ "; ")

  in construct_string lst "["
     |> print_endline

let rec append lst1 lst2 =
  match lst1 with
  | [] -> lst2
  | h::t -> h::(append t lst2)

let empty lst =
  lst = []

let print_bool b =
  b
  |> string_of_bool
  |> print_endline

let main () =
  print_result sum [1; 2; 3; 4; 5];

  print_result length [1; 2; 3; 4; 5];

  print_int_list [1; 2; 3; 4; 5];

  append [1; 2; 3] [4; 5; 6]
  |> print_int_list;

  empty []
  |> print_bool;

  empty [1; 2; 3]
  |> print_bool;

  ();;

main()
