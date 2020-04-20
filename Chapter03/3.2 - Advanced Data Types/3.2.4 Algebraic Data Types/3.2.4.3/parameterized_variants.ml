type 'a mylist = Nil | Cons of 'a * 'a mylist

let lst3 = Cons (3, Nil)  (* similar to [3] *)
let lst_hi = Cons ("hi", Nil)  (* similar to ["hi"] *)

let rec length : 'a mylist -> int = function
  | Nil -> 0
  | Cons (_,t) -> 1 + length t

let empty : 'a mylist -> bool = function
  | Nil -> true
  | Cons _ -> false

let rec sum = function
  | Nil -> 0
  | Cons(h,t) -> h + sum t

type ('a,'b) pair = {first: 'a; second: 'b}

let x = {first=2; second="hello"}
