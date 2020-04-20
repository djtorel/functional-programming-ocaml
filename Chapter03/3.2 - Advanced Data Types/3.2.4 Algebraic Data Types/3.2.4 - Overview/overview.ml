type point = float * float
type 'a pointX = X of 'a
type 'a pointY = Y of 'a
type 'a point' = 'a pointX * 'a pointY

let point1: int point' = (X 1, Y 2)


type shape =
  | Point  of point
  | Circle of point * float (* center and radius *)
  | Rect   of point * point (* lower-left and
                               upper-right corners *)

let pi = 3.1415

let area = function
  | Point _ -> 0.0
  | Circle (_,r) -> pi *. (r ** 2.0)
  | Rect ((x1,y1),(x2,y2)) ->
    let w = x2 -. x1 in
    let h = y2 -. y1 in
    w *. h

let center = function
  | Point p -> p
  | Circle (p,_) -> p
  | Rect ((x1,y1),(x2,y2)) ->
    ((x2 +. x1) /. 2.0,
     (y2 +. y1) /. 2.0)

type string_or_int =
  | String of string
  | Int of int

type string_or_int_list = string_or_int list

let rec sum : string_or_int list -> int = function
  | [] -> 0
  | (String s)::t -> int_of_string s + sum t
  | (Int i)::t -> i + sum t

let three = sum [String "1"; Int 2]


type t = Left of int | Right of int
let x = Left 1
let double_right = function
  | Left i -> i
  | Right i -> 2*i
