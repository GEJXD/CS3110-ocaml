(** [hd list] returns the first element of [lst]
    Raises [Failure "hd"] if [lst = []] *)
let hd = function
    | [] -> Failure "hd"
    | head :: tail -> head

let rec sum lst = 
    match lst with
    | [] -> 0
    | head :: tail -> head + sum tail

let rec sum_fast (lst : int list ) (acc : int) : int = 
    match lst with
    | [] -> 0
    | head :: tail -> sum_fast tail (acc + head)

let sum_tr lst = sum_fast lst 0

let rec from i j lst = if i > j then lst else from i (j - 1) (j :: lst)

type day = Sun | Mon | Tue | Wed | Thu | Fri | Sat

let int_of_days (d : day) = 
  match d with
  | Sun -> 1
  | Mon -> 2
  | Tue -> 3
  | Wed -> 4
  | Thu -> 5
  | Fri -> 6
  | Sat -> 7

type student = {
  name : string;
  year : int; (* graduate year*)
}

let rbg = {
  name = "Ruth Bader";
  year = 1954;
}

let copy = {
    rbg with name = "another name"
}

type point = float * float
type shapes = 
    | Circle of {center : point; raduis : float}
    | Rectangle of {lower_left : point; upper_right : point}
    | Point of point

let avg a b = (a +. b) /. 2.

let abs x = if x >= 0. then x else (-.x)

let center (s : shapes) = 
    match s with
    | Circle {center; raduis} -> center
    | Rectangle {lower_left = (x_ll, y_ll);
                 upper_right = (x_ur, y_ur)} ->
        (avg x_ll x_ur, avg y_ll y_ur)
    | Point p -> p

let area = function
    | Point _ -> 0.0
    | Circle {center; raduis} -> Float.pi *. (raduis *. raduis)
    | Rectangle {lower_left = (x_ll, y_ll);
                 upper_right = (x_ur, y_ur)} ->
            (abs (x_ll -. x_ur) *. abs (y_ll -. y_ur))

type foo = int * int * float

let fst3 (x, y, z) = x
let snd3 (x, y, z) = y
let tail3 (x, y, z) = z

type string_or_int = 
    | String of string
    | Int of int

(* sum type *)

type blue_or_pink_int = 
    | Blue of int
    | Pink of int 
(* taged union *)

