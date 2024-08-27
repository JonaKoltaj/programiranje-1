(* 1. a) *)
let vzporedna (x1,y1) (x2,y2) = x1*y2 == x2*y1

(* 1. b) *)
let rec krizaj = function
    |[] -> []
    |[x] -> [] (* se ne zgodi ce je list sod, samo da appeasamo ocaml *)
    |x::y::xs -> y::x::xs

let zlozi_pocez l1 l2 = List.fold_right2 (fun a b c -> (a,b)::c) l1 (krizaj l2) []

(* 1. c) *)
let f = fun x -> x*x
let rec n_ti_kompozitum f = function
  |1 -> f
  |n -> fun x -> f ((n_ti_kompozitum f (n-1)) x)

let rec kompozitumi f = function
  |0 -> []
  |n -> (kompozitumi f (n-1))@[n_ti_kompozitum f n]

(* 1. d) *)
let rec repi = function
  |[] -> [[]]
  |x::xs -> (x::xs)::(repi xs)

(* 1. e) *)
type ('a, 'b) sum = Left of 'a | Right of 'b

let iso1 (a,sum) = match sum with
  |Left b -> Left (a, b)
  |Right c -> Right (a,c)

let iso2 = function
  |Left (a,b) -> (a, Left b)
  |Right (a,c) -> (a, Right c)