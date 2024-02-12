(* 1. a) *)
let je_pravokotni a b c = 
  if c>=a && c>=b then c*c = a*a + b*b
  else if b>=a && b>=c then b*b = a*a + c*c
  else a*a = c*c + b*b

(* 1. b) *)
let geometrijsko a q n =
  let rec potenca x = function
    |0 -> 1
    |n -> x*(potenca x (n-1))
  in
  let rec aux acc = function
    |0 -> a::acc
    |i -> aux (((potenca q i)*a)::acc) (i-1)
  in
  aux [] (n-1)

(* 1. c) *)
let premesaj = function
  |(a, b, (c, d, e)) -> (c, d, (a, e, c))

(* 1. d) *)
let odberi list i_list =
  (*predpostavim da 0 <= i <= dolzina seznama*)
  let rec i_ti i trenutni_i = function
    |[] -> failwith "index out of bounds"
    |x::xs ->
      if trenutni_i = i then x
      else i_ti i (trenutni_i + 1) xs
  in
  let rec reverse_aux acc = function
    | [] -> acc
    | x :: xs -> reverse_aux (x::acc) xs
  in
  let rec odberi_aux acc = function
    |[] -> acc
    |x::xs -> odberi_aux ((i_ti x 0 list)::acc) xs
  in
  reverse_aux [] (odberi_aux [] i_list)