let je_sodo i =
  if i mod 2 = 0 then true else false
    
let reverse list =
  let rec aux acc = function
    | [] -> acc
    | x :: xs -> aux (x::acc) xs
  in
  aux [] list

let seznam_sodih list =
  let rec aux acc = function
    |[] -> acc
    |x::xs -> if (je_sodo x) then aux (x::acc) xs
    else aux acc xs
  in
  reverse (aux [] list)

type oznaceno =
  |Sodo of int
  |Liho of int

let oznaci list =
  let rec aux acc = function
    |[] -> acc
    |x::xs -> if (je_sodo x) then aux ((Sodo x)::acc) xs
    else aux ((Liho x)::acc) xs
  in
  reverse (aux [] list)

let vsoti_kvadratov list =
  let rec aux accl accs = function
  |[] -> (accl, accs)
  |x::xs -> match x with
    |Liho i -> aux (i*i + accl) accs xs
    |Sodo i -> aux accl (i*i + accs) xs
  in
  aux 0 0 list