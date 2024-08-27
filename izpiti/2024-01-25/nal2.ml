type 'a dict = (string * 'a) list
type primitive = Bool of bool | Int of int | String of string | Null
type json = Primitive of primitive | Object of json dict | Array of json list

let reverse list =
  let rec aux acc = function
    | [] -> acc
    | x :: xs -> aux (x::acc) xs
  in
  aux [] list

let json_example =
  Object
  [
  ("name", Primitive (String "Matija"));
  ("age", Primitive (Int 20));
  ("friends", Array [
  Primitive (Int 1);
  Primitive (Int 2);
  Primitive (Int 3)
  ]);
  ("is_student", Primitive (Bool true));
  ("is_professor", Primitive (Bool false));
  ("is_ta", Primitive Null);
  ]

let prestej_stevila j =
  let rec aux acc = function
    |Primitive a -> (match a with
      |Int i -> acc + 1
      |_ -> acc)
    |Array list -> (match list with
      |[] -> acc
      |x::xs ->
        let jxs = Array(xs) in
        aux (aux acc x) jxs)
    |Object d -> (match d with
      |[] -> acc
      |(_,c)::cs ->
        let jcs = Object(cs) in
        aux (aux acc c) jcs)
  in
  aux 0 j

let izloci_nize globina jsn =
  let rec aux acc i json = match json with
    |Primitive a ->
      if i <= 0 then (match a with
        |String s -> s::acc 
        |_ -> acc)
      else acc
    |Array list -> (match list with
        |[] -> acc
        |x::xs -> aux (aux acc (i-1) x) i (Array(xs)))
    |Object d -> (match d with
        |[] -> acc
        |(_,c)::cs -> aux (aux acc (i-1) c) i (Object(cs)))
  in
  aux [] globina jsn

let dodaj_predpono str jsn = 
  let rec aux acc = function
    |Object d -> (match d with
      |[] -> acc
      |(s,c)::ss -> aux ((str ^ s, c)::acc) (Object(ss))
      )
    |_ -> acc
  in
  Object(reverse(aux [] jsn))

let rec izpisi jsn =
  let print_list lst =
    print_string "[";
    let rec aux = function
      |[] -> print_endline "]"
      |[x] -> izpisi x; print_string "]"
      |x::xs -> izpisi x; print_string ", "; aux xs
    in
    aux lst
  in
  let print_dict_list lst =
    print_endline "{";
    let rec aux = function
      |[] -> print_endline "}"
      |[(s,c)] -> print_string ("  " ^ s ^ ": "); izpisi c;print_endline ""; print_endline "}"
      |(s,c)::cs -> print_string ("  " ^ s ^ ": "); izpisi c;print_endline ","; aux cs
    in
    aux lst
  in
  match jsn with
  |Object d -> print_dict_list d
  |Array list -> print_list list
  |Primitive a -> (match a with
    |Bool f -> if f then print_string "true" else print_string "false"
    |Int i -> print_int i
    |String s -> print_string s
    |Null -> print_string "null"
    )

let type_of_primitive = function (* tle mu je pisalo "and" namesto "let" *)
  | Bool _ -> "bool"
  | Int _ -> "int"
  | String _ -> "string"
  | Null -> "null"

let rec is_primitive = function
  |[] -> true
  |x::xs -> (match x with
    |Primitive a -> is_primitive xs
    |_ -> false
    )
    
let rec primitives_of_list = function (* predpostavimo da sprejmemo list of primitives *)
  |[] -> []
  |x::xs -> (match x with
    |Primitive a -> a::(primitives_of_list xs)
    |_ -> [] (* se itak ne zgodi *)
    )

let same_type_elements list = (* ne rabi posebej preveriti za enega in also ni rekurzivna *)
  if is_primitive list then
    let prim_list = primitives_of_list list in
    match prim_list with
      |[] -> true
      |x::xs -> List.for_all (fun a -> (type_of_primitive a) = (type_of_primitive x)) xs
  else false

let rec je_konsistenten = function
  |Primitive _ -> true
  |Object dict -> List.for_all (fun (_, v) -> je_konsistenten v) dict
  |Array lst -> same_type_elements lst
let szn_exmpl = [
  Primitive (Int 1);
  Primitive (Int 2);
  Primitive (Int 3)]