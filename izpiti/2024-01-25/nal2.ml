type 'a dict = (string * 'a) list
type primitive = Bool of bool | Int of int | String of string | Null
type json = Primitive of primitive | Object of json dict | Array of json list

let json_example =
  Object
  [
  ("name", Primitive (String "Matija"));
  ("age", Primitive (Int 20));
  ("friends", Array [
  Primitive (Int 1);
  Primitive (Int 2);
  Primitive (String "Nemo")
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

  