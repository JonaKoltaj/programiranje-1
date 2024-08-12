type element = Variable of char | Constant of char
type result = element list

(* Seznam pravil podamo s pari, ki spremenljivkam priredijo sliko  *)
(* Za vse preslikave bomo predpostavili, da so enolične *)
type rules = (char * result) list
type l_tree = Node of element * l_tree list option

let example0 =
  Node
    ( Variable 'A',
      Some
        [
          Node
            ( Variable 'A',
              Some [ Node (Variable 'A', None); Node (Variable 'B', None) ] );
          Node (Variable 'B', Some [ Node (Variable 'A', None) ]);
        ] )

let rules0 = [ ('A', [ Variable 'A'; Variable 'B' ]); ('B', [ Variable 'A' ]) ]

let example1 =
  Node
    ( Variable '0',
      Some
        [
          Node (Variable '1', None);
          Node (Constant '[', None);
          Node (Variable '0', None);
          Node (Constant ']', None);
          Node (Variable '0', None);
        ] )

let rules1 =
  [
    ('1', [ Variable '1'; Variable '1' ]);
    ( '0',
      [ Variable '1'; Constant '['; Variable '0'; Constant ']'; Constant '0' ]
    );
  ]

(* 2. a) *)
let rec map f = function
  |[] -> []
  |x::xs -> (f x) :: map f xs
let rec veljavno = function
  |(_, None) -> true
  |(_, Some list) -> match list with
    |[] -> false
    |list -> List.fold_left (&&) true (map veljavno list)

(* 2. b) *)

(* 2. c) *)

(* 2. d) *)

(* 2. e *)
