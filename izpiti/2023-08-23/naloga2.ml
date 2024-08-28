type player = White | Black

type game_tree =
  |Winner of player
  |Tie
  |Decision of player * (float * game_tree) list

let primer =
  Decision
    ( White,
      [
        (0.3, Decision (Black, [ (0.5, Winner White); (0.5, Winner Black) ]));
        (0.7, Decision (Black, [ (0.5, Tie); (0.5, Winner Black) ]));
      ] )

(* 1. a) *)
let prestej_zmage gt =
  let rec aux acc = function
    |Winner p -> (match p with
      |White -> acc + 1
      |Black -> acc
      )
    |Tie -> acc
    |Decision (p1,list) -> (match list with
      |[] -> acc
      |(fl,tree)::xs -> aux (aux acc tree) (Decision (p1,xs))
      )
  in
  aux 0 gt

(* 1. b) *)
type result = { white_wins : float; black_wins : float; ties : float }

let rezultat gt =
  let rec aux acc1 acc2 = function
    |[] -> acc2
    |(fl,tree)::xs ->
      let novi_acc2 = match tree with
      |Winner p -> (match p with
        |White -> { acc2 with white_wins = acc2.white_wins +. fl *. acc1}
        |Black -> { acc2 with black_wins = acc2.black_wins +. fl *. acc1}
        )
      |Tie -> { acc2 with ties = acc2.ties +. fl *. acc1}
      |Decision (_,list) -> aux (fl *. acc1) acc2 list
      in
      aux acc1 novi_acc2 xs

  in
  match gt with
    |Decision (_,list) -> aux 1.0 {white_wins = 0.0; black_wins = 0.0; ties = 0.0} list
    |_-> {white_wins = 0.0; black_wins = 0.0; ties = 0.0}

(* 1. c) *)
let je_veljavno gt =
  let rec aux acc = function
    |Decision (_,list) ->
      let vsota = List.fold_left (fun fl1 (fl2,_) -> fl1 +. fl2) 0.0 list in
      let je_ena = Float.equal vsota 1.0 in
      List.fold_left (fun b (fl, tree) -> aux b tree) (acc && je_ena) list
    |_ -> acc
  in
  aux true gt

(* 1. d) *)
let odigraj_igro gt odlocitve =
  let rec aux acc_fl acc = function
    |Winner p -> Some (p, acc_fl)
    |Tie -> None
    |Decision (pl,list) -> (match acc with
      |[] -> None
      |i::is ->
        if (List.length list <= i) || (i<0) then None
        else
          let (fl, odlocitev) = List.nth list i in
          aux (fl *. acc_fl) is odlocitev
      )
  in
  aux 1.0 odlocitve gt