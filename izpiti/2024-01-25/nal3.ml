(* let stevilo_kombinacij l k =
  let sez = [] in
  let rec aux acc init l k =
    if k = 1 then l::acc
    else
      (for i = init to l/k do
        let sez = ::(aux i::acc (i+1) (l-i) (k-1)) in
      done)
      sez
    in
aux [] 1 l k *)