type etat = int list;;

let s:etat = [1; 2; 3];

let rec print = fun s ->
  match s with
  | [] -> []
  | a::s -> a::(print s)
;;


