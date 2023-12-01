#use "./project.ml";;

(*
  Fonction qui permet d'evaluer un statement *)

let test_statment = parse_from_file "./test.txt";;

type state = {a : bool; b : bool; c : bool; d : bool};;

let rec evalB expression state : bool =
  match expression with
  | Bexp b -> b
  | Vexp var -> (match var with
                 | A -> state.a
                 | B -> state.b
                 | C -> state.c
                 | D -> state.d
                 | _ -> failwith "Erreur evalB")
  | Bnot e -> not (evalB e state)
  | Band (e1, e2) -> (evalB e1 state) && (evalB e2 state)
  | Bor (e1, e2) -> (evalB e1 state) || (evalB e2 state);;

let update state var value =
  match var with
  | A -> {state with a = value}
  | B -> {state with b = value}
  | C -> {state with c = value}
  | D -> {state with d = value};;
  
let rec evalS statement state =
  match statement with
  | Skip -> state
  | Assign (var, exp) -> update state var (evalB exp state)
  | Seq (s1, s2) -> evalS s2 (evalS s1 state)
  | If (exp, s1, s2) -> if (evalB exp state) then evalS s1 state else evalS s2 state
  | While (exp, s) -> if (evalB exp state) then evalS (While (exp, s)) (evalS s state) else state;;

let state = {a = true; b = false; c = true; d = false};;

evalS test_statment state;;