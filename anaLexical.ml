(*
  Grammaire : 
  TOKEN ::= 'a' | .. | 'z' | 0 | .. | 9
  EXPR ::= TOKEN TOKEN'
  TOKEN' ::= TOKEN | epsilon

  Exemple :
*)

#use "./Projet-PF-LT/anacomb.ml";;

let is_token value : char option = 
  match value with
  | 'a' .. 'z' -> Some value
  | '0' .. '9' -> Some value
  | _ -> None;;

  (*Retourne Char list *)
let token = fun l -> 
  l |> terminal_res is_token;;

let tokenp = fun acc l ->
  l |> (token ++> fun x -> epsilon_res (acc::x))
    +| epsilon_res [acc];;

let p_expr = fun l ->
  l |> token ++> fun x -> tokenp x;;

p_exprToken (list_of_string "a1")

(* Horner Grammaire : 
  U ::= 0 | .. | 9
  C ::= A | epsilon
  A ::= U C
   *)

let is_digit value : int option = 
  match value with
  | '0' .. '9' -> Some (int_of_char value - int_of_char '0')
  | _ -> None;;

let p_U = fun l ->
  l |> terminal_res is_digit;;

let rec p_C = fun acc l ->
  l |> p_A acc +| epsilon_res acc
and p_A  = fun acc l ->
  l |> p_U ++> fun x -> p_C (acc * 10 + x);;

let p_horner = fun l -> 
  l |> p_A 0;;

p_horner (list_of_string "152adsqd")

(*
  Grammaire expression arithmétique : 
  E ::= T E'
  E' ::= + T E' | - T E' | epsilon
  T ::= F T'
  T' ::= * F T' | / F T' | epsilon
  F ::= ( E ) | HORNER
  *)

type aexp = 
  | Apl of aexp * aexp
  | Ami of aexp * aexp
  | Amu of aexp * aexp
  | Adi of aexp * aexp
  | Aco of int;;

let rec p_F = fun l ->
  l |> (terminal '(' -+> p_E ++> fun x -> terminal ')' -+> epsilon_res x)
    +| ( p_horner ++> fun x -> epsilon_res (Aco x) )
and p_Tp = fun acc l ->
  l |> (terminal '*' -+> p_F ++> fun x -> p_Tp (Amu (acc, x)))
    +| (terminal '/' -+> p_F ++> fun x -> p_Tp (Adi (acc, x)))
    +| epsilon_res acc
and p_T = fun l ->
  l |> p_F ++> fun x -> p_Tp x
and p_Ep = fun acc l ->
  l |> (terminal '+' -+> p_T ++> fun x -> p_Ep (Apl (acc, x)))
    +| (terminal '-' -+> p_T ++> fun x -> p_Ep (Ami (acc, x)))
    +| epsilon_res acc
and p_E = fun l ->
  l |> p_T ++> fun x -> p_Ep x;;

let p_aexp = fun l ->
  l |> p_E;;

p_aexp (list_of_string "1+57*3")


