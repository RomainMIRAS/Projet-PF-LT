(*****************************************************)
(* Project : Analyse syntaxique et sémantique *)
(*Louane LESUR
  Gregory PITITTO
  Albin HORLAVILLE
  Romain MIRAS*)
(*****************************************************)

(* Exercice 1.1.1 Définir une hiérarchie de types OCaml permettant de représenter tous les programmes ad-
mis par la description ci-dessus. *)
type var = A | B | C | D 

type exp = Vexp of var | Bexp of bool

type stmt =
  | Skip
  | Assign of var * exp
  | Seq of stmt * stmt
  | If of var * stmt * stmt
  | While of var * stmt;;

#use "./Projet-PF-LT/anacomb.ml";;
(* Exercice 1.1.2 Donner une grammaire décrivant le langage WHILEb- -. *)
(** Grammaire

  V  ::= a | b | c | d
  B  ::= 1 | 0
  E  ::= V | B
  S  ::= skip | V := E | S ; S | i ’(’ V ’)’ ’{’ S ’}’ ’{’ S ’}’ | w ’(’ V ’)’ ’{’ S ’}’

**)

(*Exercice 1.1.3 La grammaire que vous avez écrite est très probablement récursive gauche dans le cas de
la séquence de programmes. Modifiez-la pour remédier à ce problème*)

(** Grammaire 
  
    V  ::= a | b | c | d
    B  ::= 1 | 0
    E  ::= V | B
    S  ::= skip | V := E | i ’(’ V ’)’ ’{’ S ’}’ ’{’ S ’}’ | w ’(’ V ’)’ ’{’ S ’}’
    S’ ::= S | S ; S’
**)

let var_option (c:char) : var option = match c with
  | 'A' -> Some A
  | 'B' -> Some B
  | 'C' -> Some C
  | 'D' -> Some D
  | _ -> None;;

let bool_option (c:char) : bool option = match c with
  | '1' -> Some true
  | '0' -> Some false
  | _ -> None;;

let rana_V l = terminal_res var_option l;;

let rana_B l = terminal_res bool_option l;;

