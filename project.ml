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
  S  ::= ε | V := E | S ; S | i ’(’ V ’)’ ’{’ S ’}’ ’{’ S ’}’ | w ’(’ V ’)’ ’{’ S ’}’

**)

(*Exercice 1.1.3 La grammaire que vous avez écrite est très probablement récursive gauche dans le cas de
la séquence de programmes. Modifiez-la pour remédier à ce problème*)

(** Grammaire modifiée

    V  ::= a | b | c | d
    B  ::= 1 | 0
    E  ::= V | B
    S' ::= ε | ; S S'
    S  ::= ε | V := E S'| i ’(’ V ’)’ ’{’ S ’}’ ’{’ S ’}’ S' | w ’(’ V ’)’ ’{’ S ’}’ S'
    
**)

(* Sémantique naturelle (SN), dite aussi sémantique opérationnelle à grands pas *)

(* Exercice 1.2.1 Règles de SN pour un programme de la forme if expr then P else Q *)
(*
  if expr then P else Q
  SN
  if [expr] then [P] else [Q]
  SN
  if [true] then [P] else [Q] => [P]
  SN
  if [false] then [P] else [Q] => [Q]
*)
