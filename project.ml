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
    S' ::= ; S S' | ε
    S  ::= V := E
          | i ’(’ V ’)’ ’{’ M ’}’ ’{’ M ’}’  
          | w ’(’ V ’)’ ’{’ M ’}’ 
    M  ::= S S'

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

(* Partie principale *)

(* Exercice 2.1.1 Implémenter un analyseur syntaxique en OCaml pour la grammaire obtenue du langage
WHILEb- -. *)

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

let is_var (c:char) : bool = match c with
  | 'A' | 'B' | 'C' | 'D' -> true
  | _ -> false;;

let is_bool (c:char) : bool = match c with
  | '1' | '0' -> true
  | _ -> false;;

let ana_V = fun l -> terminal_cond is_var;;

let ana_B = fun l -> terminal_cond is_bool;;

let ana_E = fun l -> ana_V -| ana_B ;;




