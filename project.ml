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
    S' ::= ; M | ε
    S  ::= V := E
          | i ’(’ V ’)’ ’{’ M ’}’ ’{’ M ’}’  
          | w ’(’ V ’)’ ’{’ M ’}’ | ε
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

(* Version Analist *)
let is_var (c:char) : bool = match c with
  | 'a' | 'b' | 'c' | 'd' -> true
  | _ -> false;;

let is_bool (c:char) : bool = match c with
  | '1' | '0' -> true
  | _ -> false;;

let ana_V = fun l -> 
  l |> terminal_cond is_var;;

let ana_B = fun l -> 
  l |> terminal_cond is_bool;;

let ana_E = fun l -> 
  l |> ana_V -| ana_B ;;

let rec ana_Sp = fun l -> 
  l |> (terminal ';' --> ana_M ) -| epsilon

and ana_M = fun l -> 
  l |> ana_S --> ana_Sp

and ana_S = fun l ->
  l |> (terminal 'i' --> terminal '(' --> ana_V --> terminal ')' --> terminal '{' --> ana_M --> terminal '}' --> terminal '{' --> ana_M --> terminal '}')
  -| ( terminal 'w' --> terminal '(' --> ana_V --> terminal ')' --> terminal '{' --> ana_M --> terminal '}')
  -| (ana_V --> terminal ':' --> terminal '=' --> ana_E)
  -| (epsilon);;

  (** Grammaire modifiée

    V  ::= a | b | c | d
    B  ::= 1 | 0
    E  ::= V | B
    S' ::= ; M | ε
    S  ::= V := E
          | i ’(’ V ’)’ ’{’ M ’}’ ’{’ M ’}’  
          | w ’(’ V ’)’ ’{’ M ’}’ | ε
    M  ::= S S'

**)

  (* TEST GRAMMAIRE *)
  ana_M (list_of_string ";;;");;

  let _ = assert (ana_M (list_of_string "a:=1;w(a){}" ) = []);;
  let _ = assert (ana_M (list_of_string "a:=1;i(a){b:=1}{b:=0}") = []);;
  let _ = assert (ana_M (list_of_string "a:=1;b:=0;w(a){i(b){b:=0}{b:=1}}") = []);;
  let _ = assert (ana_M (list_of_string "a:=1;b:=0;w(a){i(b){c:=0}{a:=0;a:=1}}") = []);;
  let _ = assert (ana_M (list_of_string "a:=1;b:=0;c:=0;i(b){c:=1}{i(c){a:=b}{a:=c}}") = []);;
  let _ = assert (ana_M (list_of_string "i(a){d:=1}{;;;}") = []);;

(* Version Ranalist *)

let var_option (c:char) : var option = match c with
  | 'a' -> Some A
  | 'b' -> Some B
  | 'c' -> Some C
  | 'd' -> Some D
  | _ -> None;;

let bool_option (c:char) : bool option = match c with
  | '1' -> Some true
  | '0' -> Some false
  | _ -> None;;

let rana_V = fun l -> 
  l |> terminal_res var_option;;

let rana_B = fun l ->
  l |> terminal_res bool_option;;

let rana_E = fun l ->
  l |> (rana_V ++> fun x -> epsilon_res (Vexp x))
  +| (rana_B ++> fun x -> epsilon_res (Bexp x));;

let rec rana_Sp = fun l ->
  l |> (terminal ';' -+> rana_M ) +| (epsilon_res Skip)

and rana_M = fun l ->
  l |> rana_S ++> fun x -> rana_Sp ++> fun y -> epsilon_res (Seq (x,y))

and rana_S = fun l ->
  l |> (rana_V ++> fun x ->
                   terminal ':' --> terminal '=' -+>
                   rana_E ++> fun y -> epsilon_res (Assign (x,y)))
       +| (terminal 'i' -->
             terminal '(' -+> rana_V ++> fun x -> terminal ')' -->
             terminal '{' -+> rana_M ++> fun y -> terminal '}' -->
             terminal '{' -+> rana_M ++> fun z -> terminal '}' -+> epsilon_res (If(x,y,z)))
       +| (terminal 'w' -->
             terminal '(' -+> rana_V ++> fun x -> terminal ')' -->
             terminal '{' -+> rana_M ++> fun y -> terminal '}' -+> epsilon_res (While (x,y)))
  +| (epsilon_res Skip);;

  (* TEST GRAMMAIRE *)

  rana_M (list_of_string "a:=1;w(a){}" );;
