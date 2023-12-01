(*****************************************************)
(* Project : Analyse syntaxique et sémantique *)
(*Louane LESUR
  Gregory PITITTO
  Albin HORLAVILLE
  Romain MIRAS*)
(*****************************************************)

(* Exercice 1.1.1 Définir une hiérarchie de types OCaml permettant de représenter tous les programmes ad-
mis par la description ci-dessus. *)
type var = A | B | C | D ;;

type exp = Vexp of string | Bexp of bool | Bnot of exp | Band of exp * exp | Bor of exp * exp;;

type stmt =
  | Skip
  | Assign of var * exp
  | Seq of stmt * stmt
  | If of exp * stmt * stmt
  | While of exp * stmt;;

#use "./anacomb.ml";;
(* Exercice 1.1.2 Donner une grammaire décrivant le langage WHILEb- -. *)
(** Grammaire

  V  ::= a | b | c | d
  B  ::= 1 | 0
  E  ::= V | B
  S  ::= ε | V := E | S ; S | i ’(’ E ’)’ ’{’ S ’}’ ’{’ S ’}’ | w ’(’ E ’)’ ’{’ S ’}’

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

(* Exercice 2 Implémenter un analyseur syntaxique en OCaml pour la grammaire obtenue du langage
WHILEb. *)

  (** Grammaire Partie 2
      
    // PARTIE EXPRESSION BOOLEAN
    B ::= ’0’ | ’1’
    V ::= ’a’ | ’b’ | ’c’ | ’d’
    E ::= B | V
    ET ::= T E’
    E’ ::= ’+’ T E’ | ε
    T ::= F T’
    T’ ::= ’.’ F T’ | ε
    F ::= ’!’ F | E | ’(’ ET ’)'

    // PARTIE STATEMENT
    S' ::= ; M | ε
    S ::= V := ET
        | i ’(’ ET ’)’ ’{’ M ’}’ ’{’ M ’}’  
        | w ’(’ ET ’)’ ’{’ M ’}’ | ε
    M ::= S S'

  **)

(* Version Analist *)
let is_var (c:char) : bool = match c with
  | 'a' | 'b' | 'c' | 'd' -> true
  | _ -> false;;

let is_bool (c:char) : bool = match c with
  | '1' | '0' -> true
  | _ -> false;;

(** Partie Expression **)
let ana_B = fun l -> 
  l |> terminal_cond is_bool;;

let ana_V = fun l -> 
  l |> terminal_cond is_var;;

let ana_E = fun l -> 
  l |> ana_V -| ana_B ;;

let rec ana_ET = fun l ->
  l |> ana_T --> ana_Ep

and ana_Ep = fun l ->
  l |> (terminal '+' --> ana_T --> ana_Ep)
  -| epsilon

and ana_T = fun l ->
  l |> ana_F --> ana_Tp

and ana_Tp = fun l ->
  l |> (terminal '.' --> ana_F --> ana_Tp)
  -| epsilon

and ana_F = fun l ->
  l |> (terminal '!' --> ana_F)
  -| (ana_E)
  -| (terminal '(' --> ana_ET --> terminal ')')

(** Partie Statement **)

and ana_Sp = fun l ->
  l |> terminal ';' --> ana_M
  -| epsilon

and ana_M = fun l -> 
  l |> ana_S --> ana_Sp

and ana_S = fun l ->
  l |> (terminal 'i' --> terminal '(' --> ana_ET --> terminal ')' --> terminal '{' --> ana_M --> terminal '}' --> terminal '{' --> ana_M --> terminal '}')
  -| ( terminal 'w' --> terminal '(' --> ana_ET --> terminal ')' --> terminal '{' --> ana_M --> terminal '}')
  -| (ana_V --> terminal ':' --> terminal '=' --> ana_ET)
  -| (epsilon);;

  (* TEST GRAMMAIRE *)
  ana_M (list_of_string "a:=!(a+b.c)");;

  let _ = assert (ana_M (list_of_string "a:=1;w(a){}" ) = []);;
  let _ = assert (ana_M (list_of_string "a:=1;i(a){b:=1}{b:=0}") = []);;
  let _ = assert (ana_M (list_of_string "a:=1;b:=0;w(a){i(b){b:=0}{b:=1}}") = []);;
  let _ = assert (ana_M (list_of_string "a:=1;b:=0;w(a){i(b){c:=0}{a:=0;a:=1}}") = []);;
  let _ = assert (ana_M (list_of_string "a:=1;b:=0;c:=0;i(b){c:=1}{i(c){a:=b}{a:=c}}") = []);;
  let _ = assert (ana_M (list_of_string "i(a){d:=1}{;;;}") = []);;

(* Version Ranalist *)


(** Partie Expression **)

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

let rec rana_ET = fun l ->
  l |> rana_T ++> fun x -> rana_Ep x

and rana_Ep = fun acc l ->
  l |> (terminal '+' -+> rana_T ++> fun x -> rana_Ep (Bor (acc, x)))
  +| epsilon_res acc

and rana_T = fun l ->
  l |> rana_F ++> fun x -> rana_Tp x

and rana_Tp = fun acc l ->
  l |> (terminal '.' -+> rana_F ++> fun x -> rana_Tp (Band (acc, x)))
  +| epsilon_res acc

and rana_F = fun l ->
  l |> (terminal '!' -+> rana_F ++> fun x -> epsilon_res (Bnot x))
  +| (rana_E ++> fun x -> epsilon_res x)
  +| (terminal '(' -+> rana_ET ++> fun x -> terminal ')' -+> epsilon_res x)

(** Partie Statement **)
and rana_Sp = fun acc l ->
  l |> (terminal ';' -+> rana_M ++> fun x -> rana_Sp (Seq (acc, x)))
  +| epsilon_res acc

and rana_M = fun l ->
  l |> rana_S ++> fun x -> rana_Sp x

and rana_S = fun l ->
  l |> (rana_V ++> fun x ->
                   terminal ':' --> terminal '=' -+>
                   rana_ET ++> fun y -> epsilon_res (Assign (x,y)))
       +| (terminal 'i' -->
             terminal '(' -+> rana_ET ++> fun x -> terminal ')' -->
             terminal '{' -+> rana_M ++> fun y -> terminal '}' -->
             terminal '{' -+> rana_M ++> fun z -> terminal '}' -+> epsilon_res (If(x,y,z)))
       +| (terminal 'w' -->
             terminal '(' -+> rana_ET ++> fun x -> terminal ')' -->
             terminal '{' -+> rana_M ++> fun y -> terminal '}' -+> epsilon_res (While (x,y)))
  +| (epsilon_res Skip);;

  (* TEST GRAMMAIRE *)

  rana_M (list_of_string "a:=1;w(a){}");;

  let _ = assert (rana_M (list_of_string "a:=1;w(a){}" ) = (Seq (Assign (A, Bexp true), While (Vexp A, Skip)), []));;
  
  let _ = assert (rana_M (list_of_string "a:=1;i(a){b:=1}{b:=0}") = (Seq (Assign (A, Bexp true), If (Vexp A, Assign (B, Bexp true), Assign (B, Bexp false))), []));;
  
  let _ = assert (rana_M (list_of_string "a:=1;b:=0;w(a){i(b){b:=0}{b:=1}}") = (Seq (Assign (A, Bexp true), Seq (Assign (B, Bexp false), While (Vexp A, If (Vexp B, Assign (B, Bexp false), Assign (B, Bexp true))))), []));;
  
  let _ = assert (rana_M (list_of_string "a:=1;b:=0;w(a){i(b){c:=0}{a:=0;a:=1}}") = (Seq (Assign (A, Bexp true), Seq (Assign (B, Bexp false), While (Vexp A, If (Vexp B, Assign (C, Bexp false), Seq (Assign (A, Bexp false), Assign (A, Bexp true)))))), []));;
  
  let _ = assert (rana_M (list_of_string "a:=1;b:=0;c:=0;i(b){c:=1}{i(c){a:=b}{a:=c}}") = (Seq (Assign (A, Bexp true), Seq (Assign (B, Bexp false), Seq (Assign (C, Bexp false), If (Vexp B, Assign (C, Bexp true), If (Vexp C, Assign (A, Vexp B), Assign (A, Vexp C)))))), []));;
  
  let _ = assert (rana_M (list_of_string "i(a){d:=1}{;;;}") = (If (Vexp A, Assign (D, Bexp true), Seq (Skip, Seq (Skip, Seq (Skip, Skip)))), [])) ;;


(*** PARTIE PARTIE ***)

let rec clear = fun l ->
  match l with
  | [] -> []
  | ' '::l | '\n'::l | '\t'::l -> clear l
  | a::l -> a::(clear l)
;;

let parse_from_file (filename:string) : stmt =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  let l = list_of_string s in
  let (p, _) = rana_M (clear l) in
  p;;

let parse_from_string (s:string) : stmt =
  let l = list_of_string s in
  let (p, _) = rana_M (clear l) in
  p;;

parse_from_file "./test.txt";;
