(*****************************************************)
(* Project : Analyse syntaxique et sémantique *)
(*Louane LESUR
  Gregory PITITTO
  Albin HORLAVILLE
  Romain MIRAS*)
(*****************************************************)
(*
  Exercice traitée : Tout exercice du 1 au 2 
  Exercice Falcultaif : 3.2 (Partiellement par Romain MIRAS) 
*)

(* Exercice 1.1.1 Définir une hiérarchie de types OCaml permettant de représenter tous les programmes ad-
mis par la description ci-dessus. *)
type var = A | B | C | D ;;

type exp =
  | Vexp of var
  | Bexp of bool
  | Bnot of exp
  | Band of exp * exp
  | Bor of exp * exp;;

type stmt =
  | Skip
  | Assign of var * exp
  | Seq of stmt * stmt
  | If of exp * stmt * stmt
  | While of exp * stmt;;

#use "./anacomb.ml";;
(*Ce Document neccesite anacomb.ml pour pouvoir fonctionner, ce document n'ai pas inclus dans
   le fichier pour eviter le surplus de code.
   Vous pouvez le retrouver si neccessaire dans le rendu Chamilo. Merci *)

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

(* Exercice 1.1.4 Donner une grammaire non récursive à gauche de ce langage d’expressions. *)
(* Grammaire Final ci dessous *)

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
WHILEb. Tout les exercices de la partie 2 ce trouve si-dessous avec le resultat final 
obtenue à la fin de la partie *)

  (** Grammaire Final WHILEb
      
    // PARTIE EXPRESSION BOOLEAN
    B ::= ’0’ | ’1’
    V ::= ’a’ | ’b’ | ’c’ | ’d’
    E ::= B | V
    ET ::= T E’
    E’ ::= ’+’ T E’ | ε
    T ::= F T’
    T’ ::= ’.’ F T’ | ε
    F ::= ’!’ F | E | ’(’ ET ’)'

    // PARTIE STATEMENT ou INSTRUCTION
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
  l |> (terminal 'i' --> terminal '(' --> ana_ET --> terminal ')' --> terminal '{'
        --> ana_M --> terminal '}' --> terminal '{' --> ana_M --> terminal '}')

       -| ( terminal 'w' --> terminal '(' --> ana_ET --> terminal ')' --> terminal '{'
            --> ana_M --> terminal '}')

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
let _ = assert (ana_M (list_of_string "i(a+b.c){d:=1}{;;;}") = []);;
let _ = assert (ana_M (list_of_string "i((!d)+a){d:=1}{;;;}") = []);;

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

rana_M (list_of_string "i((!d)+a){d:=1}{;;;}");;

let _ = assert (rana_M (list_of_string "a:=1;w(a){}" ) =
                  (Seq (Assign (A, Bexp true), While (Vexp A, Skip)), []));;
  
let _ = assert (rana_M (list_of_string "a:=1;i(a){b:=1}{b:=0}") =
                  (Seq (Assign (A, Bexp true), If (Vexp A, Assign (B, Bexp true), Assign (B, Bexp false))), []));;
  
let _ = assert (rana_M (list_of_string "a:=1;b:=0;w(a){i(b){b:=0}{b:=1}}") =
                  (Seq (Assign (A, Bexp true), Seq (Assign (B, Bexp false), While (Vexp A, If (Vexp B, Assign (B, Bexp false), Assign (B, Bexp true))))), []));;
  
let _ = assert (rana_M (list_of_string "a:=1;b:=0;w(a){i(b){c:=0}{a:=0;a:=1}}") =
                  (Seq (Assign (A, Bexp true), Seq (Assign (B, Bexp false), While (Vexp A, If (Vexp B, Assign (C, Bexp false), Seq (Assign (A, Bexp false), Assign (A, Bexp true)))))), []));;
  
let _ = assert (rana_M (list_of_string "a:=1;b:=0;c:=0;i(b){c:=1}{i(c){a:=b}{a:=c}}") =
                  (Seq (Assign (A, Bexp true), Seq (Assign (B, Bexp false), Seq (Assign (C, Bexp false), If (Vexp B, Assign (C, Bexp true), If (Vexp C, Assign (A, Vexp B), Assign (A, Vexp C)))))), []));;
  
let _ = assert (rana_M (list_of_string "i(a){d:=1}{;;;}") =
                  (If (Vexp A, Assign (D, Bexp true), Seq (Skip, Seq (Skip, Seq (Skip, Skip)))), [])) ;;

let _ = assert (rana_M (list_of_string "i(a+b.c){d:=1}{;;;}") =
                  (If (Bor (Vexp A, Band (Vexp B, Vexp C)), Assign (D, Bexp true),Seq (Skip, Seq (Skip, Seq (Skip, Skip)))),[]));;

let _ = assert (rana_M (list_of_string "i((!d)+a){d:=1}{;;;}") =
                  (If (Bor (Bnot (Vexp D), Vexp A), Assign (D, Bexp true),Seq (Skip, Seq (Skip, Seq (Skip, Skip)))),[]));;

(*** PARTIE PARTIE ***)

(*
  Exercice 2.1.4 (facultatif) Améliorer l’analyseur pour qu’il accepte des programmes avec des blancs
  arbitraires : espaces, indentations et retours à la ligne
*)
let rec clear = fun l ->
  match l with
  | [] -> []
  | ' '::l | '\n'::l | '\t'::l -> clear l
  | a::l -> a::(clear l)
;;

(*
   Parser en AST à partir d'un fichier
*)
let parse_from_file (filename:string) : stmt =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  let l = list_of_string s in
  let (p, _) = rana_M (clear l) in
  p;;

(*
   Parser en AST à partir d'une chaîne de caractères
*)
let parse_from_string (s:string) : stmt =
  let l = list_of_string s in
  let (p, _) = rana_M (clear l) in
  p;;

(* Usage : parse_from_file "./test.txt";; *)

(*** Interpreteur d'AST ***)

(*
  Fonction qui permet d'evaluer un statement *)

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
  
(* Test Interpreteur *)
let state = {a = true; b = false; c = true; d = true};;
let test_statment = parse_from_string "a:=1; b:=1; c:=1; w(a){ i(c) {c:=0;a:=b} {b:=0;c:=a} }";;

evalS test_statment state;;
let _ = assert (evalS test_statment state = {a = false; b = false; c = false; d = true});;

(*** Debut de la partie Optionnel 3.2 - Partiellement Traité par Romain MIRAS ***)

(* Changement en string de Vexp *)
type exp = Vexp of string | Bexp of bool | Bnot of exp | Band of exp * exp | Bor of exp * exp;;

(*
  Grammaire : 
  TOKEN ::= 'a' | .. | 'z' | 0 | .. | 9
  EXPR ::= TOKEN TOKEN'
  TOKEN' ::= TOKEN | epsilon

  Exemple :
*)

let is_token value : char option = 
  match value with
  | 'a' .. 'z' -> Some value
  | '0' .. '9' -> Some value
  | _ -> None;;

(*Retourne Char list *)
let token = fun l -> 
  l |> terminal_res is_token ++> fun x -> epsilon_res x;;

let rec p_expr = fun l ->
  l |> token ++> fun x -> tokenp [x]

(** Char list return **)
and tokenp = fun (acc:char list) l ->
  l |> token ++> fun x -> epsilon_res (acc@[x])
       +| epsilon_res acc;;

p_expr (list_of_string "abae")

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

p_horner (list_of_string "152")

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
