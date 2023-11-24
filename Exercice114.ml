(*
Grammaire de base :

C ::= ’0’ | ’1’
V ::= ’a’ | ’b’ | ’c’ | ’d’
A ::= C | V
E ::= E ’+’ T | T
T ::= T ’.’ F | F
F ::= ’!’ F | A | ’(’ E ’)'

Grammaire sans récursivité à gauche : (le début est ET)
C  ::= ’0’ | ’1’
V  ::= ’a’ | ’b’ | ’c’ | ’d’
A  ::= C | V
ET ::= F Q
Q  ::= ST | SE | epsilon
SE ::= '+' ET
ST ::= '.’ ET
F  ::= ’!’ F | A | ’(’ ET ’)' Q

Version Merge avec celle d'avant
B  ::= ’0’ | ’1’
V  ::= ’a’ | ’b’ | ’c’ | ’d’
E  ::= B | V
ET ::= F Q
Q  ::= ST | SE | epsilon
SE ::= '+' ET
ST ::= '.’ ET
F  ::= ’!’ F | E | ’(’ ET ’)' Q
S' ::= ';' M | ε
S ::= V ":=" ET
    | i ’(’ V ’)’ ’{’ M ’}’ ’{’ M ’}’  
    | w ’(’ V ’)’ ’{’ M ’}’ | ε
M ::= S S'


## Version Romain
C ::= ’0’ | ’1’
V ::= ’a’ | ’b’ | ’c’ | ’d’
A ::= C | V
E ::= T E’
E’ ::= ’+’ T E’ | ε
T ::= F T’
T’ ::= ’.’ F T’ | ε
F ::= ’!’ F | A | ’(’ E ’)'

B ::= ’0’ | ’1’
V ::= ’a’ | ’b’ | ’c’ | ’d’
E ::= B | V
ET ::= T E’
E’ ::= ’+’ T E’ | ε
T ::= F T’
T’ ::= ’.’ F T’ | ε
F ::= ’!’ F | E | ’(’ ET ’)'
S' ::= ; M | ε
S ::= V := ET
    | i ’(’ V ’)’ ’{’ M ’}’ ’{’ M ’}’  
    | w ’(’ V ’)’ ’{’ M ’}’ | ε
M ::= S S'

*)
#use "anacomb.ml";;

let is_var :char -> bool = fun c ->
  match c with
  | 'a' | 'b' | 'c' | 'd' -> true
  | _ -> false
;;

let is_bool : char -> bool = fun c ->
  match c with
  | '1' | '0' -> true
  | _ -> false
;;

let ana_C = fun l ->
  l |> terminal_cond is_bool
;;

let ana_V = fun l ->
  l |> terminal_cond is_var
;;

let ana_A = fun l ->
  l |>
  ana_C -| ana_V
;;


let rec ana_ET = fun l ->
  l |> ana_F --> ana_Q

and ana_F = fun l ->
  l |> (terminal '!' --> ana_F)
  -|
  ana_A
  -|
  (terminal '(' --> ana_ET --> terminal ')' --> ana_Q)

and ana_Q = fun l ->
  l |> ana_ST
  -|
  ana_SE
  -|
  epsilon
  
and ana_ST = fun l ->
  l |> terminal '.' --> ana_ET
  
and ana_SE = fun l ->
  l |> terminal '+' --> ana_ET
;;

let rec ana_Sp = fun l ->
  l |> (terminal ';' --> ana_M)
  -|
  epsilon

and ana_S = fun l ->
  l |>
  (ana_V --> terminal ':' --> terminal '=' --> ana_ET)
  -|
  (terminal 'i' --> terminal '(' --> ana_V --> terminal ')' --> terminal '{' --> ana_M --> terminal '}' --> terminal '{' --> ana_M --> terminal '}')
  -|
  (terminal 'w' --> terminal '(' --> ana_V --> terminal ')' --> terminal '{' --> ana_M --> terminal '}')
  -|
  epsilon

and ana_M = fun l ->
  l |> ana_S --> ana_Sp
;;

ana_C (list_of_string "1" );;
ana_V (list_of_string "a+b" );;
ana_A (list_of_string "a" );;
ana_ET (list_of_string "a+b.c+(1.d+(!0))" );;
ana_ET (list_of_string "!!!!!!(((((((((((((1)))))))))))))" );;
ana_M (list_of_string "a:=1");;