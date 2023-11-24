(*
B  ::= ’0’ | ’1’
V  ::= ’a’ | ’b’ | ’c’ | ’d’
E  ::= B | V
ET ::= F Z Q
Q  ::= ST | SE | epsilon
SE ::= '+' Z ET
ST ::= '.’ Z ET
F  ::= ’!’ Z F | Z E | ’(’ Z ET Z ’)' Z Q
S' ::= ';' Z M | ε
S ::= V Z ":=" Z ET
    | i Z ’(’ Z V Z ’)’ Z ’{’ Z M Z ’}’ Z ’{’ Z M Z ’}’  
    | w Z ’(’ Z V Z ’)’ Z ’{’ Z M Z ’}’ | ε
M ::= Z S Z S' Z
Z ::= ' ' Z | '\n' Z | '\t' Z | epsilon 
*)

#use "anacomb.ml";;

let is_var (c:char) : bool = match c with
  | 'a' | 'b' | 'c' | 'd' -> true
  | _ -> false;;

let is_bool (c:char) : bool = match c with
  | '1' | '0' -> true
  | _ -> false;;

let ana_B = fun l -> 
  l |> terminal_cond is_bool;;

let ana_V = fun l -> 
  l |> terminal_cond is_var;;

let ana_E = fun l -> 
  l |> ana_V -| ana_B ;;

let rec ana_Z = fun l ->
  l |> (terminal ' ' --> ana_Z)
  -| (terminal '\n' --> ana_Z)
  -| (terminal '\t' --> ana_Z)
  -| epsilon

let rec ana_ET = fun l -> 
  l |> ana_F --> ana_Z --> ana_Q
  
and ana_Q = fun l ->
  l |> (ana_ST --> ana_Q)
  -| (ana_SE --> ana_Q)
  -| epsilon

and ana_SE = fun l ->
  l |> terminal '+' --> ana_Z --> ana_ET

and ana_ST = fun l ->
  l |> terminal '.' --> ana_Z --> ana_ET

and ana_F = fun l ->
  l |> (terminal '!' --> ana_Z --> ana_F)
  -| (ana_Z --> ana_E)
  -| (terminal '(' --> ana_Z --> ana_ET --> ana_Z -->terminal ')' --> ana_Z --> ana_Q)

and ana_Sp = fun l ->
  l |> (terminal ';' --> ana_Z -->ana_M)
  -| epsilon

and ana_M = fun l -> 
  l |> ana_Z --> ana_S --> ana_Z --> ana_Sp --> ana_Z

and ana_S = fun l ->
  l |> (terminal 'i' --> ana_Z --> terminal '(' --> ana_Z --> ana_V --> ana_Z --> terminal ')' --> ana_Z --> terminal '{' --> ana_Z --> ana_M --> ana_Z --> terminal '}' --> ana_Z --> terminal '{' --> ana_Z--> ana_M --> ana_Z --> terminal '}')
  -| ( terminal 'w' --> ana_Z --> terminal '(' --> ana_Z --> ana_V --> ana_Z --> terminal ')' --> ana_Z --> terminal '{' --> ana_Z --> ana_M --> ana_Z --> terminal '}')
  -| (ana_V --> ana_Z --> terminal ':' --> terminal '=' --> ana_Z --> ana_ET)
  -| (epsilon);;

let _ = assert (ana_M (list_of_string "  a    :=   1
;  w  ( a )     {

}" ) = []);;
let _ = assert (ana_M (list_of_string "   a
:=
1
;
i
(
  a
  )
  {
    b
    :=
    1
    }
    {
      b
      :=
      0
      
      }       ") = []);;
let _ = assert (ana_M (list_of_string "a:=1;b:=0;w(a){i(b){b:=0}{b:=1}}") = []);;
let _ = assert (ana_M (list_of_string "a:=1;b:=0;w(a){i(b){c:=0}{a:=0;a:=1}}") = []);;
let _ = assert (ana_M (list_of_string "a:=1;b:=0;c:=0;i(b){c:=1}{i(c){a:=b}{a:=c}}") = []);;
let _ = assert (ana_M (list_of_string "i(a){d:=1}{;;;}") = []);;


(*
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
*)

#use "anacomb.ml";;

let is_var (c:char) : bool = match c with
  | 'a' | 'b' | 'c' | 'd' -> true
  | _ -> false;;

let is_bool (c:char) : bool = match c with
  | '1' | '0' -> true
  | _ -> false;;

let ana_B = fun l -> 
  l |> terminal_cond is_bool;;

let ana_V = fun l -> 
  l |> terminal_cond is_var;;

let ana_E = fun l -> 
  l |> ana_V -| ana_B ;;

let rec ana_ET = fun l -> 
  l |> ana_F --> ana_Q
  
and ana_Q = fun l ->
  l |> (ana_ST --> ana_Q)
  -| (ana_SE --> ana_Q)
  -| epsilon

and ana_SE = fun l ->
  l |> terminal '+' --> ana_ET

and ana_ST = fun l ->
  l |> terminal '.' --> ana_ET

and ana_F = fun l ->
  l |> (terminal '!' --> ana_F)
  -| (ana_E)
  -| (terminal '(' --> ana_ET -->terminal ')' --> ana_Q)

and ana_Sp = fun l ->
  l |> (terminal ';' -->ana_M)
  -| epsilon

and ana_M = fun l -> 
  l |> ana_S --> ana_Sp

and ana_S = fun l ->
  l |> (terminal 'i' --> terminal '(' --> ana_V --> terminal ')' --> terminal '{' --> ana_M --> terminal '}' --> terminal '{' --> ana_M --> terminal '}')
  -| ( terminal 'w' --> terminal '(' --> ana_V --> terminal ')' --> terminal '{' --> ana_M --> terminal '}')
  -| (ana_V --> terminal ':' --> terminal '=' --> ana_ET)
  -| (epsilon);;

let rec clear = fun l ->
  match l with
  | [] -> []
  | ' '::l | '\n'::l | '\t'::l -> clear l
  | a::l -> a::(clear l)
;;

let ana_Start = fun l ->
  ana_M (clear l)
;;

let _ = assert (ana_M (list_of_string "  a    :=   1
;  w  ( a )     {

}" ) = []);;
let _ = assert (ana_M (list_of_string "   a
:=
1
;
i
(
  a
  )
  {
    b
    :=
    1
    }
    {
      b
      :=
      0
      
      }       ") = []);;
let _ = assert (ana_M (list_of_string "a:=1;b:=0;w(a){i(b){b:=0}{b:=1}}") = []);;
let _ = assert (ana_M (list_of_string "a:=1;b:=0;w(a){i(b){c:=0}{a:=0;a:=1}}") = []);;
let _ = assert (ana_M (list_of_string "a:=1;b:=0;c:=0;i(b){c:=1}{i(c){a:=b}{a:=c}}") = []);;
let _ = assert (ana_M (list_of_string "i(a){d:=1}{;;;}") = []);;

