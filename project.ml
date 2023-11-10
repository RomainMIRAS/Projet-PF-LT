type var = A | B | C | D 

type exp = Vexp of var | Bexp of bool

type stmt =
  | Skip
  | Assign of var * exp
  | Seq of stmt * stmt
  | If of var * stmt * stmt
  | While of var * stmt;;
  
let (:=) v e = Assign (v, e)

let skip = Skip

let if_ e s1 s2 = If (e, s1, s2)

let while_ e s = While (e, s)

