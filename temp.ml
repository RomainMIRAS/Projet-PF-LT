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