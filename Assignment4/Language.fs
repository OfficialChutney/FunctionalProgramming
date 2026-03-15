module Interpreter.Language

type error =
    | DivisionByZero

type aexpr =
    | Num of int
    | Add of aexpr * aexpr
    | Mul of aexpr * aexpr
    | Div of aexpr * aexpr
    
let (.+.) a b = Add (a, b)
let (.*.) a b = Mul (a, b)
let (.-.) a b = a .+. (b .*. Num -1)    (* Minus defined as adding a negative number *)
let (./.) a b = Div (a, b)
let (.%.) a b = a .-. ((a ./. b) .*. b)   (* Modulo a%b = a - (a / b) * b *)
    
type bexpr =
    | TT
    | Eq of aexpr * aexpr
    | Lt of aexpr * aexpr
    | Conj of bexpr * bexpr
    | Not of bexpr
    
let FF = Not TT
let (~~) b = Not b
let (.&&.) b1 b2 = Conj (b1, b2)
let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2)       (* boolean disjunction *)

let (.=.)  a b = Eq (a, b)   
let (.<.)  a b = Lt (a, b)   
let (.<>.) a b = ~~(a .=. b)                (* numeric inequality *)
let (.<=.) a b = (a .<. b) .||. (a .=. b)   (* numeric smaller than or equal to *)
let (.>=.) a b = ~~(a .<. b)                (* numeric greater than or equal to *)
let (.>.)  a b = (a .<>. b) .&&. (a .>=. b) (* numeric greater than *)