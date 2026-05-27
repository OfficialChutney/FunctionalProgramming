module simpleParser
open JParsec
open JParsec.TextParser

(*
E ::= T + E | T
T ::= N * T | N
N ::= integer | (E)
*)

type calc =
    | Num of int
    | Add of calc * calc
    | Mul of calc * calc


    // Start from the bottom — atoms first

let pE, eref = createParserForwardedToRef<calc>()
let pT, (tref: Parser<calc> ref) = createParserForwardedToRef<calc>()


// N ::= integer | (E)
let pNum = pint32 |>> Num
let pPar = pchar '(' >>. pE .>> pchar ')'
let pN   = pNum <|> pPar

// T ::= N * T | N
let pMul = pN .>> pchar '*' .>>. pT |>> Mul
do tref := pMul <|> pN

// E ::= T + E | T
let pAdd = pT .>> pchar '+' .>>. pE |>> Add 
do eref := pAdd <|> pT

