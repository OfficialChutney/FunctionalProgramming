#load "Language.fs"
#load "Eval.fs"

open Interpreter.Language
open Interpreter.Eval


// ___________________________________________________________________________________
// TEST EXERCISE 4.1
// ___________________________________________________________________________________

aexprToString (Num 4)
//- val it: string = "4"

aexprToString (Num 4 .+. Num 2 .*. Num 3)
//- val it: string = "(4 + (2 * 3))"
  
aexprToString ((Num 4 .+. Num 2) .*. Num 3)
//- val it: string = "((4 + 2) * 3)"
  
aexprToString ((Num 18 ./. Num 3) ./. Num 2)
//- val it: string = "((18 / 3) / 2)"

aexprToString (Num 18 ./. (Num 3 ./. Num 2))
//- val it: string = "(18 / (3 / 2))"

aexprToString (Num 42 .*. (Num 13 .%. Num 3))
//- val it: string = "(42 * (13 + (((13 / 3) * 3) * -1)))"


// ___________________________________________________________________________________
// TEST EXERCISE 4.2
// ___________________________________________________________________________________

bexprToString TT
// - val it: string = "true"

bexprToString FF
// - val it: string = "(not true)"

bexprToString (Num 42 .=. Num 32)
// - val it: string = "(42 = 32)"

bexprToString (Num 42 .<. Num 32 .+. Num 10)
// - val it: string = "(42 < (32 + 10))"

printfn "%s" (bexprToString  ((Num 42 .<. Num 32 .+. Num 10) .||. (Num 32 .>=. Num 27 .%. Num 25)))
// - val it: string = "(not ((not (42 < (32 + 10))) /\ (not (not (32 < (27 + (((27 / 25) * 25) * -1)))))))"

// ___________________________________________________________________________________
// TEST EXERCISE 4.3
// ___________________________________________________________________________________

aexprEval (Num 4)
// - val it: int option = Some 4

aexprEval (Num 4 .+. Num 2 .*. Num 3)
// - val it: int option = Some 10

aexprEval ((Num 4 .+. Num 2) .*. Num 3)
// - val it: int option = Some 18

aexprEval ((Num 4 .+. Num 2) ./. Num 0)
// - val it: int option = None

aexprEval (Num 42 .*. (Num 13 .%. Num 3))
// - val it: int option = Some 42

aexprEval (Num 42 .*. (Num 13 .%. Num 0))
// - val it: int option = None

// ___________________________________________________________________________________
// TEST EXERCISE 4.4
// ___________________________________________________________________________________

aexprEval2 (Num 4)
// - val it: int option = Some 4

aexprEval2 (Num 4 .+. Num 2 .*. Num 3)
// - val it: int option = Some 10

aexprEval2 ((Num 4 .+. Num 2) .*. Num 3)
// - val it: int option = Some 18

aexprEval ((Num 4 .+. Num 2) ./. Num 0)
// - val it: int option = None

aexprEval (Num 42 .*. (Num 13 .%. Num 3))
// - val it: int option = Some 42

aexprEval (Num 42 .*. (Num 13 .%. Num 0))
// - val it: int option = None

// ___________________________________________________________________________________
// TEST EXERCISE 4.5
// ___________________________________________________________________________________

bexprEval TT
// - val it: bool option = Some true

bexprEval FF
// - val it: bool option = Some false

bexprEval (Num 42 .=. Num 32)
// - val it: bool option = Some false

bexprEval (Num 42 .<. Num 32 .+. Num 10)
// - val it: bool option = Some false
  
bexprEval  ((Num 42 .<. Num 32 .+. Num 10) .||. (Num 32 .>=. Num 27 .%. Num 25))
// - val it: bool option = Some true

// ___________________________________________________________________________________
// TEST EXERCISE 4.6
// ___________________________________________________________________________________

aexprToString2 (Num 4 .+. Num 2 .*. Num 3)
// - val it: string = "4 + 2 * 3"

aexprToString2 ((Num 4 .+. Num 2) .*. Num 3)
// - val it: string = "(4 + 2) * 3"

aexprToString2 ((Num 18 ./. Num 3) ./. Num 2)
// - val it: string = "18 / 3 / 2"

aexprToString2 (Num 18 ./. (Num 3 ./. Num 2))
// - val it: string = "18 / (3 / 2)"

aexprToString2 (Num 42 .*. (Num 13 .%. Num 3))
// - val it: string = "42 * (13 + 13 / 3 * 3 * -1)"

// ___________________________________________________________________________________
// TEST EXERCISE 4.7
// ___________________________________________________________________________________

bexprToString2 TT
// - val it: string = "true"

bexprToString2 FF
// - val it: string = "not true"

bexprToString2 (Num 42 .=. Num 32)
// - val it: string = "42 = 32"

bexprToString2 (Num 42 .<. Num 32 .+. Num 10)
// - val it: string = "42 < 32 + 10"

printfn "%s" (bexprToString2  ((Num 42 .<. Num 32 .+. Num 10) .||. (Num 32 .>=. Num 27 .%. Num 25)))
// - val it: string = "not (not (42 < 32 + 10) /\ not (not (32 < 27 + 27 / 25 * 25 * -1)))"


// ___________________________________________________________________________________
// TEST EXERCISE 4.10
// ___________________________________________________________________________________

aexprEval3 (Num 4)
// - val it: int option = Ok 4

aexprEval3 (Num 4 .+. Num 2 .*. Num 3)
// - val it: int option = Ok 10

aexprEval3 ((Num 4 .+. Num 2) .*. Num 3)
// - val it: int option = Ok 18

aexprEval3 ((Num 4 .+. Num 2) ./. Num 0)
// - val it: int option = Error DivisionByZero

aexprEval3 (Num 42 .*. (Num 13 .%. Num 3))
// - val it: int option = Ok 42

aexprEval3 (Num 42 .*. (Num 13 .%. Num 0))
// - val it: int option = Error DivisionByZero


// ___________________________________________________________________________________
// TEST EXERCISE 4.11
// ___________________________________________________________________________________


aexprEval4 (Num 4)
// - val it: int option = Ok 4

aexprEval4 (Num 4 .+. Num 2 .*. Num 3)
// - val it: int option = Ok 10

aexprEval4 ((Num 4 .+. Num 2) .*. Num 3)
// - val it: int option = Ok 18

aexprEval4 ((Num 4 .+. Num 2) ./. Num 0)
// - val it: int option = Error DivisionByZero

aexprEval4 (Num 42 .*. (Num 13 .%. Num 3))
// - val it: int option = Ok 42

aexprEval4 (Num 42 .*. (Num 13 .%. Num 0))
// - val it: int option = Error DivisionByZero


// ___________________________________________________________________________________
// TEST EXERCISE 4.12
// ___________________________________________________________________________________

bexprEval2 TT
// - val it: bool option = Ok true

bexprEval2 FF
// - val it: bool option = Ok false

bexprEval2 (Num 42 .=. Num 32)
// - val it: bool option = Ok false

bexprEval2 (Num 42 .<. Num 32 .+. Num 10)
// - val it: bool option = Ok false
  
bexprEval2  ((Num 42 .<. Num 32 .+. Num 10) .||. (Num 32 .>=. Num 27 .%. Num 25))
// - val it: bool option = Ok true

// ___________________________________________________________________________________
// TEST EXERCISE 4.15
// ___________________________________________________________________________________

simpleAEval (Num 42)
//- val it: int = 42

simpleAEval (Num 42 ./. Num 2)
//- val it: int = 21

// simpleAEval (Num 42 ./. Num 0)
//!!! Crashes because of division by zero


// ___________________________________________________________________________________
// TEST EXERCISE 4.16
// ___________________________________________________________________________________

simpleBEval TT
//- val it: bool = true

simpleBEval (Num 4 .>. Num 32 .%. Num 2)
// - val it: bool = true

simpleBEval (Num 4 .>. Num 32 .%. Num 0)
// !!! Crashes because of division by zero