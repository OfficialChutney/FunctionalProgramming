// For more information see https://aka.ms/fsharp-console-apps
module Interpreter.Program

    open Language
    open Eval



    printfn "%A" (bexprToString2 ((Num 42 .<. Num 32 .+. Num 10) .||. (Num 32 .>=. Num 27 .%. Num 25)))