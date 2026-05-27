(* Load this file into the interacive envorinment
   (select all and Alt-Enter in VS).
   
   Cut-and pasting these lines will typically not work unless you provide
   the entire path in the #load command. 

   Some IDEs may still complain about the path, place the full path here if that is the case.
*)

#load "JParsec.fs"
#load "Exam.fs"
open Exam2025_Template.Exam
open JParsec.TextParser;;

run parseMoves
   "Player X places a tile on row midRow and column midCol
   Player O places a tile on row topRow and column leftCol
   Player X places a tile on row topRow and column rightCol
   Player O places a tile on row midRow and column leftCol
   Player X places a tile on row botRow and column leftCol"
   |> Result.map (playGame >> evalTTT >> Result.toOption >> Option.get)