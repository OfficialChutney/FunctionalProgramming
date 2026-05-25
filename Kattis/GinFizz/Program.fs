open System

let num = Console.ReadLine() |> int

printfn "%s" (string (num*45) + " ml gin")
printfn "%s" (string (num*30) + " ml fresh lemon juice")
printfn "%s" (string (num*10) + " ml simple syrup")
printfn "%s" (string (num*1) + " slice" + (if num > 1 then "s" else "") + " of lemon")