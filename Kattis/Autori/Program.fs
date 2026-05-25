open System

let concatUppers (acc : string) (elem : char) = if System.Char.IsUpper elem then acc + string elem else acc  

let uppers = 
    Console.ReadLine().ToCharArray()
    |> Array.fold concatUppers ""

printfn "%s" uppers