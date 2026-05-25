open System

let _ = Console.ReadLine ()

let line = 
    Console.ReadLine().Split(" ") 
    |> Array.map int 
    |> Array.fold (fun acc elem -> (elem |> int) + acc) 0
    |> printfn "%d" 
