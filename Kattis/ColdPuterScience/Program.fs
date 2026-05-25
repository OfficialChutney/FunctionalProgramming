open System

Console.ReadLine()

let numArray = 
    Console.ReadLine().Split " " 
    |> Array.map int
    |> Array.fold (fun acc elem -> if elem < 0 then (acc+1) else acc) 0
    |> printfn "%d"