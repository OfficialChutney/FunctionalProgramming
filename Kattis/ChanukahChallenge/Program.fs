
open System

let numOfInput = Console.ReadLine() |> int

let rec getChanukah (aux : int) = 

    let rec getCandleNum (numOfDays : int) (aux : int) = 
        match aux > numOfDays with
        | true -> 0
        | false -> (aux+1) + getCandleNum numOfDays (aux+1)

    match aux > numOfInput with
    | true -> ()
    | false ->
        let line = (Console.ReadLine().Split " ") |> Array.map int
        printfn "%s" ((string line.[0]) + " " + (string (getCandleNum line.[line.Length-1] 1)))
        getChanukah (aux+1)

getChanukah 1