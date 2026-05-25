open System

let numOfCases = Console.ReadLine() |> int


let rec lastFact (aux : int) : unit =
    
    let rec fact (aux : int) (num : int)  =
        match aux with
        | aux when aux = num -> num
        | aux -> aux * (fact (aux+1) num)
    
    
    match aux with
    | aux when aux = numOfCases -> ()
    | aux -> 
        let num = Console.ReadLine() |> int
        
        let factAsString = (fact 1 num |> string)

        printfn "%c" factAsString.[factAsString.Length-1]

        lastFact (aux+1)

lastFact 0
