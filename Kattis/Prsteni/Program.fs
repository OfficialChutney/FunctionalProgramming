open System

Console.ReadLine() |> ignore

let circleRadii = 
    Console.ReadLine().Split " " 
    |> Array.toList 
    |> List.map int


let rec findGCD (a : int) (b : int) =
    match a with
    | 0 -> b
    | _ -> findGCD (b % a) a

let rec findTurn (radii : int list) (aux : int) =
    match aux > radii.Length-1 with
    | true -> ()
    | false -> 
        match aux = 0 with
        | true -> findTurn radii (aux+1)
        | false ->
            let gcd = findGCD radii[aux] radii[0]
            let printreturn = string(radii[0]/gcd) + "/" + string(radii[aux]/gcd)
            printfn "%s" printreturn
            findTurn radii (aux+1)

findTurn circleRadii 1