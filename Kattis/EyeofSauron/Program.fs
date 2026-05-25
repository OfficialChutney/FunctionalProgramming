open System

let line = Console.ReadLine()

let eyeOfSauron (input : string) =

    let rec needFix (firstSide : char array) (secondSide : char array) (aux : int) =
        match aux > firstSide.Length-1 with
        | true -> "correct"
        | false -> 
            match (firstSide.[aux] = '|' && secondSide.[aux] = '|') || (firstSide.[aux] = '(' && secondSide.[aux] = ')') with
            | true -> needFix firstSide secondSide (aux+1)
            | false -> "fix"
        
    let charArray = input.ToCharArray ()

    match charArray.Length % 2 = 0 with
    | true -> 

        let splitList = Array.splitAt ((charArray.Length-1) / 2) charArray
        needFix (fst splitList) (Array.rev (snd splitList)) 0
    | false -> "fix"

printfn "%s" (eyeOfSauron line)
