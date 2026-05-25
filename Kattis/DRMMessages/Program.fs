open System

let input = Console.ReadLine()

let decrypt (str : string) =

    let charList = str.ToCharArray() |> Array.toList

    let splitArray = List.splitAt (charList.Length / 2) charList

    let charToNum (c : char) = int (System.Char.ToUpper c) - 65

    let numToChar (num : int) = char ((num % 26) + 65) 

    let rec getRotationNum (arr : char list) (total : int) =
        match arr with
        | [] -> total
        | x::xs -> getRotationNum xs (total + charToNum x)

    let rec rotate (arr : char list) (rotationNum : int) =
        match arr with
        | [] -> []
        | x::xs -> (numToChar ((charToNum x) + rotationNum))::(rotate xs rotationNum)

    let rec merge (lists : char list * char list) =
        match lists with
        | ([], []) -> []
        | (frst::fstTail, scnd::scndTail) ->
            (numToChar ((charToNum frst) + (charToNum scnd)))::(merge (fstTail, scndTail))
        | (_, _) -> []

    let rotated = (
        rotate (fst splitArray) (getRotationNum (fst splitArray) 0),  
        rotate (snd splitArray) (getRotationNum (snd splitArray) 0))

    let merged = merge rotated

    List.fold (fun (acc : string) (elem : char) -> acc + string elem) "" merged


printfn "%s" (decrypt input)