open System
let doesNotOverlap (x : int * int) (y : int * int) =
    match fst x = fst y && snd x = snd y with
    | true -> false
    | false -> 
        if snd y <= fst x || snd x <= fst y then
            true
        else
            false

let getIntervals (num : int) = 
    let rec getIntervalsAux (num : int) (i : int) (acc : (int * int) list) = 
        match i > num with
        | true -> acc
        | false ->
            let array = 
                Console.ReadLine().Split " "
                |> Array.map int
                |> Array.toList
            
            getIntervalsAux num (i+1) ((array[0], array[1])::acc)


    List.sortBy (fun (x,y) -> y) 
        (getIntervalsAux num 1 List.Empty)


let getMaxNonOverlap (list : (int * int) list) =

    let rec checkOverlap (item : int * int) (list : (int * int) list) (total : int) =
        match list with
        | [] -> total
        | x::xs ->
            if doesNotOverlap item x then
                checkOverlap x xs (total+1)
            else
                checkOverlap item xs total

    checkOverlap (List.head list) (List.tail list) 1

let num = Console.ReadLine() |> int

let intervals = getIntervals num

printfn "%d" (getMaxNonOverlap intervals)