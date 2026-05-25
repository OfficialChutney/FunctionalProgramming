open System


let num = Console.ReadLine() |> int


let rec getNumOfButDays (num : int) (i : int) (total : int) =
    match i >= num with
    | true -> if total = 0 then "I must watch Star Wars with my daughter" else string(total)
    | false ->
        let input = Console.ReadLine().ToLowerInvariant()
        match input.Contains "pink" || input.Contains "rose" with
        | true -> getNumOfButDays num (i+1) (total+1)
        | false -> getNumOfButDays num (i+1) (total)

printfn "%s" (getNumOfButDays num 0 0)