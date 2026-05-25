open System


let num = Console.ReadLine() |> int

let rec getLowestTime (num : int) (i : int) (recordedTime : int64) =
    match i > num with
    | true -> recordedTime
    | false ->
        let line = Console.ReadLine().Split " "
        let time = line.[0] |> int64
        let status = line[1] |> int

        match recordedTime > time && status = 0 with
        | true -> getLowestTime num (i+1) time
        | false -> getLowestTime num (i+1) recordedTime


match getLowestTime num 1 Int64.MaxValue with
| Int64.MaxValue -> printfn "%d" -1
| time -> printfn "%d" time