open System


let num = (Console.ReadLine().Split " ")[0] |> int

let rec getMessage (num : int) (i : int) (acc : string) =
    match i > num with
    | true -> acc
    | false -> 
            let getStr (acc : string) (elem : char) =
                match elem = '.' with
                | true -> acc
                | false -> acc + string elem

            let list = Console.ReadLine().ToCharArray() |> Array.toList

            getMessage num (i+1) (List.fold getStr acc list)

printfn "%s" (getMessage num 1 "")