open System


let line = Console.ReadLine () |> int

let convertToBit () : unit =

    let rec convertToBitAux (num : int) (acc : int list) = 
        match num with
        | 0 -> acc
        | num -> convertToBitAux (num / 2) ((num % 2)::acc)


    let rec convertToNum (list : int list) (i : int) =
        match i > list.Length-1 with
        | true -> 0
        | false -> int (Math.Floor(Math.Pow(2.0, float(i)))) * list.[i] + convertToNum list (i+1)


    printfn "%d" (convertToNum(convertToBitAux line []) 0)
    


convertToBit ()