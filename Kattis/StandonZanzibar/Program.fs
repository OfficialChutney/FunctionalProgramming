open System

let num = Console.ReadLine() |> int

let rec nonBornTurtles (num : int) (aux : int) =

    let rec nonBornTurleAux (booklet : int array) (totals : int) (i : int) =
        match i > booklet.Length-1 with
        | true -> totals
        | false ->



            let getOnborn (num : int) = 
                match num with
                | num when num < 0 -> 0
                | num -> num

            match i with
            | 0 -> nonBornTurleAux booklet totals (i+1)
            | _ -> 
                let newtotals = totals + getOnborn(booklet[i] - booklet[i-1] * 2)
                nonBornTurleAux booklet newtotals (i+1)

    

    match aux >= num with
    | true -> ()
    | false -> 
        let array = Console.ReadLine().Split " " |> Array.map int

        printfn "%d" (nonBornTurleAux array 0 0)
        
        nonBornTurtles num (aux+1)


nonBornTurtles num 0