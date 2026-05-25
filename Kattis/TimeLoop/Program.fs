open System

let num = Console.ReadLine () |> int


let loop (x : int) : unit =

    let rec loopAux (i : int) (x : int) =
        match i with 
        | i when i > x -> ""
        | i -> 
            printfn "%s" ((i |> string ) + " Abracadabra")
            loopAux (i+1) x

    loopAux 1 x

    ()

loop num
