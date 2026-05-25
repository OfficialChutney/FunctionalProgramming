open System


let firstLine = Console.ReadLine ()
let secondLine = Console.ReadLine () |> int

let redupe (x : int) (str : string) =
    
    

    let rec redupeAux (x : int) (i : int) (str : string) =
        match i with
        | i when i = x -> ""
        | _ -> str + redupeAux x (i+1) str

    redupeAux x 0 str

printfn "%s" (redupe secondLine firstLine)