let hipHurra () =
    let rec hurraAux = function
        | i when i > 20 -> 0
        | i -> 
            printfn "Hipp hipp hurra!"
            hurraAux (i+1)

    hurraAux 1

hipHurra ()