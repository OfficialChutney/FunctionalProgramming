open Exam2025_Template.Exam

let testQ34_block () =
    let table = newTable 5
    philEat table 0
    philEat table 2
    philThink table 0
    philEat table 1 // this line should block
    
let testQ34_succeed () =
    let table = newTable 5
    philEat table 0
    philEat table 2
    philThink table 2
    philThink table 0
    philEat table 1
    printfn "Everyone is done eating (well, philosopher one is still eating)"
    
let runDiningPhilosophers () = diningPhilosophers 5 2 1000

// put the code that you want to test here.

() |> testQ34_block |> printfn "%A"