module Interpreter.Programs

    open Interpreter.Language
    
    let factorial x =
        Declare "result" />
        Declare "x" />
        ("result" .<-. Num 1) />
        ("x"      .<-. Num x) />
        While(Num 0 .<. Var "x",
              ("result" .<-. Var "result" .*. Var "x") />
              ("x"      .<-. Var "x" .-. Num 1))

    let factorial2 x =
        Declare "result" />
        Declare "x" />
        ("result" .<-. Num 1) />
        ("x"      .<-. Num x) />
        IT (Num 0 .<. Var "x",
            While(Num 0 .<. Var "x",
                  ("result" .<-. Var "result" .*. Var "x") />
                  ("x"      .<-. Var "x" .-. Num 1)))                           

    let factorial_err1 x =
        Declare "result" />
        Declare "result" />
        Declare "x" />
        ("result" .<-. Num 1) />
        ("x"      .<-. Num x) />
        While(Num 0 .<. Var "x",
              ("result" .<-. Var "result" .*. Var "x") />
              ("x"      .<-. Var "x" .-. Num 1))
        
    let factorial_err2 x =
        Declare "result" />
        Declare "x" />
        ("result" .<-. Num 1) />
        ("x"      .<-. Num x) />
        While(Num 0 .<. Var "x",
              ("result" .<-. Var "result" .*. Var "y") />
              ("x"      .<-. Var "x" .-. Num 1))
        
    let factorial_err3 x =
        Declare "result" />
        Declare "x" />
        ("result" .<-. Num 1) />
        ("x"      .<-. Num x) />
        While(Num 0 .<. Var "x",
              ("result" .<-. Var "result" ./. (Var "x" .-. Var "x")) />
              ("x"      .<-. Var "x" .-. Num 1))
        
    let factorialMem x =
        Declare "ptr" />
        Alloc("ptr", Num x) />
        Declare "x" />
        ("x" .<-. Num x) />
        While(Num 0 .<. Var "x",
              MemWrite(Var "ptr" .+. (Var "x" .-. Num 1), Var "x") />
              ("x" .<-. Var "x" .-. Num 1)) />
        ("x" .<-. Num (x - 1)) />
        Declare "result" />
        ("result" .<-. Num 1) />
        While(Num 0 .<=. Var "x",
              ("result" .<-. Var "result" .*. MemRead(Var "ptr" .+. Var "x")) />
              ("x"      .<-. Var "x" .-. Num 1)) />
        Free(Var "ptr", Num x)
        
    let factorialMem_err1 x =
        Declare "ptr" />
        Alloc("ptr", Num (x - 1)) />
        Declare "x" />
        ("x" .<-. Num x) />
        While(Num 0 .<. Var "x",
              MemWrite(Var "ptr" .+. (Var "x" .-. Num 1), Var "x") />
              ("x" .<-. Var "x" .-. Num 1)) />
        ("x" .<-. Num (x - 1)) />
        Declare "result" />
        ("result" .<-. Num 1) />
        While(Num 0 .<=. Var "x",
              ("result" .<-. Var "result" .*. MemRead(Var "ptr" .+. Var "x")) />
              ("x"      .<-. Var "x" .-. Num 1)) />
        Free(Var "ptr", Num x)

    let factorialMem_err2 x =
        Declare "ptr" />
        Alloc("ptr", Num x) />
        Declare "x" />
        ("x" .<-. Num x) />
        While(Num 0 .<. Var "x",
              MemWrite(Var "ptr" .+. (Var "x" .-. Num 1), Var "x") />
              ("x" .<-. Var "x" .-. Num 1)) />
        ("x" .<-. Num (x - 1)) />
        Declare "result" />
        ("result" .<-. Num 1) />
        While(Num 0 .<=. Var "x",
              ("result" .<-. Var "result" .*. MemRead(Var "ptr" .+. Var "x")) />
              ("x"      .<-. Var "x" .-. Num 1)) />
        Free(Var "ptr", Num (x + 1))

    let factorialMem_err3 x =
        Declare "ptr" />
        Alloc("ptr", Num x) />
        Declare "x" />
        ("x" .<-. Num x) />
        While(Num 0 .<. Var "x",
              MemWrite(Var "ptr" .+. Var "x", Var "x") />
              ("x" .<-. Var "x" .-. Num 1)) />
        ("x" .<-. Num (x - 1)) />
        Declare "result" />
        ("result" .<-. Num 1) />
        While(Num 0 .<=. Var "x",
              ("result" .<-. Var "result" .*. MemRead(Var "ptr" .+. Var "x")) />
              ("x"      .<-. Var "x" .-. Num 1)) />
        Free(Var "ptr", Num x)


    let factorialMem_err4 x =
        Declare "ptr" />
        Alloc("ptr", Num x) />
        Declare "x" />
        ("x" .<-. Num x) />
        While(Num 0 .<. Var "x",
              MemWrite(Var "ptr" .+. (Var "x" .-. Num 1), Var "x") />
              ("x" .<-. Var "x" .-. Num 1)) />
        ("x" .<-. Num (x - 1)) />
        Declare "result" />
        ("result" .<-. Num 1) />
        While(Num 0 .<=. Var "x",
              ("result" .<-. Var "result" .*. MemRead(Var "ptr" .+. Var "x" .+. Num 1)) />
              ("x"      .<-. Var "x" .-. Num 1)) />
        Free(Var "ptr", Num x)
                                            
    let factorialMem_err5 x =
        Declare "ptr" />
        Alloc("ptr", Num -x) />
        Declare "x" />
        ("x" .<-. Num x) />
        While(Num 0 .<. Var "x",
              MemWrite(Var "ptr" .+. (Var "x" .-. Num 1), Var "x") />
              ("x" .<-. Var "x" .-. Num 1)) />
        ("x" .<-. Num (x - 1)) />
        Declare "result" />
        ("result" .<-. Num 1) />
        While(Num 0 .<=. Var "x",
              ("result" .<-. Var "result" .*. MemRead(Var "ptr" .+. Var "x")) />
              ("x"      .<-. Var "x" .-. Num 1)) />
        Free(Var "ptr", Num x)

    let factorialMem_err6 x =
        Declare "ptr" />
        Alloc("ptr", Num x) />
        Declare "x" />
        ("x" .<-. Num x) />
        While(Num 0 .<. Var "y",
              MemWrite(Var "ptr" .+. (Var "x" .-. Num 1), Var "x") />
              ("x" .<-. Var "x" .-. Num 1)) />
        ("x" .<-. Num (x - 1)) />
        Declare "result" />
        ("result" .<-. Num 1) />
        While(Num 0 .<=. Var "x",
              ("result" .<-. Var "result" .*. MemRead(Var "ptr" .+. Var "x")) />
              ("x"      .<-. Var "x" .-. Num 1)) />
        Free(Var "ptr", Num x)

                                            