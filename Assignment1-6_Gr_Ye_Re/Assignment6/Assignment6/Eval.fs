module Interpreter.Eval

    open Result
    open Language
    open State
    
    // Exercise 6.14
    // Modify aexprEval, which still has type aexpr -> state -> int option such that given
    // an arithmetic expression a and a state st returns
    // - Some v if a is equal to MemRead e1, e1 evaluates to Some ptr and ptr points to v in the memory of the state.
    // - Like aexprEval in Assignment 5 otherwise

    let rec aexprEval (a : aexpr) (st : state) =
        match a with
        | Num x ->
            Ok x

        | Var v ->
            getVar v st

        | Add (b, c) ->
            aexprEval b st
            |> Result.bind (fun x ->
                aexprEval c st
                |> Result.bind (fun y ->
                    Ok (x + y)))

        | Mul (b, c) ->
            aexprEval b st
            |> Result.bind (fun x ->
                aexprEval c st
                |> Result.bind (fun y ->
                    Ok (x * y)))

        | Div (b, c) ->
            aexprEval b st
            |> Result.bind (fun x ->
                aexprEval c st
                |> Result.bind (fun y ->
                    if y <> 0 then Ok (x / y)
                    else Error DivisionByZero))

        | MemRead e1 ->
            aexprEval e1 st
            |> Result.bind (fun ptr ->
                getMem ptr st)


    let rec bexprEval (b : bexpr) (st : state) =
        match b with
        | TT ->
            Ok true

        | Eq (a, c) ->
            aexprEval a st
            |> Result.bind (fun x ->
                aexprEval c st
                |> Result.bind (fun y ->
                    Ok (x = y)))

        | Lt (a, c) ->
            aexprEval a st
            |> Result.bind (fun x ->
                aexprEval c st
                |> Result.bind (fun y ->
                    Ok (x < y)))

        | Conj (a, c) ->
            bexprEval a st
            |> Result.bind (fun x ->
                bexprEval c st
                |> Result.bind (fun y ->
                    Ok (x && y)))

        | Not a ->
            bexprEval a st
            |> Result.bind (fun x ->
                Ok (not x))


    let rec stmntEval (s : stmnt) (st : state) =
        match s with
        | Skip ->
            Ok st

        | Declare v ->
            declare v st

        | Assign (v, a) ->
            aexprEval a st
            |> Result.bind (fun x ->
                setVar v x st)

        | Seq (s1, s2) ->
            stmntEval s1 st
            |> Result.bind (fun st' ->
                stmntEval s2 st')

        | If (guard, s1, s2) ->
            bexprEval guard st
            |> Result.bind (fun result ->
                if result then
                    stmntEval s1 st
                else
                    stmntEval s2 st)

        | While (guard, body) ->
            bexprEval guard st
            |> Result.bind (fun result ->
                if result then
                    stmntEval body st
                    |> Result.bind (fun st' ->
                        stmntEval (While (guard, body)) st')
                else
                    Ok st)

        | Alloc (x, e) ->
            aexprEval e st
            |> Result.bind (fun size ->
                alloc x size st)

        | Free (e1, e2) ->
            aexprEval e1 st
            |> Result.bind (fun ptr ->
                aexprEval e2 st
                |> Result.bind (fun size ->
                    free ptr size st))

        | MemWrite (e1, e2) ->
            aexprEval e1 st
            |> Result.bind (fun ptr ->
                aexprEval e2 st
                |> Result.bind (fun v ->
                    setMem ptr v st))