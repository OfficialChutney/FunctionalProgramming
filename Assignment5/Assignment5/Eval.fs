module Interpreter.Eval

    open Result
    open Language
    open State
    

    let rec aexprToString = function
        | Num n -> string n
        | Add (n,x) -> "(" + aexprToString n + " + " + aexprToString x + ")"
        | Mul (n,x) -> "(" + aexprToString n + " * " + aexprToString x + ")"
        | Div (n,x) -> "(" + aexprToString n + " / " + aexprToString x + ")"

    let rec bexprToString = function
        | TT -> "true"
        | Eq (n,x) -> "(" + aexprToString n + " = " + aexprToString x + ")"
        | Lt (n,x) -> "(" + aexprToString n + " < " + aexprToString x + ")"
        | Conj (n,x) -> "(" + bexprToString n + " /\ " + bexprToString x + ")"
        | Not n -> "(not " + bexprToString n + ")"

    let rec aexprEval (a : aexpr) (st : state) =
        match a with
        | Var x -> getVar x st
        | Num a -> Ok a
        | Add (a,b) -> 
            match aexprEval a st, aexprEval b st with 
                | Ok a, Ok b -> Ok (a + b) 
                | Error a, _ -> Error a
                | _, Error b -> Error b
        | Mul (a,b) -> 
            match aexprEval a st, aexprEval b st with 
                | Ok a, Ok b -> Ok (a * b) 
                | Error a, _ -> Error a
                | _, Error b -> Error b
        | Div (a,b) ->
            match aexprEval a st, aexprEval b st with 
                | Ok a, Ok b when b <> 0 -> Ok (a / b) 
                | Error a, _ -> Error a
                | _, Error b -> Error b
                | _ -> Error DivisionByZero
      

    let rec aexprEval2 (expr : aexpr) (st : state) =
        match expr with
        | Var x -> getVar x st
        | Num n -> Ok n
        | Add (a,b) ->
            Result.bind (fun x ->
            Result.bind (fun y ->
            Ok (x + y)) (aexprEval2 b st)) (aexprEval2 a st)
        | Mul (a,b) ->
            Result.bind (fun x ->
            Result.bind (fun y ->
            Ok (x * y)) (aexprEval2 b st)) (aexprEval2 a st)
        | Div (a,b) ->
            Result.bind (fun x ->
            Result.bind (fun y ->
            if y = 0 then Error DivisionByZero else Ok (x / y)) (aexprEval2 b st)) (aexprEval2 a st)


    let rec bexprEval (bexpr : bexpr) (st : state) =
        match bexpr with
        | TT -> Ok true
        | Not a -> Result.bind (fun x -> Ok (not x)) (bexprEval a st)
        | Eq (a,b) -> Result.bind (fun x -> Result.bind (fun y -> Ok (x = y)) (aexprEval2 a st)) (aexprEval2 b st)
        | Lt (a,b) -> Result.bind (fun x -> Result.bind (fun y -> Ok (y < x)) (aexprEval2 a st )) (aexprEval2 b st)
        | Conj (a,b) -> Result.bind (fun x -> Result.bind (fun y -> Ok (x && y)) (bexprEval a st)) (bexprEval b st)
        
    let aexprToString2 (a : aexpr) =         

        let wrap (pair : string * int) (parentLvl : int) (useGE : bool) =
            match pair with
            | (s,lvl) when (not useGE && lvl > parentLvl) || (useGE && lvl >= parentLvl) -> "(" + s + ")"
            | (s,_) -> s

        let rec auxillery = function
                | Num a -> ((string)a, 0)
                | Add(a,b) ->     
                    let left  = auxillery a 
                    let right = auxillery b
                    let s = (wrap left 2 false) + " + " + (wrap right 2 false)
                    (s, 2)
                | Mul(a,b) ->     
                    let left  = auxillery a 
                    let right = auxillery b
                    let s = (wrap left 1 false) + " * " + (wrap right 1 false)
                    (s, 1)
                | Div(a,b) ->     
                    let left  = auxillery a 
                    let right = auxillery b
                    let s = (wrap left 1 false) + " / " + (wrap right 1 true)
                    (s, 1)

        fst (auxillery a)

    let bexprToString2 (a : bexpr) =         

        let wrap (pair : string * int) (parentLvl : int) (useGE : bool) =
            match pair with
            | (s,lvl) when (not useGE && lvl > parentLvl) || (useGE && lvl >= parentLvl) -> "(" + s + ")"
            | (s,_) -> s

        let rec auxillery = function
                | TT -> ("true", 0)
                | Not a -> 
                    let left  = auxillery a 
                    let s = "not " + (wrap left 1 true)
                    (s, 1)
                | Eq (a,b) ->     
                    let left  = aexprToString2 a
                    let right = aexprToString2 b
                    let s = left + " = " + right
                    (s, 2)
                | Lt(a,b) ->     
                    let left  = aexprToString2 a
                    let right = aexprToString2 b
                    let s = left + " < " + right
                    (s, 2)
                | Conj(a,b) ->     
                    let left  = auxillery a 
                    let right = auxillery b
                    let s = (wrap left 1 false) + " /\ " + (wrap right 1 false)
                    (s, 1)

        fst (auxillery a)

(*    let rec aexprFold (fnum : (int -> 'a)) (fadd : ('a -> 'a -> 'a)) (fmul : ('a -> 'a -> 'a)) (fdiv : ('a -> 'a -> 'a)) (a : 'a) = 
        match a with
            | Num a -> fnum a
            | Add(a,b) -> 
                let acc1 = aexprFold fnum fadd fmul fdiv a
                let acc2 = aexprFold fnum fadd fmul fdiv b
                fadd acc1 acc2
            | Mul(a,b) -> 
                let acc1 = aexprFold fnum fadd fmul fdiv a
                let acc2 = aexprFold fnum fadd fmul fdiv b
                fmul acc1 acc2
            | Div(a,b) -> 
                let acc1 = aexprFold fnum fadd fmul fdiv a
                let acc2 = aexprFold fnum fadd fmul fdiv b
                fdiv acc1 acc2*)


    let rec stmntEval (stmnt : stmnt) (st : state) = 
        match stmnt with
        | Skip -> Ok (st)
        | Declare v -> declare v st
        | Assign (v,a) -> 
            match aexprEval a st with
            | Ok a -> setVar v a st
            | Error a -> Error a
        | Seq (s1, s2) -> 
            let st1 = stmntEval s1 st

            match st1 with
            | Ok st -> stmntEval s2 st
            | Error e -> Error e
        | If (bexpr,s1,s2) -> 
            match bexprEval bexpr st with
            | Ok true -> stmntEval s1 st
            | Ok false -> stmntEval s2 st
            | Error b -> Error b
        | While (bexpr, s) ->
            match bexprEval bexpr st with
            | Ok true ->
                match stmntEval s st with
                | Ok st -> stmntEval (While (bexpr, s)) st
                | Error e -> Error e
            | Ok false -> Ok st
            | Error e -> Error e