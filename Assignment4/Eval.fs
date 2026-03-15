module Interpreter.Eval

    open Result
    open Language
    
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

    let rec aexprEval = function
        | Num a -> Ok a
        | Add (a,b) -> 
            match aexprEval a, aexprEval b with 
                | Ok a, Ok b -> Ok (a + b) 
                | Error a, _ -> Error a
                | _, Error b -> Error b
        | Mul (a,b) -> 
            match aexprEval a, aexprEval b with 
                | Ok a, Ok b -> Ok (a * b) 
                | Error a, _ -> Error a
                | _, Error b -> Error b
        | Div (a,b) ->
            match aexprEval a, aexprEval b with 
                | Ok a, Ok b when b <> 0 -> Ok (a / b) 
                | Error a, _ -> Error a
                | _, Error b -> Error b
                | _ -> Error DivisionByZero
      

    let rec aexprEval2 expr =
        match expr with
        | Num n -> Ok n
        | Add (a,b) ->
            Result.bind (fun x ->
            Result.bind (fun y ->
            Ok (x + y)) (aexprEval2 b)) (aexprEval2 a)
        | Mul (a,b) ->
            Result.bind (fun x ->
            Result.bind (fun y ->
            Ok (x * y)) (aexprEval2 b)) (aexprEval2 a)
        | Div (a,b) ->
            Result.bind (fun x ->
            Result.bind (fun y ->
            if y = 0 then Error DivisionByZero else Ok (x / y)) (aexprEval2 b)) (aexprEval2 a)


    let rec bexprEval = function
        | TT -> Ok true
        | Not a -> Result.bind (fun x -> Ok (not x)) (bexprEval a)
        | Eq (a,b) -> Result.bind (fun x -> Result.bind (fun y -> Ok (x = y)) (aexprEval2 a)) (aexprEval2 b)
        | Lt (a,b) -> Result.bind (fun x -> Result.bind (fun y -> Ok (y < x)) (aexprEval2 a)) (aexprEval2 b)
        | Conj (a,b) -> Result.bind (fun x -> Result.bind (fun y -> Ok (x && y)) (bexprEval a)) (bexprEval b)
        
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

    let rec aexprFold (fnum : (int -> 'a)) (fadd : ('a -> 'a -> 'a)) (fmul : ('a -> 'a -> 'a)) (fdiv : ('a -> 'a -> 'a)) (a : 'a) = 
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
                fdiv acc1 acc2


    let bexprFold _ = failwith "not implemented"
    
    let aexprToString3 _ = failwith "not implemented"
    let bexprToString3 _ = failwith "not implemented"
    let aexprEval3 _ = failwith "not implemented"
    let bexprEval3 _ = failwith "not implemented"
    let aexprToString4 _ = failwith "not implemented"
    let bexprToString4 _ = failwith "not implemented"