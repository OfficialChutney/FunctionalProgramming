module Interpreter.EvalGreen

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
        | Num a -> Some a
        | Add (a,b) | Mul (a,b) | Div (a,b) when aexprEval a = None || aexprEval b = None -> None
        | Add (a,b) -> (Option.get(aexprEval a) + Option.get(aexprEval b)) |> Some
        | Mul (a,b) -> (Option.get(aexprEval a) * Option.get(aexprEval b)) |> Some
        | Div (a,b) when Option.get(aexprEval b) <> 0 -> (Option.get(aexprEval a) / Option.get(aexprEval b)) |> Some
        | _ -> None


    let rec aexprEval2 expr =
        match expr with
        | Num n -> Some n
        | Add (a,b) ->
            Option.bind (fun x ->
            Option.bind (fun y ->
            Some (x + y)) (aexprEval2 b)) (aexprEval2 a)
        | Mul (a,b) ->
            Option.bind (fun x ->
            Option.bind (fun y ->
            Some (x * y)) (aexprEval2 b)) (aexprEval2 a)
        | Div (a,b) ->
            Option.bind (fun x ->
            Option.bind (fun y ->
            if y = 0 then None else Some (x / y)) (aexprEval2 b)) (aexprEval2 a)


    let rec bexprEval = function
        | TT -> Some true
        | Not a -> Option.bind (fun x -> Some (not x)) (bexprEval a)
        | Eq (a,b) -> Option.bind (fun x -> Option.bind (fun y -> Some (x = y)) (aexprEval2 a)) (aexprEval2 b)
        | Lt (a,b) -> Option.bind (fun x -> Option.bind (fun y -> Some (y < x)) (aexprEval2 a)) (aexprEval2 b)
        | Conj (a,b) -> Option.bind (fun x -> Option.bind (fun y -> Some (x && y)) (bexprEval a)) (bexprEval b)
        
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

    let aexprFold _ = failwith "not implemented"
    let bexprFold _ = failwith "not implemented"
    
    let aexprToString3 _ = failwith "not implemented"
    let bexprToString3 _ = failwith "not implemented"
    let aexprEval3 _ = failwith "not implemented"
    let bexprEval3 _ = failwith "not implemented"
    let aexprToString4 _ = failwith "not implemented"
    let bexprToString4 _ = failwith "not implemented"