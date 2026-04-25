module Interpreter.Eval

    open Result
    open Language
    open StateMonad
    
    type StateBuilder() =  

        member this.Bind(f, x) = (>>=) f x  
        member this.Return(x) = ret x  
        member this.ReturnFrom(x) = x  
        member this.Combine(a, b) = a >>= (fun _ -> b) 
      
    let ev = StateBuilder()

    let readFromConsole () = System.Console.ReadLine().Trim()
    let tryParseInt (str : string) = System.Int32.TryParse str

    let rec readInt () =
        let input = readFromConsole ()
        match tryParseInt input with
        | (true, value) -> value
        | (false, _) ->
            printfn "%s" (input + " is not an integer")
            readInt ()

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


    let rec arithEval2 (expr : aexpr) = ev {
        match expr with
        | MemRead a ->
            let! x = arithEval2 a
            return! getMem x

        | Var x -> return! getVar x
        | Num n -> return n
        | Add (a,b) ->
            let! x = arithEval2 a
            let! y = arithEval2 b

            return (x + y)
            
        | Mul (a,b) ->
            let! x = arithEval2 a
            let! y = arithEval2 b

            return (x * y)
        | Div (a,b) ->
            let! x = arithEval2 a
            let! y = arithEval2 b


            if y <> 0 then return (x / y) else return! fail DivisionByZero
            
        | Random -> return! random
        | Read -> return (readInt ())
        | Cond (b, a1, a2) ->
            let! x = boolEval2 b
            match x with
            | true -> return! arithEval2 a1
            | false -> return! arithEval2 a2
        | _ -> return 0
    }
    and boolEval2 (bexpr : bexpr) = ev {
        match bexpr with
        | TT -> return true
        | Not a -> 
            let! x = boolEval2 a
            return (not x)
        | Eq (a,b) -> 
            let! x = arithEval2 a
            let! y = arithEval2 b

            return (x = y)        
        | Lt (a,b) -> 
            let! x = arithEval2 a
            let! y = arithEval2 b
            return (x < y)
        | Conj (a,b) -> 
            let! x = boolEval2 a
            let! y = boolEval2 b

            return (x && y)        
    }
    

    let rec arithEval (expr : aexpr) =
        match expr with
        | MemRead a ->
            arithEval a >>= fun x -> getMem x
        | Var x -> getVar x
        | Num n -> ret n
        | Add (a,b) ->
            arithEval a >>= fun x ->
            arithEval b >>= fun y ->
            ret (x + y)
        | Mul (a,b) ->
            arithEval a >>= fun x ->
            arithEval b >>= fun y ->
            ret (x * y)
        | Div (a,b) ->
            arithEval a >>= fun x ->
            arithEval b >>= fun y ->
            if y <> 0 then ret (x / y) else fail DivisionByZero
        | Random -> random
        | Read -> ret (readInt ())
        | Cond (b, a1, a2) ->
            boolEval b >>= fun x ->
            match x with
            | true -> arithEval a1
            | false -> arithEval a2
        | _ -> ret 0
    and boolEval (bexpr : bexpr) =
        match bexpr with
        | TT -> ret true
        | Not a -> boolEval a >>= fun x -> ret (not x)
        | Eq (a,b) -> arithEval a >>= fun x -> arithEval b >>= fun y -> ret (x = y)
        | Lt (a,b) -> arithEval a >>= fun x -> arithEval b >>= fun y -> ret (x < y)
        | Conj (a,b) -> boolEval a >>= fun x -> boolEval b >>= fun y -> ret (x && y)


    let mergeStrings (es : aexpr list) (s : string) =
        let split (s1 : string) (s2 : string) = s2 |> s1.Split |> Array.toList

        let stringArray = split s "%"

        let rec evaluateValues (es : aexpr list) (acc : int list) =
            match es with
            | [] -> ret acc
            | x::xs ->
                arithEval x >>= fun y -> evaluateValues xs (y::acc)

        
        let rec mergeStringsA (es : int list) (s : string list) (acc : string) =
            match s, es with
            | (s::[], []) -> Some (acc + s)
            | (s::xs, e::ex) -> mergeStringsA ex xs (acc + s + e.ToString())
            | (_, _) -> None
        
        

        evaluateValues es [] >>= fun y ->
            match stringArray.Length-1 = y.Length with
            | false -> ret( Error (IllFormedPrint (s, y)))
            | true -> 

                let newVs = List.rev y
                
                match mergeStringsA newVs stringArray "" with
                | Some result -> ret (Ok result)
                | None -> ret (Error (IllFormedPrint (s, newVs)))

        
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

    let rec stmntEval (stmnt : stmnt) = 
        match stmnt with
        | Alloc (x, e) ->
            arithEval e >>= fun size -> alloc x size
        | Free (e1, e2) ->
            arithEval e1 >>= fun ptr -> arithEval e2 >>= fun size -> free ptr size
        | MemWrite (e1, e2) ->
            arithEval e1 >>= fun ptr -> arithEval e2 >>= fun v -> setMem ptr v
        | Skip -> ret ()
        | Declare v -> declare v
        | Assign (v,a) -> 
            arithEval a >>= fun a -> setVar v a
        | Seq (s1, s2) -> 
            stmntEval s1 >>= fun st1 -> stmntEval s2
        | If (bexpr,s1,s2) -> 
            boolEval bexpr >>= fun x ->
            match x with
            | true -> stmntEval s1
            | false -> stmntEval s2
        | While (bexpr, s) ->
            boolEval bexpr >>= fun x ->
            match x with
            | true -> stmntEval s >>= fun y -> stmntEval (While (bexpr, s))
            | false -> ret ()
        | Print (es, s) ->
            mergeStrings es s >>= fun x ->
                match x with
                | Ok s -> 
                    printfn "%s" s
                    ret ()
                | Error err -> ret ()

    let rec stmntEval2 (stmnt : stmnt) = ev {
        match stmnt with
        | Alloc (x, e) ->
            let! size = arithEval e

            return! alloc x size
        | Free (e1, e2) ->
            let! ptr = arithEval e1
            let! size = arithEval e2

            return! free ptr size

        | MemWrite (e1, e2) ->

            let! ptr = arithEval e1

            let! v = arithEval e2

            return! setMem ptr v
        | Skip -> return ()
        | Declare v -> return! declare v
        | Assign (v,a) -> 
            let! a = arithEval a
            return! setVar v a
        | Seq (s1, s2) -> 

            let! st1 = stmntEval s1

            return! stmntEval s2
        | If (bexpr,s1,s2) -> 
            let! x = boolEval bexpr
            match x with
            | true -> return! stmntEval s1
            | false -> return! stmntEval s2
        | While (bexpr, s) ->
            let! x = boolEval bexpr
            match x with
            | true -> 
                let! y = stmntEval s
                return! stmntEval (While (bexpr, s))
            | false -> return ()
        | Print (es, s) ->
                let! x = mergeStrings es s
                match x with
                | Ok s -> 
                    printfn "%s" s
                    return ()
                | Error err -> return ()
    }
