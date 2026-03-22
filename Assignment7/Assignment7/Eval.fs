module Interpreter.Eval

    open Result
    open Language
    open Memory
    open State
    

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


    let rec aexprEval (expr : aexpr) (st : state) =
        match expr with
        | MemRead a ->
            match aexprEval a st with
            | Ok ptr -> Memory.getMem ptr st.memory
            | Error e -> Error e
        | Var x -> getVar x st
        | Num n -> Ok n
        | Add (a,b) ->
            Result.bind (fun x ->
            Result.bind (fun y ->
            Ok (x + y)) (aexprEval b st)) (aexprEval a st)
        | Mul (a,b) ->
            Result.bind (fun x ->
            Result.bind (fun y ->
            Ok (x * y)) (aexprEval b st)) (aexprEval a st)
        | Div (a,b) ->
            Result.bind (fun x ->
            Result.bind (fun y ->
            if y = 0 then Error DivisionByZero else Ok (x / y)) (aexprEval b st)) (aexprEval a st)
        | Random -> Ok (st.rand.Next())
        | Read -> Ok(readInt ())
        | Cond (b, a1, a2) ->
            match bexprEval b st with
            | Ok (true) -> aexprEval a1 st
            | Ok (false) -> aexprEval a2 st
            | Error e -> Error e
        | _ -> Ok(0)
    and bexprEval (bexpr : bexpr) (st : state) =
        match bexpr with
        | TT -> Ok true
        | Not a -> Result.bind (fun x -> Ok (not x)) (bexprEval a st)
        | Eq (a,b) -> Result.bind (fun x -> Result.bind (fun y -> Ok (x = y)) (aexprEval a st)) (aexprEval b st)
        | Lt (a,b) -> Result.bind (fun x -> Result.bind (fun y -> Ok (y < x)) (aexprEval a st )) (aexprEval b st)
        | Conj (a,b) -> Result.bind (fun x -> Result.bind (fun y -> Ok (x && y)) (bexprEval a st)) (bexprEval b st)


    let mergeStrings (es : aexpr list) (s : string) (st : state) =
        let split (s1 : string) (s2 : string) = s2 |> s1.Split |> Array.toList

        let stringArray = split s "%"

        let rec evaluateValues (es : aexpr list) (st : state) (acc : int list) =
            match es with
            | [] -> Ok acc
            | x::xs ->
                match aexprEval x st with
                | Ok a -> evaluateValues xs st (a::acc)
                | Error e -> Error e

        
        let rec mergeStringsA (es : int list) (s : string list) (acc : string) =
            match s, es with
            | (s::[], []) -> Some (acc + s)
            | (s::xs, e::ex) -> mergeStringsA ex xs (acc + s + e.ToString())
            | (_, _) -> None
        
        
        match evaluateValues es st [] with
        | Ok vs -> 
            match stringArray.Length-1 = vs.Length with
            | false -> Error (IllFormedPrint (s, vs))
            | true -> 

                let newVs = List.rev vs
                
                match mergeStringsA newVs stringArray "" with
                | Some result -> Ok result
                | None -> Error (IllFormedPrint (s, newVs))
        | Error e -> Error e




    let mergeStrings2 (es : aexpr list) (s : string) (st : state) =
        let split (s1 : string) (s2 : string) = s2 |> s1.Split |> Array.toList

        let stringArray = split s "%"

        let rec evaluateValues (es : aexpr list) (st : state) (c : int list -> int list) =
            match es with
            | [] -> Ok (c [])
            | x::xs ->
                match aexprEval x st with
                | Ok a -> evaluateValues xs st (fun s -> c (a::s))
                | Error e -> Error e

        
        let rec mergeStringsA (es : int list) (s : string list) (c : string -> string) =
            match s, es with
            | (s::[], []) -> Some (c s)
            | (s::xs, e::ex) -> mergeStringsA ex xs (fun y -> c (s + e.ToString() + y))
            | (_, _) -> None
        
        
        match evaluateValues es st id with
        | Ok vs -> 
            match stringArray.Length-1 = vs.Length with
            | false -> Error (IllFormedPrint (s, vs))
            | true -> 
                match mergeStringsA vs stringArray id with
                | Some result -> Ok result
                | None -> Error (IllFormedPrint (s, vs))
        | Error e -> Error e
            
        
            


        
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
        | Alloc (x, e) ->
            match aexprEval e st with
            | Ok size ->
                match State.alloc x size st with
                | Ok st' -> Ok st'
                | Error e -> Error (e)
            | Error err -> Error err
        | Free (e1, e2) ->
            match aexprEval e1 st, aexprEval e2 st with
            | Ok ptr, Ok size ->
                match Memory.free ptr size st.memory with
                | Ok newMem -> Ok { st with memory = newMem }
                | Error e -> Error e
            | Error e, _ -> Error e
            | _, Error e -> Error e
        | MemWrite (e1, e2) ->
            match aexprEval e1 st, aexprEval e2 st with
            | Ok ptr, Ok v ->
                match Memory.setMem ptr v st.memory with
                | Ok newMem -> Ok { st with memory = newMem }
                | Error e -> Error e
            | Error e, _ -> Error e
            | _, Error e -> Error e
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
        | Print (es, s) ->
            match mergeStrings es s st with
            | Ok s -> 
                printfn "%s" s
                Ok st
            | Error e -> Error e
        | _ -> Ok st


        