module Interpreter.State

    open Result
    open Language
    
    let reservedVariableName (s : string) =
        let reservedVar = ["if"; "then"; "else"; "while"; "declare"; "print"; "random"; "fork"; "__result__"]

        List.exists ((fun x y -> x = y) s ) reservedVar

    let validVariableName (s : string) = ((System.Char.IsAsciiLetter(s.[0]) || s.[0] = '_') && (String.forall (fun c -> System.Char.IsAsciiLetterOrDigit(c) || c = '_') s))
            
    type state = {
        state : Map<string, int>
        memory : Memory.memory
        rand : System.Random
    }

    
    let mkState (size : int) (oseed : int option) = 
        let getRand = function
            | None -> System.Random()
            | Some seed -> System.Random(seed)

        {state = Map.empty; memory = Memory.empty size; rand = getRand oseed }

    let random (st : state) = st.rand.Next()
    
    let declare (x : string) (st : state) =
        match st.state with
        | vars when Map.containsKey x vars -> Error (VarAlreadyExists x)
        | vars when not (validVariableName x) -> Error (InvalidVarName x)
        | vars when reservedVariableName x -> Error (ReservedName x)
        | vars -> Ok { st with state = Map.add x 0 vars }

    let getVar (x : string) (st : state)  = 
        match st.state with
        | s when Map.containsKey x s -> Ok (Map.find x s)
        | _ -> Error (VarNotDeclared x)


    let setVar (x : string) (v : int) (st : state) = 
        match st.state with
        | s when Map.containsKey x s -> Ok ({ st with state = (Map.add x v s)})
        | _ -> Error (VarNotDeclared x)
    
    let alloc (x : string) (size : int) (st : state) =
        match Memory.alloc size st.memory with
        | Error e ->
            Error e
        | Ok (mem, ptr) ->
            match setVar x ptr { st with memory = mem } with
            | Ok st' -> Ok st'
            | Error e -> Error e
     
    let free (ptr : int) (size : int) (st : state) = 
        match Memory.free ptr size st.memory with
        | Ok a -> Ok { st with memory = a}
        | Error a -> Error a

    let setMem (ptr : int) (v : int) (st : state) = 
        match  Memory.setMem ptr v st.memory with
        | Ok a -> Ok { st with memory = a}
        | Error a -> Error a

        
    let getMem (ptr : int) (st : state) = Memory.getMem ptr st.memory

    let push _ = failwith "not implemented"
    let pop _ = failwith "not implemented"     