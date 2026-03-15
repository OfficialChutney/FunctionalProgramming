module Interpreter.State

    open Result
    open Language
    
    let reservedVariableName (s : string) =
        let reservedVar = ["if"; "then"; "else"; "while"; "declare"; "print"; "random"; "fork"; "__result__"]

        List.exists ((fun x y -> x = y) s ) reservedVar



    let validVariableName (s : string) = ((System.Char.IsAsciiLetter(s.[0]) || s.[0] = '_') && (String.forall (fun c -> System.Char.IsAsciiLetterOrDigit(c) || c = '_') s))
            
    type state = 
        | State of Map<string, int>
    
    let mkState () = State Map.empty
    
    let declare (x : string) (st : state) =
        match st with
        | State vars when Map.containsKey x vars -> Error (VarAlreadyExists x)
        | State vars when not (validVariableName x) -> Error (InvalidVarName x)
        | State vars when reservedVariableName x -> Error (ReservedName x)
        | State vars -> Ok (State (Map.add x 0 vars))

    let getVar (x : string) (st : state)  = 
        match st with
        | State s when Map.containsKey x s -> Ok (Map.find x s)
        | _ -> Error (VarNotDeclared x)


    let setVar (x : string) (v : int) (st : state) = 
        match st with
        | State s when Map.containsKey x s -> Ok (State (Map.add x v s))
        | _ -> Error (VarNotDeclared x)
    
    let push _ = failwith "not implemented"
    let pop _ = failwith "not implemented"     