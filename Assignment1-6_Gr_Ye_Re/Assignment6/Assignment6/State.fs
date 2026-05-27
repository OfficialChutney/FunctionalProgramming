module Interpreter.State

    open Result
    open Language
    open Memory
    
    // Exercise 5.1 (From Assignment 5)
    let reservedVariableName (v: string) =
        let (variablesList : string list) = ["if"; "then"; "else"; "while"; "declare"; "print"; "random"; "fork"; "__result__"]
        if List.exists ((=) v) variablesList then true
        else false

    // Exercise 5.2 (From Assignment 5)
    let validVariableName (v: string) =
        if (System.Char.IsAsciiLetter(v.[0]) || v.[0] = '_') && (String.forall (fun c -> System.Char.IsAsciiLetterOrDigit c || c = '_') v) then true
        else false

    // Exercise 6.9
    // Update the state datatype to also, in addition to the variable store from Assignment 5, include memory of type memory.
    // You can either use a record or a discriminated union to include both.
    type state =
        {
            vars : Map<string, int> list
            memory : memory
        }

    // Original type state
    // Exercise 5.3 - Yellow refactor (From Assignment 5)
    // type state = {vars : Map<string, int> list}
    
    // Exercise 6.10
    // The original mkState has type unit -> state. Change it to have type int -> state that given an integer
    // memSize returns the same state as in Assignment 5, but with the memory set to Memory.empty memSize
    
    let mkState (memSize : int) =
        {
            vars = [Map.empty]
            memory = Memory.empty memSize
        }

    // Original type state
    // Exercise 5.4 - Yellow refactor (From Assignment 5)
    // let mkState () = {vars = [Map.empty]}


    let random _ = failwith "not implemented"
    

    // Exercise 6.11
    // Update declare, getVar, and setVar to use your new version of state. This should be a very short refactor.
    // Make sure that your existing program from Assignment 5 still works.

    let declare v (st : state) =
        let top = List.head st.vars
        let rest = List.tail st.vars

        if Map.containsKey v top then
            Error (VarAlreadyExists v)
        elif not (validVariableName v) then
            Error (InvalidVarName v)
        elif reservedVariableName v then
            Error (ReservedName v)
        else
            let newTop = Map.add v 0 top
            Ok {st with vars = newTop :: rest}

    (*
    // Orignal declare function
    // Exercise 5.5 - Yellow refactor (From Assignment 5)
    let declare v (st : state) =
        let top = List.head st.vars
        let rest = List.tail st.vars

        if Map.containsKey v top then 
            Error (VarAlreadyExists v)
        elif not (validVariableName v) then
             Error (InvalidVarName v)
        elif reservedVariableName v then
             Error (ReservedName v)
        else 
            let newTop = Map.add v 0 top
            Ok {vars = newTop :: rest}
    *)

    let getVar v (st : state) =
        let rec search scopes =
            match scopes with
            | [] -> Error (VarNotDeclared v)
            | currentScope :: outerScope ->
                match Map.tryFind v currentScope with
                | Some value -> Ok value
                | None -> search outerScope

        search st.vars

    (*
    // Orignal getVar function
    // Exercise 5.6 - Yellow refactor (From Assignment 5)
    let getVar v (st : state) =
        let rec search scopes =
            match scopes with
            | [] -> Error (VarNotDeclared v)
            | currentScope :: outerScope -> 
                match Map.tryFind v currentScope with
                | Some value -> Ok value
                | None -> search outerScope
        search st.vars
    *)

    let setVar x (v : int) (st : state) =
        let rec update scopes =
            match scopes with
            | [] ->
                Error (VarNotDeclared x)

            | currentScope :: outerScope ->
                if Map.containsKey x currentScope then
                    let updatedCurrentScope = Map.add x v currentScope
                    Ok (updatedCurrentScope :: outerScope)
                else
                    match update outerScope with
                    | Ok updatedOuterScope ->
                        Ok (currentScope :: updatedOuterScope)
                    | Error e ->
                        Error e

        match update st.vars with
        | Ok updatedScope -> Ok {st with vars = updatedScope}
        | Error e -> Error e

    (*
    // Original setVar function
    // Exercise 5.7 (From Assignment 5)
    let setVar x (v : int) (st : state) =
        let rec update scopes = 
            match scopes with 
            | [] -> 
                Error (VarNotDeclared x)

            | currentScope :: outerScope -> 
                if Map.containsKey x currentScope then
                    let updatedCurrentScope = Map.add x v currentScope
                    Ok (updatedCurrentScope :: outerScope)
                else
                    match update outerScope with
                    | Ok updatedOuterScope -> 
                        Ok (currentScope :: updatedOuterScope)
                    | Error e -> 
                        Error e
        match update st.vars with
        | Ok updatedScope -> Ok {vars = updatedScope}
        | Error e -> Error e
    
    *)

    // Exercise 6.12
    // Create a function alloc of type string -> int -> state -> state option that given a variable x 
    // and a size of memory to allocate size, and a state st allocates size cells of memory in st and returns

    // Some st', where st' is equal to st but where the memory has been allocated and where the variable x points to the newly allocated memory
    // None if either memory allocation fails or if writing to the variable x fails

    let alloc (x : string) (size : int) (st : state) =
        Memory.alloc size st.memory
        |> Result.bind (fun (newMemory, ptr) ->
            let stWithNewMemory = {st with memory = newMemory}
            setVar x ptr stWithNewMemory)


    // Exercise 6.13
    // Create functions free, getMem, and setMem that have the same signatures as in Memory.fs except all occurrences of mem 
    // have been replaced by state. The free function, for instance, should have type int -> int -> state -> state option. 
    // These functions all leave the variable store alone but use their corresponding functions from Memory.fs to read 
    // and modify the memory part of the state.

    let free (ptr : int) (size : int) (st : state) =
        Memory.free ptr size st.memory
        |> Result.bind (fun newMemory ->
            Ok {st with memory = newMemory})


    let getMem (ptr : int) (st : state) =
        Memory.getMem ptr st.memory


    let setMem (ptr : int) (v : int) (st : state) =
        Memory.setMem ptr v st.memory
        |> Result.bind (fun newMemory ->
            Ok {st with memory = newMemory})


    // Red Functions (From Assignment 5)
    let push (st : state) = {st with vars = Map.empty :: st.vars}

    let pop (st : state) = {st with vars = List.tail st.vars}
    