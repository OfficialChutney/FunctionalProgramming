module Interpreter.State

    open Result
    open Language
    
// GREEN EXERCISES
// _________________________________________________________

// Exercise 5.1
// Create a function reservedVariableName of type string -> bool that given a variable name v returns true if
// true if v is one of if, then, else, while, declare, print, random,  fork, or __result__.
// false otherwise
    let reservedVariableName (v: string) =
        let (variablesList : string list) = ["if"; "then"; "else"; "while"; "declare"; "print"; "random"; "fork"; "__result__"]
        if List.exists ((=) v) variablesList then true
        else false

// Exercise 5.2
// Create a function validVariableName of type string -> bool that given a variable name v returns true if
// v starts with a letter or an underscore (Hint: use System.Char.IsAsciiLetter).
// v only contains letters, numbers, or underscores (Hint: the functions String.forall and System.Char.IsAsciiLetterOrDigit are useful)
// and false otherwise.

    let validVariableName (v: string) =
        if (System.Char.IsAsciiLetter(v.[0]) || v.[0] = '_') && (String.forall (fun c -> System.Char.IsAsciiLetterOrDigit c || c = '_') v) then true
        else false

// Exercise 5.3
// Create a type state, using either a disjoint union or a record type, that contains a map from strings to integers that will 
// contain your variables. Do not just use a type alias, like you did for complex numbers, as this will not scale for the rest
// of the course. Moreover, using a record or disjoint union will make the pretty printing accurate.


// This is record type. I chose this because it is easily scalable and in this case it should just hold information.
// A disjoint union would not make sense later on when we would like to add memory etc.
    type state = {vars : Map<string, int>}


// Exercise 5.4
// Create a function mkState : unit -> state that returns a state with an empty variable environment.
    
    let mkState () = {vars = Map.empty}
    
// Exercise 5.5
// Create a function declare of type string -> state -> state option that given a variable name x and a state st returns.
// Some st' where st' is equal to st with the variable x set to 0 if all of the following hold
// x does not appear in the variables of st
// x is a valid variable name
// x is not a reserved variable name
// None otherwise

    let declare x (st : state) =
        if Map.containsKey x st.vars then None
        elif not (validVariableName x) then None
        elif reservedVariableName x then None
        else Some { vars = Map.add x 0 st.vars }
            
// Map.add x 0 st.vars
//         │ │   │
//         │ │   └── the old map we want to add to
//         │ └────── the value we want to store
//         └──────── the key/name of the variable
        
(* 
    let declare1 x (st : state) =
        if Map.containsKey x st.vars then
            None
        elif not (validVariableName x) then
            None
        elif reservedVariableName x then
            None
        else
            let updatedMap = Map.add x 0 st.vars
            let updatedState = { vars = updatedMap }
            Some updatedState
*)

// Exercise 5.6
// Create a function getVar of type string -> state -> int option that given a variable name x and a state st returns
// Some v, where v is the valuated associated with the variable x if that variable is declared in st
// None otherwise

    let getVar x (st : state) =
        if Map.containsKey x st.vars then
            Some (Map.find x st.vars)
        else 
            None

// Test this by running in f# interactive
// let st = { vars = Map.ofList [("x", 10); ("y", 5)] }
// > getVar "x" st;;


// Exercise 5.7
// Create a function setVar of type string -> int -> state -> state option that given a variable name x, an integer value v and a state st returns
// Some st' where st' is equal to st where the variable x has had its value updated to v, if x is declared in st
// None otherwise

    let setVar x (v : int) (st : state) =
        if Map.containsKey x st.vars then
            Some { vars = Map.add x v st.vars }
        else 
            None

// For at teste de eksempler i assignment skal alle functioner markeres (uden open result / language)
// Kør dem i interactive, og skriv først "open Option;;", derefter kan du køre test.

// Test case 3:
// > () |> mkState |> declare "x" |> bind (setVar "x" 42) |> bind (getVar "x")
// - val it: int option = Some 42




// YELLOW EXERCISES - Part 1
// _________________________________________________________

// Refactor the code above to return a  Result<int, Error> rather than an int option such
// that they return Ok result in stead of Some result when the functions succeed and 
// return an Error, rather than None, such that


// Exercise 5.12
// declare returns the error
// - VarAlreadyExists(v) if the variable v has already been declared
// - ReservedName(v) if v is one of if, then, else, while, declare, print, random, fork, or __result__.
// - InvalidVarName(v) if v is an invalid variable name.

    let declare2 v (st : state) =
        if Map.containsKey v st.vars then Error (VarAlreadyExists v)
        elif not (validVariableName v) then Error (InvalidVarName v)
        elif reservedVariableName v then Error (ReservedName v)
        else Ok { vars = Map.add v 0 st.vars }
            

// Exercise 5.13
// getVar and setVar return VarNotDeclared v if the variable v, that they are either trying to get or set,
// has not been declared. You do not have to concern yourself with illegal or invalid names here, declare
// does that.

    let getVar2 x (st : state) =
        if Map.containsKey x st.vars then
            Ok (Map.find x st.vars)
        else 
            Error (VarNotDeclared x)

    let setVar2 x (v : int) (st : state) =
        if Map.containsKey x st.vars then
            Ok { vars = Map.add x v st.vars }
        else 
            Error (VarNotDeclared x)


// RED EXERCISES - Part 1
// _________________________________________________________

// Read Assignment description - Essentially state so that instead of storing their variables in a map from variables to integers (Map<string, int>)
// we will now store them in a stack of these maps (Map<string, int> list). The idea is:

// - When you enter a block from a while- or an if-statement, you push an empty variable environment 
// (an empty map) to the top of the stack and when you exit the block you pop the top element.

// - When you read a variable you start reading from the top of the stack, check if the variable is 
// there and if so returns its value, if its not go down one level in the stack and try again.

    type state2 = {vars : Map<string, int> list}

    let mkState2 () = {vars = [Map.empty]}
    
// Exercise 5.17
// Change declare so that

// - It only returns the VarNotDeclared(v) error if v is declared in the top environment of the stack, meaning that you may re-declare a variable as long as the other identical declarations appear further down the stack.
// - Puts the newly declared variable at the top of the stack.

    let declare3 v (st : state2) =
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


// Exercise 5.18
// Change getVar and setVar so that they traverse through the stack and get or set the first occurence of the variable they find.
// Only if they reach the end of the stack do they return the error VarNotDeclared.

    let getVar3 v (st : state2) =
        let rec search scopes =
            match scopes with
            | [] -> Error (VarNotDeclared v)
            | currentScope :: outerScope -> 
                match Map.tryFind v currentScope with
                | Some value -> Ok value
                | None -> search outerScope
        search st.vars


    let setVar3 x (v : int) (st : state2) =
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


// Exercise 5.19
// Create a function push of type state -> state that given a state st pushes an empty variable
// enviroment (an empty map) to the top of the stack.


    let push (st : state2) =
        {vars = Map.empty :: st.vars}

    let pop (st : state2) : state2 =
        {vars = List.tail st.vars}    
