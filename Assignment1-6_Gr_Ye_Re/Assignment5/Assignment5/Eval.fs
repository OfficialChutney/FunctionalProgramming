module Interpreter.Eval

    open Result
    open Language
    open State

// GREEN EXERCISES
// _________________________________________________________

// Exercise 5.8 
// Create a recursive function aexprEval of type aexpr -> state -> int option that given
// an arithmetic expression a and a state st works exactly like aexprEval from Assignment 4
// (so you may not use Option.bind) but returns
// - Some x if a is equal to Var v and v is declared in st
// - None otherwise

(*
To test this, mark the whole eval, run it in interactive
Then run in interactive all of this:

#load "Language.fs"
#load "State.fs"
#load "Eval.fs"

open Interpreter.Language
open Interpreter.State
open Interpreter.Eval
open Option

let emptyState = mkState ()
let st = emptyState |> declare "x" |> bind (setVar "x" 42) |> Option.get

Then run the test cases, for example:
> st |> aexprEval (Div (Var "x", Num 2))

Output = Some 21

*)

    let rec aexprEval (a : aexpr) (st : state) =
        match a with
        | Num x -> Some x
        | Var v -> getVar v st
        | Add (b, c) ->
            match aexprEval b st, aexprEval c st with
            | Some x, Some y -> Some (x + y)
            | _ -> None
        | Mul (b, c) ->
            match aexprEval b st, aexprEval c st with
            | Some x, Some y -> Some (x * y)
            | _ -> None 
        | Div (b, c) ->
            match aexprEval b st, aexprEval c st with
            | Some x, Some y when y <> 0 -> Some (x / y)
            | _ -> None
        | Mod (b, c) ->
            match aexprEval b st, aexprEval c st with
            | Some x, Some y when y <> 0 -> Some (x % y)
            | _ -> None


// Exercise 5.9
// Create a function aexprEval2 that behaves exactly the same as aexprEval but which uses
//  Option.bind. You may not pattern match on options in any way, such as the ones obtained
//  by using recursive calls to aexprEval2, and you may not use aexprEval.

    let rec aexprEval2 (a : aexpr) (st : state) =
        match a with
        | Num x -> Some x
        | Var v -> getVar v st
        | Add (b, c) ->
            aexprEval2 b st
            |> Option.bind (fun x ->
                aexprEval2 c st
                |> Option.bind (fun y ->
                    Some (x + y)))
        | Mul (b, c) ->
            aexprEval2 b st
            |> Option.bind (fun x ->
                aexprEval2 c st
                |> Option.bind (fun y ->
                    Some (x * y)))
        | Div (b, c) ->
            aexprEval2 b st
            |> Option.bind (fun x ->
                aexprEval2 c st
                |> Option.bind (fun y ->
                    if y <> 0 then Some (x / y)
                    else None))
        | Mod (b, c) ->
            aexprEval2 b st
            |> Option.bind (fun x ->
                aexprEval2 c st
                |> Option.bind (fun y ->
                    if y <> 0 then Some (x % y)
                    else None))
 

// Exercise 5.10
// Create a function bexprEval of the type bexpr -> state -> bool option that behaves exactly
// like bexprEval from Assignment 4 but which additionally takes a state as an argument and 
// uses that state in your aexprEval or aexprEval2 functions to compute the value of the arithmetic expressions
    let rec bexprEval (b : bexpr) (st : state) = 
        match b with
        | TT -> Some true
        | Eq (a, c) ->
            match aexprEval a st, aexprEval c st with
            | Some x, Some y -> Some (x = y)
            | _ -> None
        | Lt(a, c) ->
            match aexprEval a st, aexprEval c st with
            | Some x, Some y -> Some (x < y)
            | _ -> None
        | Conj (a, c) -> 
            match bexprEval a st, bexprEval c st with
            | Some x, Some y -> Some (x && y)
            | _ -> None
        | Not a -> 
            match bexprEval a st with
            | Some x -> Some (not x)
            | _ -> None


// Exercise 5.11
// Create a function stmntEval of type stmnt -> state -> state option that given a statement s and an initial state st returns
// - Some st, if s is equal to Skip
// - Some st', if s is equal to Declare v, where st' is equal to st with the variable v declared in st,
// - Some st', if s is equal to Assign(v, a)and a evaluates to Some x in st, where st' is equal to st with the variable v set to x.
// - Some st'' if s is equal to Seq(s1, s2), s1 evaluates to Some st' in state st, and s2 evaluates to Some st'' in st'.
// - Some st', if s is equal to If (guard, s1, s2) and guard evaluates to Some x in st, and if either x is equal to
//   - true and s1 evaluates to Some st' in st.
//   - false and s2 evaluates to Some st' in st.
// - Some st'', if s is equal to While (guard, s') and guard evaluates to Some x in st, and if either x is equal to
//   - true and s' evaluates to Some st' in st and While (guard, s') evaluates to st'' in `st'.
//   - false then st'' is equal to st
// - None otherwise

    let rec stmntEval (s : stmnt) (st : state) =
        match s with
        | Skip -> Some st
        | Declare v -> declare v st
        | Assign (v, a) -> 
            match aexprEval a st with 
            | Some x -> setVar v x st
            | None -> None
        | Seq (s1, s2) -> 
            match stmntEval s1 st with
            | Some st' -> stmntEval s2 st'
            | None -> None
        | If (guard, s1, s2) ->
            match bexprEval guard st with
            | Some true -> stmntEval s1 st
            | Some false -> stmntEval s2 st
            | None -> None
        | While (guard, s') ->
            match bexprEval guard st with
            | Some true ->
                match stmntEval s' st with
                | Some st' ->
                    stmntEval (While (guard, s')) st'
                | None -> None
            | Some false -> 
                Some st
            | None -> None


// YELLOW EXERCISES - Part 2
// _________________________________________________________

// Exercise 5.14
// aexprEval forwards any errors from setVar and also, as in Assignment 4, 
// returns DivisionByZero if the user attempts to divide or modulo by 0.

    let rec aexprEval3 (a : aexpr) (st : state) =
        match a with
        | Num x -> Ok x
        | Var v -> getVar2 v st
        | Add (b, c) ->
            match aexprEval3 b st, aexprEval3 c st with
            | Ok x, Ok y -> Ok (x + y)
            | _, Error e -> Error e
            | Error e, _ -> Error e
        | Mul (b, c) ->
            match aexprEval3 b st, aexprEval3 c st with
            | Ok x, Ok y -> Ok (x * y)
            | _, Error e -> Error e
            | Error e, _ -> Error e
        | Div (b, c) ->
            match aexprEval3 b st, aexprEval3 c st with
            | Ok x, Ok y -> 
                if y = 0 then Error DivisionByZero
                else Ok (x / y)
            | _, Error e -> Error e
            | Error e, _ -> Error e
        | Mod (b, c) ->
            match aexprEval3 b st, aexprEval3 c st with
            | Ok x, Ok y ->
                if y = 0 then Error DivisionByZero
                else Ok (x % y)
            | _, Error e -> Error e
            | Error e, _ -> Error e


// Exercise 5.15
// bexprEval forwards any errors from aexprEval

    let rec bexprEval2 (b : bexpr) (st : state) = 
        match b with
        | TT -> Ok true
        | Eq (a, c) ->
            match aexprEval3 a st, aexprEval3 c st with
            | Ok x, Ok y -> Ok (x = y)
            | _, Error e -> Error e
            | Error e, _ -> Error e
        | Lt(a, c) ->
            match aexprEval3 a st, aexprEval3 c st with
            | Ok x, Ok y -> Ok (x < y)
            | _, Error e -> Error e
            | Error e, _ -> Error e
        | Conj (a, c) -> 
            match bexprEval2 a st, bexprEval2 c st with
            | Ok x, Ok y -> Ok (x && y)
            | _, Error e -> Error e
            | Error e, _ -> Error e
        | Not a -> 
            match bexprEval2 a st with
            | Ok x -> Ok (not x)
            | Error e -> Error e

// Exercise 5.16
// stmntEval forwards any errors from declare, setVar, aexprEval, and bexprEval.

    let rec stmntEval2 (s : stmnt) (st : state) =
        match s with
        | Skip -> Ok st
        | Declare v -> declare2 v st
        | Assign (v, a) -> 
            match aexprEval3 a st with 
            | Ok x -> setVar2 v x st
            | Error e -> Error e
        | Seq (s1, s2) -> 
            match stmntEval2 s1 st with
            | Ok st' -> stmntEval2 s2 st'
            | Error e -> Error e
        | If (guard, s1, s2) ->
            match bexprEval2 guard st with
            | Ok true -> stmntEval2 s1 st
            | Ok false -> stmntEval2 s2 st
            | Error e -> Error e
        | While (guard, s') ->
            match bexprEval2 guard st with
            | Ok true ->
                match stmntEval2 s' st with
                | Ok st' ->
                    stmntEval2 (While (guard, s')) st'
                | Error e -> Error e
            | Ok false -> 
                Ok st
            | Error e -> Error e


// RED EXERCISES - Part 2
// _________________________________________________________

// Exercise 5.20
// In stmntEval change the If and While cases such that whenever you enter the branches
// of an if, else, or while statement you push a new empty environment to the stack, 
// and you pop the top one off the stack when you exit the same branch. This effectively
// means that you will shadow any variable declared outside the branch (as getVar and 
// setVar will find the newly declared variables first rather than the old ones) and 
// that any variable declared inside a branch will be forgotten once you exit. Every pop
// should be preceeded by a push so you should never be able to pop from an empty stack.

// For this to work, we need versions of aexprEval and bexprEval that uses the state2 type.

    let rec aexprEval4 (a : aexpr) (st : state2) =
        match a with
        | Num x ->
            Ok x

        | Var v ->
            getVar3 v st

        | Add (b, c) ->
            match aexprEval4 b st, aexprEval4 c st with
            | Ok x, Ok y -> Ok (x + y)
            | Error e, _ -> Error e
            | _, Error e -> Error e

        | Mul (b, c) ->
            match aexprEval4 b st, aexprEval4 c st with
            | Ok x, Ok y -> Ok (x * y)
            | Error e, _ -> Error e
            | _, Error e -> Error e

        | Div (b, c) ->
            match aexprEval4 b st, aexprEval4 c st with
            | Ok x, Ok y ->
                if y = 0 then Error DivisionByZero
                else Ok (x / y)
            | Error e, _ -> Error e
            | _, Error e -> Error e

        | Mod (b, c) ->
            match aexprEval4 b st, aexprEval4 c st with
            | Ok x, Ok y ->
                if y = 0 then Error DivisionByZero
                else Ok (x % y)
            | Error e, _ -> Error e
            | _, Error e -> Error e


    let rec bexprEval3 (b : bexpr) (st : state2) =
        match b with
        | TT ->
            Ok true

        | Eq (a, c) ->
            match aexprEval4 a st, aexprEval4 c st with
            | Ok x, Ok y -> Ok (x = y)
            | Error e, _ -> Error e
            | _, Error e -> Error e

        | Lt (a, c) ->
            match aexprEval4 a st, aexprEval4 c st with
            | Ok x, Ok y -> Ok (x < y)
            | Error e, _ -> Error e
            | _, Error e -> Error e

        | Conj (a, c) ->
            match bexprEval3 a st, bexprEval3 c st with
            | Ok x, Ok y -> Ok (x && y)
            | Error e, _ -> Error e
            | _, Error e -> Error e

        | Not a ->
            match bexprEval3 a st with
            | Ok x -> Ok (not x)
            | Error e -> Error e

    // Now we can create the stmntEval3 function that uses the state2 type.

    let rec stmntEval3 (s : stmnt) (st : state2) =
        match s with
        | Skip ->
            Ok st

        | Declare v ->
            declare3 v st

        | Assign (v, a) ->
            match aexprEval4 a st with
            | Ok x -> setVar3 v x st
            | Error e -> Error e

        | Seq (s1, s2) ->
            match stmntEval3 s1 st with
            | Ok st' -> stmntEval3 s2 st'
            | Error e -> Error e

        | If (guard, s1, s2) ->
            match bexprEval3 guard st with
            | Ok true ->
                match stmntEval3 s1 (push st) with
                | Ok st' -> Ok (pop st')
                | Error e -> Error e

            | Ok false ->
                match stmntEval3 s2 (push st) with
                | Ok st' -> Ok (pop st')
                | Error e -> Error e

            | Error e ->
                Error e

        | While (guard, body) ->
            match bexprEval3 guard st with
            | Ok true ->
                match stmntEval3 body (push st) with
                | Ok st' ->
                    stmntEval3 (While (guard, body)) (pop st')
                | Error e ->
                    Error e

            | Ok false ->
                Ok st

            | Error e ->
                Error e

