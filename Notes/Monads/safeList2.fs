module safeList2
// ============================================================================
// EXAM NOTES: Monads — From Simple to State Monad
// Functional Programming 2026, ITU Copenhagen (Lecture 10)
// ============================================================================
//
// This file builds up monads step by step:
//   Part 1: The problem (nested pattern matching)
//   Part 2: Option monad (>>= and ret)
//   Part 3: Computation expressions (let! and return)
//   Part 4: State monad (the abstract part)
//   Part 5: Building state monads with SM
//   Part 6: Chaining state monads
//   Part 7: Patterns to memorise
//
// ============================================================================


// ============================================================================
// PART 1: THE PROBLEM — WHY MONADS EXIST
// ============================================================================

// When functions can fail (return option), chaining them requires
// nested pattern matches that repeat the same boilerplate.

let safeDivide a b : int option =
    if b = 0 then None else Some (a / b)

// WITHOUT monads — ugly nested matching:
let chainedCalcUgly a b c : int option =
    match safeDivide a b with
    | Some r1 -> match safeDivide r1 c with
                 | Some r2 -> Some r2
                 | None    -> None
    | None    -> None

// Every level repeats: "if None return None, if Some unwrap and continue"


// ============================================================================
// PART 2: OPTION MONAD — >>= AND ret
// ============================================================================

// ret: wrap a plain value in Some (put it on the success track)
let ret x = Some x

// >>=: the "and then" operator / pipe that can derail
//   Left side: an option value
//   Right side: a function that takes the unwrapped value
//   If left is Some: unwrap and feed to the function
//   If left is None: skip the function, return None
let (>>=) x f =
    match x with
    | Some a -> f a
    | None   -> None

// WITH monads — clean chaining, no nesting:
let chainedCalc a b c : int option =
    safeDivide a b >>= fun r1 ->
    safeDivide r1 c >>= fun r2 ->
    ret r2

// Read as: "divide a by b, AND THEN divide by c, AND THEN return"
// If anything fails, >>= short-circuits to None automatically


// ============================================================================
// PART 3: COMPUTATION EXPRESSIONS — SYNTACTIC SUGAR
// ============================================================================

// Computation expressions make >>= chains look like normal code.
// You define a builder class that maps keywords to monad operations:
//
//   type OptionBuilder() =
//       member bld.Bind(a, f)    = a >>= f      // handles let!
//       member bld.Return e      = ret e         // handles return
//       member bld.ReturnFrom e  = e             // handles return!
//   let ev = OptionBuilder()
//
// TRANSLATION RULES (how F# converts the sugar):
//   let! x = e; rest   →   Bind(e, fun x -> rest)    i.e.  e >>= fun x -> rest
//   do! e; rest         →   Bind(e, fun () -> rest)   i.e.  e >>= fun () -> rest
//   return v            →   Return v                  i.e.  ret v
//   return! e           →   ReturnFrom e              i.e.  e (no wrapping)
//
// So this:
//   ev { let! r1 = safeDivide a b
//        let! r2 = safeDivide r1 c
//        return r2 }
//
// Is exactly the same as:
//   safeDivide a b >>= fun r1 ->
//   safeDivide r1 c >>= fun r2 ->
//   ret r2
//
// let! = >>= (bind the result and continue)
// return = ret (wrap the final value)
// return! = return something already wrapped (avoid double-wrapping)
// do! = >>= but ignore the value (for operations returning unit)


// ============================================================================
// PART 4: STATE MONAD — THE ABSTRACT PART
// ============================================================================
//
// THE IDEA:
// Sometimes functions need to read or modify some shared state
// (like a variable map, a counter, memory). Passing state around
// manually is tedious. The state monad hides the state threading.
//
// A state monad is a FUNCTION waiting to be run.
// It sits in a box (SM) doing nothing until you give it a state.
//
// Think of it like a recipe:
//   - SM wraps the recipe
//   - The recipe says "give me a state, and I'll give you a result + new state"
//   - evalState is what actually runs the recipe with a starting state
//
// THE TYPE:
//   type 'a stateMonad = SM of (state -> ('a * state) option)
//
//   Breaking this down:
//   - SM: just a tag/wrapper, so F# knows it's a state monad
//   - (state -> ...): a function waiting for a state
//   - ('a * state): a tuple of (the value produced, the updated state)
//   - option: the whole thing can fail (None)
//
//   'a is what the monad RETURNS (int, bool, unit, etc.)
//   state changes are HIDDEN inside — that's the whole point

module StateMonadNotes =

    // Using int as our state for simplicity
    type 'a stateMonad = SM of (int -> ('a * int) option)


    // ========================================================================
    // PART 5: THE THREE BUILDING BLOCKS — ret, fail, bind
    // ========================================================================

    // ret: "I have a value, wrap it up, don't touch the state"
    //   - Puts the value on the success track
    //   - State passes through unchanged
    //   - Example: ret 42 creates a monad that, when run with ANY state,
    //     returns Some (42, thatSameState)
    let ret x = SM (fun st -> Some (x, st))

    // fail: "something went wrong, abort everything"
    //   - Ignores the state entirely
    //   - Returns None no matter what
    //   - Once you hit fail, >>= will skip everything after it
    let fail : 'a stateMonad = SM (fun _ -> None)

    // bind: "run the first thing, then run the second thing with the result"
    //   Step by step:
    //   1. Unwrap the first monad to get function f
    //   2. Run f with the current state st
    //   3. If it succeeded (Some (x, st')):
    //      a. Pass value x to function g, which returns a new monad (SM h)
    //      b. Unwrap that monad to get function h
    //      c. Run h with the UPDATED state st'
    //   4. If it failed (None): return None (skip everything)
    let bind (SM f) g =
        SM (fun st ->
            match f st with
            | Some (x, st') -> let (SM h) = g x in h st'
            | None -> None)

    // >>= is just bind as an infix operator
    let (>>=) a f = bind a f

    // >>>= is bind but throwing away the value
    // Used when an operation changes state but returns unit
    // Example: incrementM >>>= getCountM
    //   "increment the state, then get the count"
    //   We don't care about the () that incrementM returns
    let (>>>=) a b = a >>= (fun _ -> b)


    // ========================================================================
    // PART 6: BUILDING YOUR OWN STATE MONADS WITH SM
    // ========================================================================
    //
    // There are TWO patterns for building state monads:
    //
    // PATTERN A: Change state, no meaningful return value
    //   Use when: the operation modifies the state (declare, setVar, increment)
    //   Template: SM (fun st -> Some ((), newState))
    //   The () means "I don't return anything useful, I just change state"
    //
    // PATTERN B: Read state, don't change it
    //   Use when: the operation reads from the state (getVar, getMem, getCount)
    //   Template: SM (fun st -> Some (valueFromState, st))
    //   The state st is returned unchanged
    //
    // PATTERN C: Read state, might fail
    //   Use when: the operation could fail (variable not found, bad address)
    //   Template: SM (fun st -> match lookupSomething st with
    //                           | Some v -> Some (v, st)
    //                           | None   -> None)

    // --- Example plain functions (not monadic) ---
    let increment (st : int) : int = st + 1
    let getCount (st : int) : int = st

    // --- Wrapping them as monads ---

    // PATTERN A: changes state, returns unit
    // "When given a state, increment it. Return () as the value."
    let incrementM : unit stateMonad =
        SM (fun st -> Some ((), increment st))
    //                       ^^  ^^^^^^^^^^^^
    //                       |   new state (incremented)
    //                       value (nothing useful, just ())

    // PATTERN B: reads state, doesn't change it
    // "When given a state, return its value. Don't change the state."
    let getCountM : int stateMonad =
        SM (fun st -> Some (getCount st, st))
    //                      ^^^^^^^^^^^  ^^
    //                      |            state unchanged
    //                      value (the count we read)

    // PATTERN A again: a "set" operation
    // "When given a state, replace it with the new value."
    let setCountM (n : int) : unit stateMonad =
        SM (fun st -> Some ((), n))
    //                       ^^ ^
    //                       |  new state (replaced entirely)
    //                       value (nothing useful)

    // PATTERN C: might fail
    // "When given a state, check if it's positive. If not, fail."
    let getPositiveM : int stateMonad =
        SM (fun st -> if st > 0 then Some (st, st) else None)


    // ========================================================================
    // evalState: ACTUALLY RUNNING A STATE MONAD
    // ========================================================================
    //
    // Nothing happens until you call evalState.
    // All the SM functions are just recipes — evalState runs the recipe.
    //
    // It takes a starting state and a monad, runs the monad's function
    // with that state, and returns just the value (throws away final state).

    let evalState (st : int) (SM f) : 'a option =
        match f st with
        | Some (value, _) -> Some value    // succeeded: return the value
        | None            -> None          // failed: return None


    // ========================================================================
    // PART 7: CHAINING — PUTTING IT ALL TOGETHER
    // ========================================================================

    // Example chains and what they do:

    // incrementM >>>= getCountM |> evalState 0
    //   1. Start with state 0
    //   2. incrementM: state becomes 1, value is () (ignored by >>>=)
    //   3. getCountM: reads state 1, returns 1
    //   4. evalState extracts: Some 1

    // incrementM >>>= incrementM >>>= getCountM |> evalState 0
    //   1. Start with state 0
    //   2. First incrementM: state becomes 1
    //   3. Second incrementM: state becomes 2
    //   4. getCountM: reads state 2, returns 2
    //   5. evalState extracts: Some 2

    // setCountM 42 >>>= getCountM |> evalState 0
    //   1. Start with state 0
    //   2. setCountM 42: state becomes 42
    //   3. getCountM: reads state 42, returns 42
    //   4. evalState extracts: Some 42

    // fail >>>= getCountM |> evalState 0
    //   1. Start with state 0
    //   2. fail: returns None
    //   3. >>= sees None, skips getCountM
    //   4. evalState extracts: None

    // getCountM >>= fun count -> setCountM (count * 2) >>>= getCountM |> evalState 5
    //   1. Start with state 5
    //   2. getCountM: reads 5, binds to count
    //   3. setCountM (5 * 2): state becomes 10
    //   4. getCountM: reads 10, returns 10
    //   5. evalState extracts: Some 10


// ============================================================================
// QUICK REFERENCE — PATTERNS TO MEMORISE
// ============================================================================
//
// BUILDING MONADS (inside StateMonad.fs — you use SM directly):
//
//   Change state:    SM (fun st -> Some ((), modifiedState))
//   Read state:      SM (fun st -> Some (valueFromState, st))
//   Can fail:        SM (fun st -> if ok then Some (value, st) else None)
//
// USING MONADS (inside Eval.fs — you never see SM, only >>= and ret):
//
//   Chain with value:     a >>= fun x -> ...    or    let! x = a
//   Chain ignoring value: a >>>= b              or    do! a
//   Wrap a value:         ret x                 or    return x
//   Pass through:         (no operator)         or    return! x
//   Fail:                 fail
//
// RUNNING MONADS:
//   evalState initialState someMonad    → 'a option
//
// KEY INSIGHT:
//   When BUILDING monads (SM constructor): you work with raw state
//   When USING monads (>>= and ret): state is completely hidden
//   This separation is the whole point — eval code never touches state
//
// ============================================================================