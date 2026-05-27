// ============================================================================
// EXAM NOTES: From Regular Functions to State Monads
// Pattern guide for FP exam questions
// ============================================================================
//
// EXAM PATTERN:
//   Step 1: Define a type (stack, tree, etc.)
//   Step 2: Write a regular function using pattern matching
//   Step 3: Define push/pop/get/set as state monads using SM
//   Step 4: Rewrite the function using computation expressions
//   Step 5: Write a parser for the input format
//
// This file walks through each step using the stack machine exam question.
// ============================================================================


// ============================================================================
// STEP 1: DEFINE THE TYPE
// ============================================================================

// The simplest stack is just a list.
// Push = cons onto front, pop = take from front.
type stack = int list
let emptyStack () : stack = []

// The commands our stack machine understands:
type cmd = Push of int | Add | Mult
type stackProgram = cmd list


// ============================================================================
// STEP 2: REGULAR FUNCTION (no monads)
// ============================================================================
//
// The regular version passes the stack around explicitly as 'acc'.
// You handle errors with failwith.
// This is always the first question on the exam.

let runStackProgram (prog : stackProgram) : int = 
    let rec calc recipe acc : int list =
        match recipe with
        | [] -> acc
        | x :: xs ->
            match x with
            | Push v -> calc xs (v :: acc)
            | Add    -> match acc with
                        | a :: b :: ls -> calc xs ((a + b) :: ls)
                        | _            -> failwith "empty stack"
            | Mult   -> match acc with
                        | a :: b :: ls -> calc xs ((a * b) :: ls)
                        | _            -> failwith "empty stack"

    match calc prog [] with
    | [x] -> x                      // exactly one result: return it
    | _   -> failwith "empty stack" // empty or multiple: ill-formed

// KEY OBSERVATION: 
//   - acc (the stack) is threaded through every recursive call
//   - errors are handled with failwith
//   - the stack is visible everywhere
//
// The state monad version will HIDE the stack and replace failwith with
// monadic fail.


// ============================================================================
// STEP 3: DEFINE THE STATE MONAD + push/pop
// ============================================================================
//
// The state monad wraps a function: stack -> ('a * stack) option
//   'a = the return value
//   stack = the state being threaded through
//   option = can fail (None)
//
// These are PROVIDED on the exam. You don't write them yourself.
// But you MUST understand what they do.

type StateMonad<'a> = SM of (stack -> ('a * stack) option)

// ret: wrap a value, don't change state, don't fail
//   "I have a value, put it on the success track"
let ret x = SM (fun s -> Some (x, s))

// fail: always fail regardless of state
//   "Something went wrong, derail the whole chain"
let fail = SM (fun _ -> None)

// bind: chain two monadic operations
//   Run the first one. If it succeeds, feed the result to the next.
//   If it fails, the whole chain fails.
let bind f (SM a) : StateMonad<'b> = 
    SM (fun s -> 
        match a s with 
        | Some (x, s') -> 
            let (SM g) = f x   // f takes the value, returns a new SM
            g s'               // run the new SM with the updated state
        | None -> None)        // first failed → skip everything

let (>>=) x f = bind f x
// >>>= is >>= but ignoring the value (for unit operations like push)
let (>>>=) x y = x >>= (fun _ -> y)

// evalSM: actually RUN the state monad from an empty stack
//   Returns ('a * stack) option
let evalSM (SM f) = f (emptyStack ())


// ========================================================================
// NOW YOU WRITE: push and pop
// ========================================================================
//
// These break the SM abstraction — you construct SM directly.
// You use these to build the bridge between the state monad world
// and the actual stack operations.
//
// TWO PATTERNS (same as the monad notes):
//   Change state: SM (fun st -> Some ((), newState))
//   Read state:   SM (fun st -> Some (value, st))
//   Can fail:     SM (fun st -> match ... with | ... -> Some | ... -> None)

// push: add a value to the top of the stack
//   Changes state (adds to stack), returns nothing useful (unit)
//   Never fails
let push (x : int) : StateMonad<unit> = 
    SM (fun st -> Some ((), x :: st))
//                       ^^  ^^^^^^^
//                       |   new state: x pushed onto stack
//                       return value: unit (nothing useful)

// pop: remove and return the top value
//   Reads AND changes state (removes from stack)
//   CAN FAIL if stack is empty
let pop : StateMonad<int> = 
    SM (fun st -> 
        match st with
        | x :: xs -> Some (x, xs)    // success: return top, new state is rest
        | []      -> None)           // fail: empty stack
//                   ^^^^
//                   monadic failure — >>= will skip everything after this


// ============================================================================
// STEP 4: REWRITE USING COMPUTATION EXPRESSIONS
// ============================================================================
//
// The computation expression builder translates let!/do!/return
// into bind/ret calls.

type StateBuilder() =
    member this.Bind(f, x)    = bind x f     // let! and do!
    member this.Return(x)     = ret x        // return
    member this.ReturnFrom(x) = x            // return!
    member this.Combine(a, b) = a >>= (fun _ -> b)  // sequencing

let state = new StateBuilder()

// NOW: rewrite runStackProgram using push/pop inside state { }
//
// KEY DIFFERENCES FROM THE REGULAR VERSION:
//   Before (regular):              After (monadic):
//   ─────────────────              ────────────────
//   acc is a parameter             stack is hidden inside SM
//   failwith "..."                 pop returns None (monadic fail)
//   calc xs (v :: acc)             do! push v; return! loop xs
//   match acc with                 let! a = pop; let! b = pop
//     | a::b::ls -> ...              (pop handles the empty case)
//   return a value                 return value

let rec runStackProg2 (prog : stackProgram) : StateMonad<int> = state {
    match prog with
    | [] -> 
        // Base case: program is done, pop the result
        let! result = pop
        return result

    | cmd :: xs -> 
        // Process one command, then recurse
        do! match cmd with
            | Push v -> push v
            | Add    -> state { let! a = pop
                                let! b = pop
                                do! push (a + b) }
            | Mult   -> state { let! a = pop
                                let! b = pop
                                do! push (a * b) }
        return! runStackProg2 xs
}

// USAGE:
//   [Push 5; Push 4; Add] |> runStackProg2 |> evalSM |> Option.map fst
//   → Some 9
//
//   [Add] |> runStackProg2 |> evalSM |> Option.map fst
//   → None  (pop failed on empty stack — monadic failure propagated)


// ============================================================================
// STEP 5: PARSER (uses JParsec combinators)
// ============================================================================
//
// The exam often asks you to parse the textual format into the cmd type.
// Format:
//   "PUSH 5\nPUSH 4\nADD\nMULT"
//
// Parser for each command:
//   let pPush = pstring "PUSH" >>. spaces >>. pint32 |>> Push
//     Read "PUSH", skip spaces, read an int, wrap in Push
//
//   let pAdd = pstring "ADD" |>> fun _ -> Add
//     Read "ADD", return the Add command
//
//   let pMult = pstring "MULT" |>> fun _ -> Mult
//     Read "MULT", return the Mult command
//
// Single command parser:
//   let pCmd = pPush <|> pAdd <|> pMult
//     Try each alternative
//
// Full program parser (commands separated by newlines):
//   let pProg = sepBy (spaces >>. pCmd .>> spaces) (pchar '\n')
//     Parse commands with optional spaces, separated by newlines
//
// Run it:
//   run pProg "PUSH 5\nPUSH 4\nADD\nPUSH 8\nMULT"
//   → Ok [Push 5; Push 4; Add; Push 8; Mult]


// ============================================================================
// SIDE-BY-SIDE: REGULAR vs MONADIC
// ============================================================================
//
// REGULAR VERSION:
//   let rec calc recipe acc =
//       match recipe with
//       | [] -> acc
//       | Push v :: xs -> calc xs (v :: acc)
//       | Add :: xs    -> match acc with
//                         | a :: b :: ls -> calc xs ((a+b) :: ls)
//                         | _ -> failwith "empty stack"
//       | Mult :: xs   -> match acc with
//                         | a :: b :: ls -> calc xs ((a*b) :: ls)
//                         | _ -> failwith "empty stack"
//
// MONADIC VERSION:
//   let rec runStackProg2 prog = state {
//       match prog with
//       | [] -> let! result = pop
//               return result
//       | Push v :: xs -> do! push v
//                         return! runStackProg2 xs
//       | Add :: xs    -> let! a = pop
//                         let! b = pop
//                         do! push (a + b)
//                         return! runStackProg2 xs
//       | Mult :: xs   -> let! a = pop
//                         let! b = pop
//                         do! push (a * b)
//                         return! runStackProg2 xs
//   }
//
// WHAT CHANGED:
//   acc                    → hidden inside SM
//   v :: acc               → do! push v
//   match acc with a::b::  → let! a = pop; let! b = pop
//   (a+b) :: ls            → do! push (a+b)
//   failwith               → pop returns None (automatic)
//   calc xs newAcc         → return! runStackProg2 xs
//   return acc             → let! result = pop; return result


// ============================================================================
// COMMON EXAM PATTERNS
// ============================================================================
//
// 1. DEFINE SM FUNCTIONS (push/pop/getVar/setVar/alloc/etc.)
//    Always the same two patterns:
//      Change state:  SM (fun st -> Some ((), modify st))
//      Read state:    SM (fun st -> Some (readFrom st, st))
//      Can fail:      SM (fun st -> if ok then Some (v, st) else None)
//
// 2. CONVERT REGULAR → MONADIC
//    Replace explicit state threading with let!/do!/return
//    Replace failwith with monadic fail (via pop/getVar returning None)
//    Replace recursive accumulator calls with return! recurse rest
//
// 3. COMPUTATION EXPRESSION KEYWORDS
//    let! x = e     "await and bind" — e is SM<T>, x is T
//    do! e          "await and ignore" — e is SM<unit>
//    return v       "wrap and finish" — v is a plain value
//    return! e      "tail-call" — e is already SM<T>
//
// 4. RUNNING THE MONAD
//    someMonad |> evalSM             → ('a * stack) option
//    someMonad |> evalSM |> Option.map fst  → 'a option (just the value)
//
// ============================================================================