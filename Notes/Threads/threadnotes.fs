// ============================================================================
// EXAM NOTES: Asynchronous & Parallel Programming
// Functional Programming 2026, ITU Copenhagen (Lecture 12)
// ============================================================================


// ============================================================================
// PART 1: TERMINOLOGY
// ============================================================================
//
// Process:      one running application, has its own memory
// Thread:       computation inside a process, own stack, shared heap
// Thread pool:  collection of reusable threads
// Synchronous:  blocks the thread until operation finishes
// Asynchronous: frees the thread while waiting, other work can happen
//
// THREE EXECUTION MODELS:
//   Sequential sync:    one task at a time, thread blocked during waits
//   Async sequential:   one task at a time, thread FREE during waits (let!)
//   Async parallel:     multiple tasks at same time (Async.Parallel)
//
// TWO KINDS OF PARALLELISM:
//   Data parallelism:   same operation on different data
//   Task parallelism:   different operations running simultaneously


// ============================================================================
// PART 2: ASYNC SYNTAX
// ============================================================================
//
// Async<'T> is a computation that EVENTUALLY produces a value of type 'T.
// It's a RECIPE — nothing runs until you execute it.
//
//   async { return 42 }    -- defines a recipe, doesn't run yet
//   Async.RunSynchronously (async { return 42 })  -- actually runs it → 42
//
//
// KEYWORDS INSIDE async { }:
//
//   let! x = asyncOp       -- await an async operation, bind result to x
//                              Thread is FREED during the wait
//   do! asyncOp            -- await but ignore the result (for Async<unit>)
//   let x = normalExpr     -- regular let, no waiting
//   return value           -- wrap a value and finish
//   return! asyncOp        -- tail-call another async (used in loops)
//
//
// KEY DIFFERENCE: let vs let!
//   let x = someFunc()     -- someFunc returns int, string, etc.
//   let! x = someAsync     -- someAsync returns Async<T>, let! unwraps to T


// ============================================================================
// PART 3: EXAMPLES OF ASYNC COMPUTATIONS
// ============================================================================

open System.Threading

// EXAMPLE 1: Simple async that returns immediately
let simple : Async<int> = async { return 42 }
// Run it:
// Async.RunSynchronously simple → 42

// EXAMPLE 2: Async that waits then returns
let delayed : Async<string> = async {
    do! Async.Sleep 1000       // do! because Sleep returns Async<unit>
    return "done"
}
// Async.RunSynchronously delayed → "done" (after 1 second)

// EXAMPLE 3: Chaining async operations
let chainExample : Async<int> = async {
    do! Async.Sleep 500
    let! result1 = async { return 10 }     // let! unwraps Async<int> to int
    let! result2 = async { return 20 }     // sequential: waits for result1 first
    let sum = result1 + result2            // regular let: no async needed
    return sum
}
// Async.RunSynchronously chainExample → 30

// EXAMPLE 4: Recursive async loop
let countTo n : Async<unit> = 
    let rec loop i = async {
        if i > n then return ()            // base case: stop
        else
            printfn "%d" i
            do! Async.Sleep 100
            return! loop (i + 1)           // return! for tail-call
    }
    loop 1


// ============================================================================
// PART 4: RUNNING ASYNC — THREE WAYS
// ============================================================================

// 1. Async.RunSynchronously : Async<'T> -> 'T
//    Blocks until done, returns the value.
//    Use for: testing, scripts, getting results.
let r1 = Async.RunSynchronously (async { return 42 })
// r1 = 42

// 2. Async.Start : Async<unit> -> unit
//    Fire and forget — returns immediately.
//    Only works with Async<unit>.
//    Use for: background tasks.
// Async.Start (async { do! Async.Sleep 5000; printfn "done!" })
// prints "done!" 5 seconds later, but doesn't block

// 3. Async.Parallel : seq<Async<'T>> -> Async<'T[]>
//    Runs ALL async computations in parallel.
//    Returns ONE async containing an array of results.
//    Still needs RunSynchronously to execute.
let r3 = 
    [async { return 1 }; async { return 2 }; async { return 3 }]
    |> Async.Parallel
    |> Async.RunSynchronously
// r3 = [|1; 2; 3|]

// Async.Ignore : Async<'T> -> Async<unit>
//    Throws away the result. Useful when you don't need values.


// ============================================================================
// PART 5: SEQUENTIAL VS PARALLEL — WITH EXAMPLES
// ============================================================================

// Simulated slow operation
let slowDouble x = async {
    do! Async.Sleep 1000
    return x * 2
}

// SEQUENTIAL: let! then let! — one after another
// Total time: 2 seconds (1 + 1)
let sequentialExample = async {
    let! a = slowDouble 5     // waits 1 sec → 10
    let! b = slowDouble 8     // THEN waits 1 sec → 16
    return (a, b)             // (10, 16)
}
// Async.RunSynchronously sequentialExample → (10, 16) after ~2 sec

// PARALLEL: Async.Parallel — all at once
// Total time: 1 second (both run simultaneously)
let parallelExample =
    [slowDouble 5; slowDouble 8]
    |> Async.Parallel
    |> Async.RunSynchronously
// parallelExample = [|10; 16|] after ~1 sec

// COMPACT PATTERN: map then parallel
// Very common in exams and assignments
let processMany (items : int list) : int[] =
    items
    |> List.map slowDouble         // list of Async<int>
    |> Async.Parallel              // one Async<int[]> running all in parallel
    |> Async.RunSynchronously      // execute and get int[]


// ============================================================================
// PART 6: PARALLEL PATTERNS FOR EXAMS
// ============================================================================

// PATTERN 1: List.map + Async.Parallel + RunSynchronously
// "Process each item in parallel and collect results"
let pattern1 (items : int list) : int[] =
    items
    |> List.map (fun x -> async { return x * 2 })
    |> Async.Parallel
    |> Async.RunSynchronously

// PATTERN 2: List comprehension + Async.Parallel + Ignore
// "Do something for every (i,j) pair in parallel, no return values"
let pattern2 (rows : int) (cols : int) =
    [ for i in 0..rows-1 do
        for j in 0..cols-1 do
            yield async { printfn "(%d,%d)" i j } ]
    |> Async.Parallel
    |> Async.Ignore
    |> Async.RunSynchronously

// PATTERN 3: Mutate a shared structure in parallel
// "Initialise each cell in parallel using side effects"
// CAREFUL: only safe if each thread writes to a DIFFERENT cell
let pattern3 (rows : int) (cols : int) (f : int -> int -> int) =
    let m = Array2D.create rows cols 0    // mutable 2D array
    [ for i in 0..rows-1 do
        for j in 0..cols-1 do
            yield async { m.[i,j] <- f i j } ]
    |> Async.Parallel
    |> Async.Ignore
    |> Async.RunSynchronously
    m   // return the mutated array

// PATTERN 4: Array.Parallel for data parallelism
// "Apply the same function to every element"
let pattern4 = Array.Parallel.map (fun n -> n * 2) [|1; 2; 3; 4; 5|]
// = [|2; 4; 6; 8; 10|]

// PATTERN 5: Task for simple task parallelism
// "Run two different computations simultaneously"
open System.Threading.Tasks
let pattern5 () =
    let r1 = Task.Factory.StartNew(fun () -> 5 + 1)
    let r2 = Task.Factory.StartNew(fun () -> 2 * 2)
    (r1.Result, r2.Result)    // blocks until both done → (6, 4)


// ============================================================================
// PART 7: CANCELLATION
// ============================================================================

// CancellationTokenSource creates tokens to cancel tasks

// EXAMPLE: Start two tasks, cancel only one
let cancellationExample () =
    let cts1 = new CancellationTokenSource()
    let cts2 = new CancellationTokenSource()

    let task name = async {
        let rec loop n = async {
            printfn "%s: %d" name n
            do! Async.Sleep 1000
            return! loop (n + 1)
        }
        do! loop 1
    }

    Async.Start(task "counter1", cts1.Token)   // uses token 1
    Async.Start(task "counter2", cts2.Token)   // uses token 2

    Thread.Sleep 3000     // let both run for 3 seconds
    cts2.Cancel()         // cancel only counter2, counter1 keeps going

// StartWithContinuations: handle success, error, cancellation separately
// EXAMPLE:
let continuationsExample () =
    let riskyTask = async {
        do! Async.Sleep 500
        return 10 / 0          // will throw exception
    }

    Async.StartWithContinuations(
        riskyTask,
        (fun result -> printfn "OK: %d" result),            // success
        (fun exn -> printfn "Error: %s" exn.Message),       // exception
        (fun _ -> printfn "Cancelled")                      // cancellation
    )


// ============================================================================
// PART 8: RACE CONDITIONS
// ============================================================================

// PROBLEM: parallel threads accessing shared mutable state
let raceConditionDemo () =
    let mutable n = 0
    seq [1..10000]
    |> Seq.map (fun _ -> async { n <- n + 1 })
    |> Async.Parallel
    |> Async.Ignore
    |> Async.RunSynchronously
    printfn "%d" n    // might print 9971 instead of 10000!

// WHY IT HAPPENS:
//   Thread 1: reads n=5, computes 6
//   Thread 2: reads n=5, computes 6 (before Thread 1 writes back)
//   Thread 1: writes n=6
//   Thread 2: writes n=6 ← one increment lost!

// SOLUTION 1: Don't share mutable state (functional approach)
let solution1 () =
    seq [1..10000]
    |> Seq.map (fun _ -> async { return 1 })
    |> Async.Parallel
    |> Async.RunSynchronously
    |> Array.sum          // each thread returns its own value, sum at end

// SOLUTION 2: Locking
let solution2 () =
    let mutable n = 0
    let lockObj = new System.Object()
    seq [1..10000]
    |> Seq.map (fun _ -> async { lock lockObj (fun _ -> n <- n + 1) })
    |> Async.Parallel
    |> Async.Ignore
    |> Async.RunSynchronously
    n   // always 10000, but threads wait for each other

// DANGER: Deadlock
//   Thread 1 has lock A, wants lock B
//   Thread 2 has lock B, wants lock A
//   Both wait forever → program freezes

// SOLUTION 3: MailboxProcessor (see Part 9)


// ============================================================================
// PART 9: MAILBOXPROCESSOR — SIMPLE VERSION
// ============================================================================

// CONCEPT:
//   Only ONE thread accesses the shared resource
//   Other threads send messages to a queue
//   Processor handles messages one by one — no race conditions
//   Sending a message is near-instant — clients don't block

// EXAMPLE 1: Simple printer (fire-and-forget messages)
let console = MailboxProcessor.Start(fun inbox ->
    let rec loop () = async {
        let! message = inbox.Receive()    // wait for next message
        printfn "%s" message              // process it
        return! loop ()                   // loop forever
    }
    loop ()
)

// Usage:
// console.Post("hello")        // fire and forget
// console.Post("world")        // messages processed in order


// ============================================================================
// PART 10: MAILBOXPROCESSOR — WITH STATE AND RESPONSES
// ============================================================================

// Message type: discriminated union with cases for each operation
// Cases that need a response include AsyncReplyChannel<T>
type CounterMsg =
    | Add of int                          // change state, no response needed
    | Get of AsyncReplyChannel<int>       // read state, send response back

let counter = MailboxProcessor.Start(fun inbox ->
    let mutable n = 0
    let rec loop () = async {
        let! msg = inbox.Receive()
        match msg with
        | Add x  -> n <- n + x            // modify state, no reply
        | Get ch -> ch.Reply n            // send current value back
        return! loop ()
    }
    loop ()
)

// SENDING MESSAGES:
//
// Post: fire and forget (no response)
//   counter.Post(Add 7)
//   Type: 'msg -> unit
//
// PostAndReply: send and wait for response
//   counter.PostAndReply(fun ch -> Get ch)
//   Type: (AsyncReplyChannel<'T> -> 'msg) -> 'T
//   The fun creates the message with a reply channel baked in
//   Returns the actual value (int, not Async<int>)

// USAGE EXAMPLE:
// counter.Post(Add 7)                                    // n becomes 7
// counter.Post(Add 3)                                    // n becomes 10
// counter.PostAndReply(fun ch -> Get ch)                  // returns 10


// ============================================================================
// PART 11: MAILBOXPROCESSOR — IMMUTABLE STATE VERSION
// ============================================================================

// Instead of mutable state, pass updated state to next loop iteration
// This is the pattern used in the assignment for Memory.fs

type StoreMsg =
    | Set of string * int
    | Get2 of string * AsyncReplyChannel<int option>
    | GetAll of AsyncReplyChannel<(string * int) list>

let store = MailboxProcessor.Start(fun inbox ->
    // State is passed as parameter, not mutable
    let rec loop (state : Map<string, int>) = async {
        let! msg = inbox.Receive()
        match msg with
        | Set (key, value) -> 
            return! loop (Map.add key value state)        // pass updated map
        | Get2 (key, ch) ->
            ch.Reply (Map.tryFind key state)              // reply with result
            return! loop state                            // state unchanged
        | GetAll ch ->
            ch.Reply (Map.toList state)
            return! loop state
    }
    loop Map.empty    // start with empty map
)

// USAGE:
// store.Post(Set ("x", 42))
// store.Post(Set ("y", 10))
// store.PostAndReply(fun ch -> Get2 ("x", ch))    // → Some 42
// store.PostAndReply(fun ch -> GetAll ch)          // → [("x",42); ("y",10)]


// ============================================================================
// PART 12: MAILBOXPROCESSOR — WRAPPING EXISTING FUNCTIONS
// ============================================================================
//
// The assignment pattern: wrap existing module functions in a mailbox
//
// You have existing functions like:
//   OldModule.getMem : int -> memory -> int option
//   OldModule.setMem : int -> int -> memory -> memory option
//
// Wrap them:
//
//   type MemMsg =
//       | GetMem of int * AsyncReplyChannel<int option>
//       | SetMem of int * int * AsyncReplyChannel<memory option>
//       | Alloc of int * AsyncReplyChannel<(memory * int) option>
//       | Free of int * int * AsyncReplyChannel<memory option>
//
//   type memory = Mem of MailboxProcessor<MemMsg>
//
//   let inbox s (i : MailboxProcessor<MemMsg>) =
//       let rec loop (mem : OldModule.memory) = async {
//           let! msg = i.Receive()
//           match msg with
//           | GetMem (ptr, ch) ->
//               ch.Reply (OldModule.getMem ptr mem)
//               return! loop mem                          // read: state unchanged
//           | SetMem (ptr, v, ch) ->
//               match OldModule.setMem ptr v mem with
//               | Some newMem -> 
//                   ch.Reply (Some newMem)
//                   return! loop newMem                   // write: pass new state
//               | None ->
//                   ch.Reply None
//                   return! loop mem                      // failed: state unchanged
//           // ... similar for Alloc, Free
//       }
//       loop (OldModule.empty s)
//
//   let empty s = Mem (MailboxProcessor.Start (inbox s))
//
//   // Public functions that clients call:
//   let getMem ptr (Mem mb) = mb.PostAndReply(fun ch -> GetMem (ptr, ch))
//   let setMem ptr v (Mem mb) = mb.PostAndReply(fun ch -> SetMem (ptr, v, ch))


// ============================================================================
// QUICK REFERENCE
// ============================================================================
//
// BUILDING:
//   async { }                    computation expression
//   let! x = asyncOp             await and bind (frees thread)
//   do! asyncOp                  await and ignore (for Async<unit>)
//   return value                 wrap and finish
//   return! asyncOp              tail-call another async
//
// RUNNING:
//   Async.RunSynchronously       block until done, get result
//   Async.Start                  fire and forget
//   Async.Parallel               run list in parallel → Async<'T[]>
//   Async.Ignore                 discard result → Async<unit>
//
// PARALLEL PATTERNS:
//   items |> List.map (fun x -> async { ... }) |> Async.Parallel
//   [ for i in 0..n-1 do yield async { ... } ] |> Async.Parallel
//   Array.Parallel.map f arr
//   Task.Factory.StartNew(fun () -> ...)
//
// SYNCHRONISATION:
//   Immutable data               no race conditions possible
//   lock obj (fun _ -> ...)      one thread at a time (deadlock risk)
//   MailboxProcessor             message queue (safe, no deadlocks)
//     .Post(msg)                 send, don't wait
//     .PostAndReply(fun ch -> msg)  send and wait for response
//
// CANCELLATION:
//   let cts = new CancellationTokenSource()
//   Async.Start(task, cts.Token)
//   cts.Cancel()
//   Async.StartWithContinuations(task, onOk, onErr, onCancel)
//
// MAILBOXPROCESSOR TEMPLATE:
//   type Msg = DoStuff of args | GetStuff of AsyncReplyChannel<T>
//   let mb = MailboxProcessor.Start(fun inbox ->
//       let rec loop state = async {
//           let! msg = inbox.Receive()
//           match msg with
//           | DoStuff args -> return! loop (update state args)
//           | GetStuff ch  -> ch.Reply (read state)
//                             return! loop state
//       }
//       loop initialState)
//
// ============================================================================