module notes
// ============================================================================
// EXAM NOTES: Efficiency — Memory Model & Tail Recursion
// Functional Programming 2026, ITU Copenhagen (Lecture 8)
// ============================================================================
//
// KEY CONCEPTS
// ------------
// 
// THE MEMORY MODEL
//   - Memory is split into a STACK and a HEAP.
//   - The stack stores primitive values (ints, bools) and stack frames.
//   - The heap stores composite values (lists, tuples, closures).
//   - Every function call pushes a new stack frame onto the stack.
//   - The stack has a fixed size — too many frames causes a STACK OVERFLOW.
//
// THREE RECURSION STYLES
//
//   1. Regular recursion
//      - Pending work waits on the stack after each recursive call.
//      - Stack grows O(n), collapses back when base case is reached.
//      - Risk of stack overflow on large inputs.
//
//   2. Tail recursion (with accumulators)
//      - The recursive call is the LAST operation — no pending work.
//      - F# reuses the same stack frame → O(1) stack.
//      - Use an accumulator to carry the running result forward.
//      - Works when: operation is associative (sum, product, length)
//        or when reversing order is acceptable (reverse).
//
//   3. Continuation-passing style (CPS)
//      - Pending work is captured in a FUNCTION (closure) on the heap.
//      - The recursive call is still in tail position → O(1) stack.
//      - Closures accumulate on the heap → O(n) heap, but heap is
//        much larger and garbage-collected, so no overflow.
//      - Works for EVERYTHING, but especially needed when:
//        * Order must be preserved (map, filter, append, foldBack)
//        * There are multiple recursive calls (trees)
//
// CONTINUATION PATTERN (memorise this)
//   - Every branch of aux must call either aux or c — never return
//     a bare value.
//   - Base case: feed the base value to c (e.g. c 0, c [])
//   - Recursive case: aux rest (fun r -> c (... r ...))
//   - Start with: aux input (fun x -> x)
//
// WHEN TO USE WHICH
//   - Accumulator: sum, product, length, reverse, fold (left)
//   - Continuation: map, filter, flatten, append, collect, zip,
//                   foldBack, tree operations
//   - fold (left) is already tail-recursive — no conversion needed.
//   - foldBack needs continuations to be safe.
//
// ============================================================================


// ============================================================================
// PART 1: LIST FUNCTIONS
// ============================================================================


// --- SUM LIST ---------------------------------------------------------------

// Regular: O(n) stack — pending "x + ▢" at each level
let rec reg_sumList (lst : int list) = 
    match lst with
    | x :: xs -> x + reg_sumList xs
    | _ -> 0

// Tail-recursive: O(1) stack — acc carries the running total
let tail_sumList (lst : int list) =
    let rec aux (lst : int list) (acc : int) =
        match lst with
        | x :: xs -> aux xs (acc + x)
        | _ -> acc
    aux lst 0

// Continuation: O(1) stack, O(n) heap — closures capture each "+ x"
let con_sumList (lst : int list) =
    let rec aux (lst : int list) (c : int -> int) =
        match lst with
        | x :: xs -> aux xs (fun r -> c (x + r))
        | [] -> c 0
    aux lst (fun x -> x)


// --- MAP --------------------------------------------------------------------

// Regular: O(n) stack — pending "f x :: ▢" at each level
let rec reg_map f lst =
    match lst with
    | x :: xs -> f x :: reg_map f xs
    | [] -> []

// Continuation: accumulator would reverse the list, so use CPS
let con_map f lst =
    let rec aux lst c =
        match lst with
        | x :: xs -> aux xs (fun r -> c (f x :: r))
        | [] -> c []
    aux lst (fun x -> x)


// --- FILTER -----------------------------------------------------------------

// Regular: O(n) stack
let rec reg_filter p lst =
    match lst with
    | x :: xs -> if p x then x :: reg_filter p xs
                 else reg_filter p xs
    | [] -> []

// Continuation: when p x is false, pass c unchanged — no new closure needed
let con_filter p lst =
    let rec aux lst c =
        match lst with
        | x :: xs -> if p x then aux xs (fun r -> c (x :: r))
                     else aux xs c
        | [] -> c []
    aux lst (fun x -> x)


// --- REVERSE ----------------------------------------------------------------

// Regular: O(n) stack, also O(n²) time because of @ on each step
let rec reg_reverse lst = 
    match lst with
    | x :: xs -> reg_reverse xs @ [x]
    | [] -> []

// Tail-recursive: accumulator works perfectly here — consing onto
// the front of acc naturally reverses the order
let tail_reverse lst =
    let rec aux lst acc =
        match lst with 
        | x :: xs -> aux xs (x :: acc)
        | [] -> acc
    aux lst []


// --- FLATTEN ----------------------------------------------------------------

// Regular: O(n) stack — pending "x @ ▢" at each level
let rec reg_flatten lst =
    match lst with
    | x :: xs -> x @ reg_flatten xs
    | [] -> []

// Continuation: x is a whole sublist, appended in front of r
let con_flatten lst =
    let rec aux lst c =
        match lst with 
        | x :: xs -> aux xs (fun r -> c (x @ r))
        | [] -> c []
    aux lst (fun x -> x)


// --- COLLECT (map + flatten) ------------------------------------------------

// Regular: apply f (which returns a list), then flatten
let rec reg_collect f lst = 
    match lst with
    | x :: xs -> f x @ reg_collect f xs
    | [] -> []

// Continuation: like flatten but with f x instead of x
let con_collect f lst =
    let rec aux lst c =
        match lst with
        | x :: xs -> aux xs (fun r -> c (f x @ r))
        | [] -> c []
    aux lst (fun x -> x)


// --- ZIP --------------------------------------------------------------------

// Regular: pairs up elements from two lists
let rec reg_zip lst1 lst2 =
    match lst1, lst2 with
    | x :: xs, y :: ys -> (x, y) :: reg_zip xs ys
    | _, _ -> []

// Continuation: matching on two lists, otherwise same pattern
let con_zip lst1 lst2 =
    let rec aux lst1 lst2 c =
        match lst1, lst2 with
        | x :: xs, y :: ys -> aux xs ys (fun r -> c ((x, y) :: r))
        | _, _ -> c []
    aux lst1 lst2 (fun x -> x)


// --- FOLD (LEFT) ------------------------------------------------------------

// Already tail-recursive! The recursive call is the last operation.
// acc is updated at every step — no pending work on the stack.
// Processes left to right: ((acc op x1) op x2) op x3
let rec fold f acc lst =
    match lst with
    | x :: xs -> fold f (f acc x) xs
    | [] -> acc


// --- FOLDBACK (RIGHT) -------------------------------------------------------

// Regular: NOT tail-recursive — pending "f x ▢" at each level
// Processes right to left: x1 op (x2 op (x3 op acc))
let rec reg_foldBack f lst acc =
    match lst with
    | x :: xs -> f x (reg_foldBack f xs acc)
    | [] -> acc

// Continuation: needed because of right-to-left processing
let con_foldBack f lst acc =
    let rec aux lst c =
        match lst with
        | x :: xs -> aux xs (fun r -> c (f x r))
        | [] -> c acc
    aux lst (fun x -> x)

// NOTE: every list continuation above is a special case of foldBack:
//   map f lst       = foldBack (fun x r -> f x :: r) lst []
//   filter p lst    = foldBack (fun x r -> if p x then x :: r else r) lst []
//   flatten lst     = foldBack (fun x r -> x @ r) lst []
//   sumList lst     = foldBack (fun x r -> x + r) lst 0


// ============================================================================
// PART 2: TREE FUNCTIONS
// ============================================================================

type Tree =
    | Leaf of int
    | Node of Tree * Tree


// --- SUM TREE ---------------------------------------------------------------

// Regular: two recursive calls — pending "▢ + ▢" on the stack
let rec reg_sumTree tree =
    match tree with
    | Leaf x -> x
    | Node (left, right) -> reg_sumTree left + reg_sumTree right

// Continuation: NESTED continuations — recurse left, then inside
// that continuation recurse right, then combine
let con_sumTree tree =
    let rec aux tree c =
        match tree with
        | Leaf v -> c v
        | Node (left, right) -> 
            aux left (fun leftResult -> 
                aux right (fun rightResult -> 
                    c (leftResult + rightResult)))
    aux tree (fun x -> x)


// --- DEPTH TREE -------------------------------------------------------------

// Regular: take the max of both subtrees, add 1 for current level
let rec reg_depthTree tree =
    match tree with
    | Leaf _ -> 0
    | Node (left, right) -> 1 + max (reg_depthTree left) (reg_depthTree right)

// Continuation: same nested pattern as sumTree
let con_depthTree tree =
    let rec aux tree c =
        match tree with
        | Leaf _ -> c 0
        | Node (left, right) -> 
            aux left (fun leftResult -> 
                aux right (fun rightResult -> 
                    c (1 + max leftResult rightResult)))
    aux tree (fun x -> x)


// ============================================================================
// QUICK REFERENCE: THE CONTINUATION TEMPLATE
// ============================================================================
//
// For lists:
//
//   let con_FUNC lst =
//       let rec aux lst c =
//           match lst with
//           | x :: xs -> aux xs (fun r -> c (DO_SOMETHING_WITH x AND r))
//           | [] -> c BASE_VALUE
//       aux lst (fun x -> x)
//
// For trees:
//
//   let con_FUNC tree =
//       let rec aux tree c =
//           match tree with
//           | Leaf v -> c (LEAF_VALUE)
//           | Node (left, right) ->
//               aux left (fun lr ->
//                   aux right (fun rr ->
//                       c (COMBINE lr AND rr)))
//       aux tree (fun x -> x)
//
// ============================================================================