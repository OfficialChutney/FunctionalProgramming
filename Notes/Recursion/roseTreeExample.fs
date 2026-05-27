module roseTree

type RoseTree = Leaf | Node of int * Children
and Children = RoseTree list

// Mutual recursion (AND)

let rec sumRose tree =
    match tree with
    | Leaf -> 0
    | Node (v, rest) -> v + sumChildren rest

and sumChildren (tree : RoseTree list) =
    match tree with
    |x::xs -> sumRose x + sumChildren xs
    |[] -> 0



//Continuation version
let rec sumRoseC tree c =
    match tree with
    | Leaf -> c 0
    | Node (v, rest) -> sumChildrenC rest (fun r -> c(r+v))
and sumChildrenC (tree : RoseTree list) c =
    match tree with
    | x :: xs -> sumRoseC x (fun s -> sumChildrenC xs (fun s' -> c (s + s')))
    | [] -> c 0