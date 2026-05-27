
module tree
open System

type Tree =
    | Leaf of int
    | Node of Tree * Tree


let rec reg_sumTree tree =
    match tree with
    | Leaf x -> x
    | Node (x,y) -> (reg_sumTree x) + (reg_sumTree y)



let con_sumTree tree =
    let rec aux tree c =
        match tree with
        | Leaf v -> c v
        | Node (left, right) -> 
            aux left (fun leftResult -> 
                aux right (fun rightResult -> 
                    c (leftResult + rightResult)))
    aux tree (fun x -> x)

let max a b : int =
    if a > b then a else b
let rec reg_depthTree tree =
    match tree with
    | Leaf x -> 0
    | Node (x,y) -> 1 + max (reg_depthTree x) (reg_depthTree y)


let con_depthTree tree =

    let rec aux tree c =
        match tree with
        | Leaf v -> c 0
        | Node (left, right) -> 

            aux left (fun leftResult -> 

                aux right (fun rightResult -> 
                
                    c (max (1 + leftResult) (1 + rightResult))))

    aux tree (fun x -> x)


