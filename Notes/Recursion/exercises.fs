module simple
open System
let rec reg_sumList (lst : int list) = 
    match lst with
    |x::xs -> x + reg_sumList xs
    |_ -> 0


let tail_sumList (lst : int list) =

    let rec aux (lst : int list) (acc : int) =
        match lst with
        |x::xs-> aux xs (acc + x)
        |_ -> acc

    aux lst


let con_sumList (lst : int list)  =

    let rec aux (lst : int list) (c : int -> int) =
        match lst with
        |x::xs -> aux xs (fun r -> c (x+r) )
        |[] -> c 0

    aux lst (fun x -> x)

let rec reg_map lst f =
    match lst with
    |x::xs -> f x :: reg_map xs f
    |[] -> []


let con_map lst f =
    let rec aux lst c =
        match lst with
        | x :: xs -> aux xs (fun r -> c (f x :: r))
        | [] -> c []
    aux lst (fun x -> x)


let rec reg_filter lst p =
    match lst with
    | x::xs -> if p x then x :: reg_filter xs p
                      else reg_filter xs p
    |[] -> []


let con_filter lst p =
    
    let rec aux lst c =
        match lst with
        | x :: xs -> if p x then aux xs (fun r -> c (x :: r))
                            else aux xs c  //else aux xs (fun r -> c r)
        |[] -> c []

    aux lst (fun x -> x)


let rec reg_reverse lst = 
    match lst with
    | x::xs -> reg_reverse xs @ [x]
    | [] -> []


let tail_reverse lst =

    let rec aux lst acc =
        match lst with 
        |x::xs -> aux xs (x::acc)
        |[] -> acc

    aux lst []


let rec reg_flatten lst =
    match lst with
    |x::xs -> x @ reg_flatten xs
    |[] -> []


let con_flatten lst =
    let rec aux lst c =
        match lst with 
        | x :: xs -> aux xs (fun r -> c (x @ r))   
        | [] -> c []

    aux lst (fun x-> x)


let rec reg_collect lst f= 
    match lst with
    | x :: xs -> f x @ reg_collect xs f
    | [] -> []


let con_collect lst f =

    let rec aux lst c =
        match lst with
        | x :: xs -> aux xs (fun r -> c (f x @ r))
        | [] -> c []

    aux lst (fun x -> x)


let rec reg_zip lst1 lst2 =
    match lst1, lst2 with
    |x::xs, y::ys -> (x,y) :: reg_zip xs ys
    |_, _ -> []

let con_zip lst1 lst2 =
    
    let rec aux lst1 lst2 c =
        match lst1, lst2 with
        |x :: xs, y :: ys -> aux xs ys (fun r -> c ((x,y) :: r))
        |_,_ -> c []

    aux lst1 lst2 (fun x -> x)


let rec reg_foldBack f lst acc =
    match lst with
    | x :: xs -> f x (reg_foldBack f xs acc)
    | [] -> acc


let con_foldBack f lst acc =
    let rec aux lst c =
        match lst with
        | x :: xs -> aux xs (fun r -> c (f x r))
        | [] -> c acc
    aux lst (fun x -> x)


let rec reg_fold f acc lst =
    match lst with
    | x :: xs -> reg_fold f (f acc x) xs
    | [] -> acc


let tail_fold f acc lst =
    let rec aux lst acc =
        match lst with
        | x :: xs -> aux xs (f acc x)
        | [] -> acc
    aux lst acc

let con_fold f lst acc =
    
    let rec aux lst c =
        match lst with
        | x :: xs -> aux xs (fun r -> c(f r x))
        | [] -> c acc

    aux lst (fun x -> x)
