module safeList 
open System

let safeHead lst : int option =
    match lst with
    | []    -> None
    | x::xs -> Some (x)

let safeTail lst : int option =
    match lst with
    | [] -> None
    | x::xs -> 
        let rec getLast lst last: 'a =
            match lst with 
            |[] -> last
            |x::xs -> getLast xs x

        Some (getLast xs x)



let headPlusLast lst =
    match lst with
    | [] -> None
    | x::xs -> match safeHead lst with
               |None -> None
               |Some a -> match safeTail lst with
                             |None -> None
                             |Some b -> Some (a+b)


let (>>=) x f =
    match x with
    | Some a -> f a
    | None -> None

let ret x = Some x


let headPLustLast2 lst =
    safeHead lst >>= fun a -> safeTail lst >>= fun b -> ret(a+b)


    