module Assignment3
    
    let add5 x = x + 5
    let mul3 x = x * 3

    let add5mul3 x = add5 x |> mul3
    let add5mul3_2 = add5 >> mul3
    
    let add5_2 (f : ('a -> int)) (x : 'a) = f x |> (+) 5
    let mul3_2 (f : ('a -> int)) (x : 'a) = f x |> (*) 3
   
    let rec downto4 (f : (int -> 'a -> 'a)) (n : int) (e : 'a) = 
        match n with
        | n when n > 0 -> downto4 f (n-1) (f n e)
        | _ -> e
                
    let fac n = downto4 (fun i acc -> i * acc) n 1
    
    let range (g : (int -> 'a)) (n : int) = 
        downto4 (
            fun i acc -> 
                match i with 
                    | i when i <= 0 -> []
                    | i -> (g i)::acc
        ) n []
    
    let rec double = function
        | [] ->  []
        | x::xs -> (x*2)::double xs
        
    let double_2 lst = List.map (fun n -> n*2) lst
    
    let rec stringLength (lst : string list) =
        match lst with
        | [] -> []
        | x::xs -> x.Length::stringLength xs
        
    let stringLength_2 (lst : string list) = List.map (fun (s : string) -> s.Length) lst
    
    let rec keepEven = function
        | [] -> []
        | x::xs when x % 2 = 0 -> x::keepEven xs
        | x::xs -> keepEven xs
    
    let keepEven_2 (lst : int list) = List.filter (fun n -> n % 2 = 0) lst
            
    let rec keepLengthGT5 (lst : string list) = 
        match lst with 
            | [] -> []
            | x::xs when x.Length > 5 -> x :: keepLengthGT5 xs
            | x::xs -> keepLengthGT5 xs

    let keepLengthGT5_2 (lst : string list) = List.filter (fun (s : string) -> s.Length > 5) lst

    
    
    let rec sumPositive = function
        | [] -> 0
        | x::xs when x >= 0 -> x + sumPositive xs
        | x::xs -> sumPositive xs
        
    let sumPositive_2 lst = List.fold (
        fun n a -> 
            match a with
                | a when a >= 0 -> a + n
                | _ -> n    ) 0 lst
        
    let sumPositive_3 lst = List.filter (fun n -> n >= 0) lst |> List.fold (+) 0
        
    let add5mul3_3 (f : ('a -> int)) (x : 'a) = (f |> (add5_2 >> mul3_2)) x
 
    let mergeFuns (fs : ('a -> 'a) list) (x : 'a) = (List.fold (fun (a : ('a -> 'a)) (b : ('a -> 'a)) -> a >> b) id fs) x
        
    let facFuns (x : int) = downto4 (fun (n : int) (lst : (int -> int) list) -> (fun y -> n * y)::lst) x []
        
    let fac_2 x = mergeFuns (facFuns x) 1

    let removeOddIdx lst : 'a list = 

        let foldFunc (lst : ('a list), n : int) (elem : 'a) =
           match (lst, n) with
           | (x, n) when n % 2 = 0 -> ((x @ [elem]), n+1)
           | (x, n) -> (x, n+1)
        
        fst (List.fold foldFunc ([], 0) lst)
        
       
    let weird (strs : string list) = List.filter (fun (s : string) -> if s.Length % 2 = 0 then true else false) strs |> (List.fold (fun (state : string) (elem : string) -> state + string(elem.Length)) "")
    
   
    let rec insert _ = failwith "not implemented"
                
    let rec permutations _ = failwith "not implemented"