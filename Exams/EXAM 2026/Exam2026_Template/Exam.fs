module Exam2026_Template.Exam

    open JParsec.TextParser
    open System

    (* Question 1: Parametric Lucas numbers (25%) *)
    
    (* Question 1.1 *)
    let rec lucas_number (k : int) (n : int) = 
        match n with
        | 0 -> 0
        | 1 -> 1
        | n -> k * (lucas_number (k) (n-1)  ) + (lucas_number k (n-2))
    
    (* Question 1.2 *)
    let lucas_number_acc (k : int) (n : int) = 

        let rec aux (k : int) (n : int) (i : int) (l1 : int) (l2 : int) =
            match i > n with
            | true -> l1
            | false -> 
                match i with
                | 0 -> aux k n (i+1) 0 0
                | 1 -> aux k n (i+1) 1 l1
                | i -> aux k n (i+1) (k * l1 + l2) l1

        aux k n 0 0 0


    (* Question 1.3 *)
    let sqrt_approx (k : int) (n : int) = 2.0 * (float (lucas_number_acc k n ) / float (lucas_number_acc k (n-1) )) - float k
    
    (* Question 1.4 *)
    let approx_steps_needed (k : int) (epsilon : float) = 

        let rec aux (i : int) =
            let isUnder= 
                Math.Abs(float (lucas_number_acc k i) - (sqrt_approx k i)) < epsilon

            match isUnder with
            | true -> i
            | false -> aux (i+1)

        aux 2



    (* Question 1.5 *)
    let lucas_seq (k : int) = 
        Seq.initInfinite (fun a -> lucas_number_acc k a)

    
    (* Question 2: Code comprehension (25%) *)
    
    let rec foo x =
        function
        | a when x = a     -> a  
        | a when x % a = 0 -> a  
        | a                -> foo x (a + 1)  
          
    let bar =  
        function  
        | 0 -> 0  
        | 1 -> 1  
        | x -> foo x 2
        
    let rec baz x =  
        match bar x with  
        | y when x = y -> [y]  
        | y            -> y :: (baz (x / y))

    (* Question 2.1 *)
    (*
     
     Q: What do the functions `foo`, `bar`, and `baz` do? Focus on what they do rather than how they do it.
     A: 
        foo prints the closest number to a (increasing), which divides x
        bar finds the smallest number which dividex x.
        baz figures returns a list of all numbers we could divide x with until we could not divide it anymore.
        This means if we input 10, we get [2;5] because 10 / 2 = 5 and 5/5 = 1. We cannot divide 1 anymore.

     
     Q: What would be appropriate names for functions `foo`, `bar`, and `baz`.
     A: 
        foo = getClosestDivisor
        bar = getSmallestDivisor
        baz = getAllSmallestDivisors
     
     Q: For these functions to behave meaningfully, we must place a restriction on the input values. What restriction?
     A: 
        They may not be negative. It does produce an output, for example baz -10 is [2; 5; -1],
        but it is not ideal, because the diviser for bar has no base case for numbers under 0,
        and since 'a' in foo is incremented by one everytime the base cases do not match,
        a simply climbes towards the 32 integer max value.
    *)
    
    (* Question 2.2 *)
    

    let rec foo2 (a : int) (x : int) = 
        match x = a || x % a = 0 with
        | true -> a
        | false -> foo x (a + 1)
    
    (* Question 2.3 *)


    let baz_inverse (list : int list) = List.fold (fun (acc : int) (elem : int) -> elem * acc) 1 list
    
    (* Question 2.4 *)
    
    (*
      Q: One of the functions from Question 2.1 is not tail recursive.
      Explain which one and why. To make a compelling argument you must evaluate
      a function call of the function, similarly to what is done in
      Chapter 1.4 of HR, and reason about that evaluation. You need to make clear
      what aspects of the evaluation tell you that the function is not tail recursive.
      Keep in mind that all steps in an evaluation chain must evaluate to the
      same value (```(5 + 4) * 3 --> 9 * 3 --> 27```, for instance).
      
      A: Baz is not tail recursive, as it needs to wait on the "add to head of list" operation
      on each stack until all recursive calls has been completed. This can clearly be seen on
      the evaluation chain below:

          let rec baz x =  
        match bar x with  
        | y when x = y -> [y]  
        | y            -> y :: (baz (x / y))

        baz 50 -->
        2::[baz (50 / 2)] -->
        2::5::(baz (20 / 5)) -->
        2::5::5 -->
        [2;5;5]

        As can be seen on the evaluation chain above, the (::) operator can't evaluate or execute,
        before each of the baz recursive calls has been evaluated and we meet the base case.
        Therefor it is not tail recursive.

    *)
    
    (* Question 2.5 *)
    
    (*
    
          let rec baz x =  
        match bar x with  
        | y when x = y -> [y]  
        | y            -> y :: (baz (x / y))
    
    
    *)

    let cont (x : int) = 

        let rec aux (x : int) (f) =
            match bar x with
            | y when x = y -> f [y]
            | y -> aux (x / y) (fun a -> y::a)

        aux x id
    
    (* Question 3: The robbers language (25%) *)
    
    let explode (str : string) = [for c in str -> c]  

    let implode (cs : char list) = cs |> Array.ofList |> System.String  

    let isConsonant (c : char) = "bcdfghjklmnpqrstvwxz".IndexOf(System.Char.ToLower c) >= 0
        
    (* Question 3.1 *)
    
    let encode (str : string) = 

        let rec aux (chars : char list) (acc : string) =
            match chars with
            | [] -> acc
            | x::xs ->
                match isConsonant x with
                | true -> aux xs (acc + string x + "o" + string x)
                | false -> aux xs (acc + string x)
            
        
        aux (explode str) ""

    (* Question 3.2 *)
    
    let decode (str : string) = 

        let rec aux (chars : char list) (acc : string) =
            match chars with
            | x::y::z::xs ->
                match x = z && y = 'o' with
                | true -> aux xs (acc + string x)
                | false -> aux (y::z::xs) (acc + string x)
            | x::xs -> aux xs (acc + string x)
            | [] -> acc

        aux (explode str) ""



    (* Question 3.3 *)
    
    let encode_fun (f : (char -> string)) (str : string) = 

        let rec aux (chars : char list) (acc : string) =
            match chars with
            | [] -> acc
            | x::xs -> aux xs (acc + f x)
        
        aux (explode str) ""
    

    (*
        let encode (str : string) = 

        let rec aux (chars : char list) (acc : string) =
            match chars with
            | [] -> acc
            | x::xs ->
                match isConsonant x with
                | true -> aux xs (acc + string x + "o" + string x)
                | false -> aux xs (acc + string x)
    
    
    *)

    let encode2 (str : string)  = 
        encode_fun (fun (c : char) -> 
            match isConsonant c with
            | true -> string c + "o" + string c
            | false -> string c) str
        
    (* Question 3.4 *)
    
    let encodeConsonant = satisfy isConsonant |>> (fun c -> string c + "o" + string c)
    let noEncode = anyChar |>> (fun c -> string c)

    let parser_robbers_language = many (choice [encodeConsonant;noEncode]) |>> (fun (list : string list) -> List.fold (fun (acc : string) (elem : string) -> acc + elem) "" list)
    
    (* Question 3.5 *)
    


    let encode_par (str : string) (n : int) = 
        let array = str.Split " "
        let numInSeq= array.Length / n

        Array.chunkBySize numInSeq array
        |> Array.map (fun elem -> async {
            return encode (Array.fold (fun (acc : string) (elem : string) -> acc + " " + elem) "" elem)
        })
        |> Async.Parallel
        |> Async.RunSynchronously
        |> Array.fold (fun (acc : string) (elem : string) -> acc + " " + elem) ""



    (* Question 4: The N-Rooks problem (25%) *)
    
    (* Question 4.1 *)
    
    //Row first, then column

    type board = {
        game: Map<(int * int), bool>
        dimensions: int
    }
    
    let empty (n : int) : board = 

        let rec aux (square : (int * int)) b =
            match square with
            | x,y when x = n && y = n -> Map.add (x,y) false b
            | x,y when x = n && y < n -> aux (0, y+1) (Map.add (x,y) false b)
            | x,y when x < n && y <= n -> aux (x+1, y) (Map.add (x,y) false b)
    
        {game = (aux (0,0) Map.empty); dimensions = n}
    
    let get_dimension (b : board) = b.dimensions
    
    let has_rook (r : int) (c : int) (b : board) = Map.find (r,c) b.game
    
    (* Question 4.2 *)
        
    let place_rook (r : int) (c : int) (b : board) : board option = 

        let dimension = get_dimension b

        let rec aux (i : int) (checkColumn : bool) =
            match checkColumn with
            | true when i >= dimension -> true
            | false when i >= dimension -> aux 0 true
            | true ->
                match Map.find (r, i) b.game with
                | false -> aux (i+1) true
                | true -> false
            | false ->
                match Map.find (i,c) b.game with
                | false -> aux (i+1) false
                | true -> false

        match aux 0 false with
        | true -> Some ({dimensions = dimension; game = (Map.add (r,c) true b.game)})
        | false -> None

    
    let valid_solution (b : board) = 
        
        let n = get_dimension b
        
        let rec aux (square : (int * int)) (i : int) b =
            match square with
            | x,y when x = n && y = n -> if Map.find (x,y) b then (i+1) else i
            | x,y when x = n && y < n -> if Map.find (x,y) b then aux (0,y+1) (i+1) b else aux (0,y+1) (i) b
            | x,y when x < n && y <= n -> if Map.find (x,y) b then aux (x+1,y) (i+1) b else aux (x+1,y) (i) b

        aux (0,0) 0 b.game = n 
        
    (* Question 4.3 *)
    type chessMonad<'a> = CM of (board -> ('a * board) option)  

    let ret x = CM (fun h -> (Some (x, h)))    
    let fail  = CM (fun _ -> None)    
    let bind f (CM a)  =    
        CM (fun b ->    
        match a b with    
        | Some (x, b') ->    
            let (CM g) = f x    
            g b'          
        | None -> None)    

    let (>>=) a f = bind f a  
    let (>>>=) a b = a >>= (fun _ -> b)  
      
    let evalCM (CM f) N = f (empty N) 
            
    let place_rook2 (r : int) (c : int) = 
        CM (fun b -> 
            match place_rook r c b with
            | Some v -> Some ((), v)
            | None -> 
                let (CM f) = fail
                f b)
    
    let valid_solution2 = 
        CM (fun b ->
            Some (valid_solution b, b))

    (* Question 4.4 *)
        
    let rec create_solution (list : (int * int) list) = 
        match list with
        | [] -> valid_solution2
        | (r,c)::xs ->
            place_rook2 r c >>>= create_solution xs
    
    (* Question 4.5 *)
    
    type ChessBuilder() =
        member this.Bind(f, x)    = bind x f
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Combine(a, b) = a >>= (fun _ -> b)

    let chess = new ChessBuilder()
    
    let rec create_solution2 (list : (int * int) list) = chess {
        match list with
        | [] -> return! valid_solution2
        | (r,c)::xs ->
            do! place_rook2 r c
            return! create_solution2 xs
    }