module Exam2023

    open System.Net.Mime
    open System.Runtime.InteropServices.JavaScript

(* If you are importing this into F# interactive then comment out
   the line above and remove the comment for the line bellow.

   Do note that the project will not compile if you do this, but 
   it does allow you to work in interactive mode and you can just remove the '=' 
   to make the project compile again.

   You will also need to load JParsec.fs. Do this by typing
   #load "JParsec.fs" 
   in the interactive environment. You may need the entire path.

   Do not remove the module declaration (even though that does work) because you may inadvertently
   introduce indentation errors in your code that may be hard to find if you want
   to switch back to project mode. 

   Alternative, keep the module declaration as is, but load ExamInteractive.fsx into the interactive environment
   *)
(*
 module ReExam2023 = 
 *)

(* 1: Arithmetic *)
    
    type arith =
    | Num of int
    | Add of arith * arith
    
    let p1 = Num 42
    let p2 = Add(Num 5, Num 3)
    let p3 = Add(Add(Num 5, Num 3), Add(Num 7, Num (-9)))
    
(* Question 1.1: Evaluation *)
    let rec eval =
        function
        | Num x       -> x
        | Add (x, y)  -> eval x + eval y
    
(* Question 1.2: Negation and subtraction *)
    let rec negate =
        function
        | Num x       -> Num (-x)
        | Add (a, b)  -> Add (negate a, negate b)
        
    let subtract a b = Add (a, negate b)

(* Question 1.3: Multiplication *)
        
    let rec multiply a b =
        match a, b with
        | Num x, Num y      -> Num (x * y)
        | Num x, Add (a, b) -> Add (multiply (Num x) a, multiply (Num x) b) 
        | Add (a, b), c     -> Add (multiply a c, multiply b c) 
    
(* Question 1.4: Bounded existential quantifiers *)

    let pow a =
        let rec aux acc b =
            match eval b with
            | 1 -> acc
            | _ -> aux (multiply a acc) (subtract b (Num 1))
            
        aux a
    
(* Question 1.5: Bounded unique existential quantifiers *)

    let rec iterate f acc x =
        match eval x with
        | 1 -> f acc
        | b -> iterate f (f acc) (subtract x (Num 1))
        
    let pow2 a = iterate (fun acc -> multiply acc a) (Num 1)
    
(* 2: Code Comprehension *)
 
    let rec foo =
        function
        | 0            -> true
        | x when x > 0 -> bar (x - 1)
        | x            -> bar (x + 1)
        
    and bar =
        function
        | 0            -> false
        | x when x > 0 -> foo (x - 1)
        | x            -> foo (x + 1)
        
    let rec baz =
        function
        | []                 -> [], []
        | x :: xs when foo x ->
            let ys, zs = baz xs
            (x::ys, zs)
        | x :: xs ->
            let ys, zs = baz xs
            (ys, x::zs)
        

(* Question 2.1: Types, names and behaviour *)

    (* 
    
    Q: What are the types of functions foo, bar, and baz?

    A: 
        foo has type int -> bool
        bar has type int -> bool
        baz has type int list -> int list * int list 


    Q: What do the function foo, bar, and baz do.

    A: foo takes an integer x and returns true if x is even and false otherwise
        bar takes an integer x and returns true if x is odd and false otherwise
        baz takes a list of integers xs and partitions xs into its even and 
        odd components, maintaining the order, and returns the even and the odd
        partition in a tuple.
    
    Q: What would be appropriate names for functions 
       foo, bar, and baz?

    A: foo: isEven
        bar: isOdd
        baz: partitionEvenOdd
        
    *)
        

(* Question 2.2: Code snippets *)

 
    (* 
    The function baz contains the following three code snippets. 

    * A: baz xs
    * B: bar x
    * C: (ys, x::zs)

    Q: In the context of the baz function, i.e. assuming that x , xs , ys , and zs 
       all have the correct types, what are the types of snippets A, B, and C, 
       expressed using the F# syntax for types, and what are they -- 
       focus on what they do rather than how they do it.
    
    A: baz xs has the type int list * int list
       bar x has the type bool
       (ys, x :: zs) has the type int list * int list
       
       baz xs is the partition of the even and odd elements where the
       even elements are the first element of the tuple, and the odd elements
       is the second.
       
       bar x is true if x is odd and false otherwise.
       
       (ys, x :: zs) is the tuple containing the lists ys as its first element
       and the number x concatinated with zs as its second element.
       
       NOTE: The last answer may seem a bit on the how rather than the what but
       looking at the line in isolation this is really all we are able to deduce.
       Nevertheless, we also accept answers more akin to the description of `bar xs` here
       if well motivated.
    
    Q: explain the use of the and -operator that connect the foo and the bar functions.
       argue if the program would work if you replaced and with let rec .

    A: The and keyword is used to declare mutually recursive funcitons, i.e. functions that call each other.
       If you were to remove and and replace it with let rec then the code would no longer compile
       as the the call to bar inside of foo would fail as bar has not been declared yet.

    *)

(* Question 2.3: No recursion *) 

    let foo2 x = x % 2 = 0
    let bar2 x = x % 2 = 1

(* Question 2.4 *)

    (*

    Q: The function `baz` is not tail recursive. Demonstrate why.
       To make a compelling argument you should evaluate a function call of the function,
       similarly to what is done in Chapter 1.4 of HR, and reason about that evaluation. 
       You need to make clear what aspects of the evaluation tell you that the function 
       is not tail recursive. Keep in mind that all steps in an evaluation chain must 
       evaluate to the same value ( (5 + 4) * 3 --> 9 * 3 --> 27 , for instance).
       
       You do not have to step through the foo- or the bar-functions. You are allowed to evaluate 
       those function immediately.

    A: 
        baz [1;2;3;4] -->
        
        let ys, zs = baz [2;3;4]
        (ys, 1::zs) -->
        
        let ys, zs =
            let ys, zs = baz [3;4]
            (2::ys, zs)
        (ys, 1::zs) -->
        
        let ys, zs =
            let ys, zs = 
                let ys, zs = baz [4]
                (ys, 3::zs)
            (2::ys, zs)
        (ys, 1::zs) -->
        
        let ys, zs =
            let ys, zs = 
                let ys, zs = 
                    let ys, zs = baz []
                    (4::ys, zs)
                (ys, 3::zs)
            (2::ys, zs)
        (ys, 1::zs) --> 

        let ys, zs =
            let ys, zs = 
                let ys, zs = 
                    let ys, zs = [], []
                    (4::ys, zs)
                (ys, 3::zs)
            (2::ys, zs)
        (ys, 1::zs) -->
        
        let ys, zs =
            let ys, zs = 
                let ys, zs = 
                    ([4], [])
                (ys, 3::zs)
            (2::ys, zs)
        (ys, 1::zs) -->

        let ys, zs =
            let ys, zs = 
                ([4], [3])
            (2::ys, zs)
        (ys, 1::zs) -->
        
        let ys, zs =
            ([2; 4], [3])
        (ys, 1::zs) -->
        

        ([2; 4], [1; 3])

    *)

    (* DO NOT USE THIS TEXT VERBATIM IN YOUR OWN EXAM.
       In the trace above, the let-statements (let ys, zs ...) keep nesting
       until baz reaches its base case (baz [] = [], []). Only then
       can the final result be computed, and until then all intermediate
       results are stored on the stack which will cause a stack 
       overflow if the nesting gets too deep.
    *)
    
(* Question 2.5 *)

    let bazTail =
        let rec aux c =
            function
            | []                  -> c ([], [])
            | x :: xs when foo x  ->
                aux (fun (ys, zs) -> c (x :: ys, zs)) xs
            | x :: xs ->
                aux (fun (ys, zs) -> c (ys, x :: zs)) xs
                
        aux id
        

(* 3: Collatz Conjecture *)

(* Question 3.1: Collatz sequences *)
    
    let explode (str : string) = [for c in str -> c]
    let implode (lst : char list) = lst |> List.toArray |> System.String    
    let balanced (str : string) =
        let rec aux lst stack =
            match lst, stack with
            | []        , []                 -> true 
            | '(' :: xs , _                  -> aux xs (')' :: stack)
            | '[' :: xs , _                  -> aux xs (']' :: stack)
            | '{' :: xs , _                  -> aux xs ('}' :: stack)
            | x :: xs   , y :: st when x = y -> aux xs st
            | _         , _                  -> false
            
        aux (explode str) []
        
    
    let balanced2 m (str : string) =
        let rec aux stack =
            function
            | []      -> stack = []
            | x :: xs ->
                match Map.tryFind x m, stack with
                | Some y, z :: stack' when x = z ->
                  aux stack' xs || aux (y :: z :: stack') xs
                | Some y, _ -> aux (y :: stack) xs
                | None, y :: stack' when x = y -> aux stack' xs
                | _, _ -> false
                
        aux [] (explode str)
        
    
        
    let balanced3 lst = balanced2 (Map.ofList [('{', '}'); ('(', ')'); ('[', ']')]) lst 
    
    let symmetric  =
        explode >>
        List.filter System.Char.IsLetter >>
        List.map System.Char.ToLower >>
        implode >>
        balanced2 (Map.ofList [for c in 'a'..'z' -> (c, c)])
        
        
               
    open JParsec.TextParser
    
        
    let ParseBalanced, bref = createParserForwardedToRef<unit>()
    
    let parseBalancedAux =
        many (choice [pchar '{' >>. ParseBalanced .>> pchar '}'
                      pchar '[' >>. ParseBalanced .>> pchar ']'
                      pchar '(' >>. ParseBalanced .>> pchar ')']) |>> (fun _ -> ())
        
        
    let parseBalanced = parseBalancedAux .>> pstring "**END**"
    
    do bref := parseBalancedAux
            

(* 4: BASIC *)
    
    
    type var = string

    type expr =  
    | Num    of int              // Integer literal
    | Lookup of var              // Variable lookup
    | Plus   of expr * expr      // Addition
    | Minus  of expr * expr      // Subtraction
    
    type stmnt =
    | If of expr * uint32       // Conditional statement (if-then (no else)).
    | Let of var * expr        // Variable update/declaration
    | Goto of uint32           // Goto
    | End                      // Terminate program
      
    type prog = (uint32 * stmnt) list  // Programs are sequences of commands with their own line numbers 

    
    let (.+.) e1 e2 = Plus(e1, e2)  
    let (.-.) e1 e2 = Minus(e1, e2)  
    
    let fibProg xarg =  
        [(10u, Let("x",    Num xarg))                         // x = xarg
         (20u, Let("acc1", Num 1))                            // acc1 = 1
         (30u, Let("acc2", Num 0))                            // acc2 = 0
         
         (40u, If(Lookup "x", 60u))                           // if x > 0 then goto 60 (start loop)
         
         (50u, Goto 110u)                                     // Goto 110 (x = 0, terminate program)
         
         (60u, Let ("x", Lookup "x" .-. Num 1))               // x = x - 1
         (70u, Let ("result", Lookup "acc1"))                 // result = acc1
         (80u, Let ("acc1", Lookup "acc1" .+. Lookup "acc2")) // acc1 = acc1 + acc2
         (90u, Let ("acc2", Lookup "result"))                 // acc2 = result
         (100u, Goto 40u)                                     // Goto 40u (go to top of loop)
         
         (110u, End)                                          // Terminate program
                                                              // the variable result contains the
                                                              // fibonacci number of xarg
         ]

(* Question 4.1: Basic programs *)

    type basicProgram = Map<uint32, stmnt>
    
    let mkBasicProgram = Map.ofList
    let getStmnt = Map.find
    
    let nextLine l = Map.findKey (fun k _ -> k > l)
    
    let firstLine m = m |> Map.minKeyValue |> fst
    
(* Question 4.2: State *)

    type state = { currentLine : uint32
                   vars : Map<string, int> }
    
    let emptyState prog = { currentLine = firstLine prog; vars = Map.empty }
    
    let getCurrentStmnt prog st = getStmnt st.currentLine prog
    let lookup v st = Map.find v st.vars
    
    let update v x st = {st with vars = Map.add v x st.vars}
    
    let goto l st = {st with currentLine = l}

(* Question 4.3: Evaluation *)
    
    let step prog st = goto (nextLine st.currentLine prog) st
    
    let evalExpr e st =
        let rec aux =
            function
            | Num x          -> x
            | Lookup v       -> lookup v st
            | Plus (e1, e2)  -> aux e1 + aux e2
            | Minus (e1, e2) -> aux e1 - aux e2
            
        aux e
        
    let evalProg prog =
        let rec aux st = 
            match getCurrentStmnt prog st with
            | If (e, l) when evalExpr e st <> 0 -> st |> goto l |> aux
            | If (_, _)                         -> st |> step prog |> aux
            | Let (v, e)                        -> st |> update v (evalExpr e st) |> step prog |> aux
            | Goto l                            -> st |> goto l |> aux
            | End                               -> st
            
        aux (emptyState prog)
    
(* Question 4.4: State monad *)
    type StateMonad<'a> = SM of (basicProgram -> state -> 'a * state)  
      
    let ret x = SM (fun _ s -> (x, s))
    
    let bind f (SM a) : StateMonad<'b> =   
        SM (fun p s ->
            let x, s' = a p s
            let (SM g) = f x
            g p s')
          
    let (>>=) x f = bind f x  
    let (>>>=) x y = x >>= (fun _ -> y)  
      
    let evalSM p (SM f) = f p (emptyState p)

    let goto2 l = SM (fun p s -> ((), goto l s))
    
    let lookup2 x = SM (fun _ st -> (lookup x st, st))
    let update2 x v  = SM (fun _ st -> ((), update x v st))
    
    let getCurrentStmnt2 = SM (fun prog st -> (getCurrentStmnt prog st, st))
    
    let step2 = SM (fun prog st -> ((), step prog st))

(* Question 4.5: State monad evaluation *)

    type StateBuilder() =

        member this.Bind(f, x)    = bind x f
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Combine(a, b) = a >>= (fun _ -> b)

    let state = StateBuilder()

    let rec evalExpr2 =
        function
        | Num x         -> ret x
        | Lookup v      -> lookup2 v
        | Plus (e1, e2) -> evalExpr2 e1 >>= (fun x -> evalExpr2 e2 >>= (fun y -> ret (x + y)))
        | Minus (e1, e2) -> evalExpr2 e1 >>= (fun x -> evalExpr2 e2 >>= (fun y -> ret (x - y)))
    
    let rec evalProg2 =
        getCurrentStmnt2 >>=
        function
        | If (e, l)  -> evalExpr2 e >>= (fun x -> if x <> 0 then goto2 l else step2) >>>= evalProg2
        | Let (v, e) -> evalExpr2 e >>= update2 v >>>= step2 >>>= evalProg2
        | Goto l     -> goto2 l >>>= evalProg2
        | End        -> ret ()
        
        
    let rec evalExpr3 e = state {
        match e with
        | Num x -> return x
        | Lookup v -> return! lookup2 v
        | Plus(e1, e2) ->
            let! v1 = evalExpr3 e1
            let! v2 = evalExpr3 e2
            return (v1 + v2)
        | Minus(e1, e2) ->
            let! v1 = evalExpr3 e1
            let! v2 = evalExpr3 e2
            return (v1 - v2)
    }
    
    let rec evalProg3 = state {
        let! s = getCurrentStmnt2
        match s with
        | If(e, l) ->
            let! v = evalExpr3 e
            do! (if v <> 0 then goto2 l else step2)
            do! evalProg3
        | Let (v, e) ->
            let! x = evalExpr3 e
            do! update2 v x
            do! step2
            do! evalProg3
        | Goto l ->
            do! goto2 l
            do! evalProg3
        | End ->
            return ()
    }