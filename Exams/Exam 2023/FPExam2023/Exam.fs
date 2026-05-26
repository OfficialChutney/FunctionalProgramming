module Exam2023
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
 module Exam2023 = 
 *)

(* 1: Logic *)

    type prop =  
    | TT  
    | FF  
    | And of prop * prop  
    | Or of prop * prop
    
    let p1 = And(TT, FF)  
    let p2 = Or(TT, FF)  
    let p3 = And(Or(TT, And(TT, FF)), TT)  
    let p4 = And(Or(TT, And(TT, FF)), Or(FF, And(TT, FF)))
    
(* Question 1.1: Evaluation *)
    let rec eval (p : prop) = 
        match p with
        | TT -> true
        | FF -> false
        | And (a,b) -> eval a && eval b
        | Or (a,b) -> eval a || eval b
    
(* Question 1.2: Negation and implication *)
    let rec negate (p : prop) = 
        match p with
        | TT -> FF
        | FF -> TT
        | And(a,b) -> Or(negate a, negate b)
        | Or(a,b) -> And(negate a, negate b)

    let implies (p : prop) (q : prop) = Or(negate p, q)

(* Question 1.3: Bounded universal quantifiers *)
    let forall (f : ('a -> prop)) (list : 'a list) = 

        let rec aux (f : ('a -> prop)) (list : 'a list) (acc : prop) = 
            match list with
            | [] -> TT
            | x::[] -> And (acc, f x)
            | x::xs -> aux f xs (And (acc, f x))

        aux f list TT

(* Question 1.4: Bounded existential quantifiers *)

    let exists (f : ('a -> prop)) (list : 'a list) = 
        match list.IsEmpty with
        | true -> FF
        | false -> List.fold (fun (acc : prop) (elem : 'a) -> (Or (acc, f elem))) FF list
            
        
        
    
(* Question 1.5: Bounded unique existential quantifiers *)

    let existsOne (f : ('a -> prop)) (list : 'a list) = 
        let lengthList = list.Length

        let rec negateElements (i : int) (auxInt : int) (acc : prop) = 
            match auxInt >= lengthList with
            | true -> acc
            | false -> 
                match i = auxInt with
                | true -> negateElements i (auxInt+1) acc
                | false -> negateElements i (auxInt+1) (And(negate (f list.[auxInt]), acc))

        let rec aux (f : ('a -> prop)) (list : 'a list) (acc : prop) (i : int) =
            match i >= lengthList with
            | true -> acc
            | false -> aux f list (Or (negateElements i 0 (f list.[i]), acc)) (i+1)

        match list.IsEmpty with
        | true -> FF
        | false -> aux f list FF 0


    
(* 2: Code Comprehension *)
 
    let rec foo xs ys =  
        match xs, ys with  
        | _       , []                  -> Some xs   
        | x :: xs', y :: ys' when x = y -> foo xs' ys'   
        | _       , _                   -> None
          
    let rec bar xs ys =
        match foo xs ys with
        | Some zs -> bar zs ys
        | None -> match xs with
                  | [] -> []
                  | x :: xs' -> x :: (bar xs' ys)  

    

    let baz (a : string) (b : string) =  
        bar [for c in a -> c] [for c in b -> c] |>  
        List.fold (fun acc c -> acc + string c) ""

(* Question 2.1: Types, names and behaviour *)

    (* 
    
    Q: What are the types of functions foo, bar, and baz?

    A: foo is tail recursive using accumulators. Foo is not type explicit but the type is implicit.
       Bar is non tail recursive. Bar is not type explicit but the type is implicit.
       Baz is not recursive, and uses higher order functions for recursion like. Baz is type annotation explicit.


    Q: What do the function foo, bar, and baz do.
       Focus on what they do rather than how they do it.

    A: foo takes in a list "a" and a list "b", 
    and removes all elements where the head of "a" matches the head of "b".a

    bar takes in a list "a" nad a list "b" and removes any combination in a that matches in b.
    Specifically while foo only removes if the head of the two lists are exact, so foo will not remove "A string" when the matching list is "string,"
    but bar will remove "string" from "a string" and leave out "a ".

    baz takes in a string "a" as input and another string "b". Baz removes any string in "a" which matches "b" no matter placement in the string.
    
    Q: What would be appropriate names for functions 
       foo, bar, and baz?

    A: baz would be called "removeString"
       bar would be called "removeElementsFromList"
       foo would be called "removeMatchingHeadElementsFromList"
        
    *)
        

(* Question 2.2: Code snippets *)

 
    (* 
    The function baz contains the following three code snippets. 

    * A: `[for c in a -> c]`
    * B: `[for c in b -> c]`
    * C: `List.fold (fun acc c -> acc + string c) ""`

    Q: In the context of the baz function, i.e. assuming that `a` and `b` are strings, 
       what are the types of snippets A, B, and C and what are they -- 
       focus on what they do rather than how they do it.
    
    A: A and B is an array constructor, and constructs an array of characters from the characters that make up the strings.

    C is a fold function, which loops over each element in the returned char list from bar and reconstructs the string.
    
    Q: Explain the use of the `|>`-operator in the baz function.

    A: It is a pipe function, which takes the output/value from the left side and pipes it into the function on the right side.
    I.e. the output from the left side becomes the input for the right side.

    *)

(* Question 2.3: No recursion *) 

    let foo2 (a : 'a list) (b : 'a list)  = 
        
        let aux (a : 'a list) (b : 'a list) =
            List.fold2 (fun (acc : 'a list) (a : 'a) (b : 'a) -> 
                match a = b with
                | true -> acc
                | false -> a::acc) [] a b
        
        match a.Length, b.Length with
        | (aI,bI) when aI > bI -> 
            let split = List.splitAt (bI) a
            Some ((aux (fst split) b) @ (snd split))
        | (aI, bI) when aI < bI ->
            None
        | _ ->
            Some (aux a b)

    (* Question 2.4 *)

    (*

    Q: The function `bar` is not tail recursive. Demonstrate why.
       To make a compelling argument you should evaluate a function call of the function,
       similarly to what is done in Chapter 1.4 of HR, and reason about that evaluation. 
       You need to make clear what aspects of the evaluation tell you that the function 
       is not tail recursive. Keep in mind that all steps in an evaluation chain must 
       evaluate to the same value ( (5 + 4) * 3 --> 9 * 3 --> 27 , for instance).
       
       You do not have to step through the foo-function. You are allowed to evaluate 
       that function immediately.

    A:  bar ['a','b','c','d'] ['a','b','d','e'] -->
        bar ['c','d'] ['a','b','d','e'] -->
        'c'::(bar ['d'] ['a','b','d','e']) -->
        'c'::'d'::(bar [] ['a','b','d','e']) -->
        'c'::'d'::[] -->
        ['c';'d']

        As you can see on the above chain, everytime foo returns Some, no operation awaits on the stack,
        but as soon as foo does not remove anything, the first character is removed from the input,
        and the add element to head of list operation (::) awaits on the stack to be executed until all
        recursive calls have been called. Therefor it is not tail recursive.

        It is worth mentioning that there is an error in the bar function that if the list ys is input as an empty list, it becomes an infinite loop.
    *)
(* Question 2.5 *)

        (*let rec bar xs ys =
        match foo xs ys with
        | Some zs -> bar zs ys
        | None -> match xs with
                  | [] -> []
                  | x :: xs' -> x :: (bar xs' ys)  *)

    let barTail xs ys  = 

        let rec aux xs ys f =
            match foo xs ys with
            | Some zs -> bar zs ys
            | None -> match xs with
                      | [] -> f []
                      | x :: xs' -> aux xs' ys (fun a -> f (x::a))

        aux xs ys id

(* 3: Collatz Conjecture *)

(* Question 3.1: Collatz sequences *)

    let collatz (x : int) = 

        let rec aux (x : int) (list : int list) =
            match x with
            | 1 -> 1::list
            | x when x % 2 = 0 -> aux (x/2) (x::list)
            | x when x % 2 = 1 -> aux (3*x + 1) (x::list)
            | x -> failwithf "Pattern non matched on %d" x

        match x with
        | x when x <= 0 -> failwithf "Non positive number: <%d>" x
        | _ -> List.rev (aux x [])            

(* Question 3.2: Even and odd Collatz sequence elements *)

    let evenOddCollatz (x : int) = 
        List.fold (fun (acc : (int * int)) (elem : int) -> 
            match elem % 2 = 0 with
            | true -> (fst acc + 1, snd acc)
            | false -> (fst acc, snd acc + 1)) (0,0) (collatz x)

(* Question 3.3: Maximum length Collatz Sequence *)
  
    let maxCollatz (x : int) (y : int) = 
        List.fold (fun (acc : (int * int)) (elem : int) -> 
            let length = (collatz elem).Length
            match length > snd acc with
            | true -> (elem, length)
            | false -> acc) (0,0) [x..y]


(* Question 3.4: Collecting by length *)
    let collect (x : int) (y : int) = 
        List.fold (fun (acc : Map<int, Set<int>>) (elem : int) ->
            let collatzLength = (collatz elem).Length
            match acc.TryFind collatzLength with
            | Some s -> acc.Add (collatzLength, s.Add elem)
            | None -> acc.Add (collatzLength, Set.empty.Add elem)) Map.empty [x..y]
    
(* Question 3.5: Parallel maximum Collatz sequence *)

    let parallelMaxCollatz (x : int) (y : int) (n : int) = 
        let z = (y - x) / n

        Seq.init n (fun (i : int) -> 
            async { 
                let x = (z*i + x)
                let y = (z*(i+1) + x-1)

                return maxCollatz x y
            })
        |> Async.Parallel
        |> Async.RunSynchronously
        |> Array.fold (fun (acc : (int * int)) (elem : (int * int)) -> if (snd elem) > (snd acc) then elem else acc) (0,0)
        |> fst




(* 4: Memory machines *)

    type expr =  
    | Num    of int              // Integer literal
    | Lookup of expr             // Memory lookup
    | Plus   of expr * expr      // Addition
    | Minus  of expr * expr      // Subtraction
          
    type stmnt =  
    | Assign of expr * expr      // Assign value to memory location
    | While  of expr * prog      // While loop
      
    and prog = stmnt list        // Programs are sequences of statements

    let (.+.) e1 e2 = Plus(e1, e2)  
    let (.-.) e1 e2 = Minus(e1, e2)  
    let (.<-.) e1 e2 = Assign (e1, e2)
    
    // Starting from memory {0, 0, 2, 0}
    let fibProg x =  
        [Num 0 .<-. Num x       // {x, 0, 2, 0}
         Num 1 .<-. Num 1       // {x, 1, 2, 0}
         Num 2 .<-. Num 0       // {x, 1, 0, 0}
         While (Lookup (Num 0), 
                [Num 0 .<-. Lookup (Num 0) .-. Num 1  
                 Num 3 .<-. Lookup (Num 1)  
                 Num 1 .<-. Lookup (Num 1) .+. Lookup (Num 2)  
                 Num 2 .<-. Lookup (Num 3)  
                ]) // after loop {0, fib (x + 1), fib x, fib x}
         ]

(* Question 4.1: Memory blocks *)

    type mem = {
        mutable memory: int array
        size: int
    } 
    let emptyMem (size : int) = 
        let mutable mem = Array.init size (fun _ -> 0)
        { memory = mem; size = size }
    let lookup (m : mem) (i : int) = m.memory[i]


    let assign (m : mem) (i : int) (v : int) = 
        m.memory[i] <- v
        m

(* Question 4.2: Evaluation *)

    let rec evalExpr (m : mem) (e : expr) = 
        match e with
        | Num x -> x
        | Lookup e' -> lookup m (evalExpr m e')
        | Plus (e1,e2) -> evalExpr m e1 + evalExpr m e2
        | Minus (e1, e2) -> evalExpr m e1 - evalExpr m e2
    let rec evalStmnt (m : mem) (s : stmnt) = 
        match s with
        | Assign (e1, e2) -> assign m (evalExpr m e1) (evalExpr m e2)
        | While (e, p) ->
            match evalExpr m e with
            | 0 -> m
            | _ -> evalProg m (p @ [While (e,p)])
    and evalProg (m : mem) (p : prog) = 
        match p with
        | [] -> m
        | x::xs -> evalProg (evalStmnt m x) xs
    
(* Question 4.3: State monad *)
    type StateMonad<'a> = SM of (mem -> ('a * mem) option)  
      
    let ret x = SM (fun s -> Some (x, s))  
    let fail  = SM (fun _ -> None)  
    let bind f (SM a) : StateMonad<'b> =   
        SM (fun s ->   
            match a s with   
            | Some (x, s') ->  let (SM g) = f x               
                               g s'  
            | None -> None)  
          
    let (>>=) x f = bind f x  
    let (>>>=) x y = x >>= (fun _ -> y)  
      
    let evalSM m (SM f) = f m

    let lookup2 (i : int) = 
        SM (fun m ->
            match 0 <= i && i < m.size with
            | true -> Some (lookup m i, m)
            | false -> None)
    let assign2 (i : int) (v : int) = 
        SM (fun m ->
            match 0 <= i && i < m.size with
            | true ->  Some ((), assign m i v)
            | false -> None)

(* Question 4.4: State monad evaluation *)

    type StateBuilder() =

        member this.Bind(f, x)    = bind x f
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Combine(a, b) = a >>= (fun _ -> b)

    let state = StateBuilder()


    let rec evalExpr2 (e : expr) = state {
        match e with
        | Num x -> return x
        | Lookup e' -> 
            let! eval = evalExpr2 e'
            return! lookup2 eval
        | Plus (e1, e2) -> 
            let! a = evalExpr2 e1
            let! b = evalExpr2 e2
            return (a + b)
        | Minus (e1, e2) -> 
            let! a = evalExpr2 e1
            let! b = evalExpr2 e2
            return (a - b)
    }

    let rec evalStmnt2 (s : stmnt) = state {
        match s with
        | Assign (e1, e2) -> 
            let! a = evalExpr2 e1
            let! b = evalExpr2 e2
            return! assign2 a b
        | While (e,p) ->
            let! eval = evalExpr2 e
            match eval with
            | 0 -> return ()
            | _ -> return! evalProg2 (p @ [While (e,p)])
    }
    and evalProg2 (p : prog) = state {
        match p with
        | [] -> return ()
        | x::xs -> 
            do! evalStmnt2 x
            return! evalProg2 xs
    }
    
(* Question 4.5: Parsing *)
    
    open JParsec.TextParser
      
    let ParseExpr, eref = createParserForwardedToRef<expr>()  
    let ParseAtom, aref = createParserForwardedToRef<expr>()  
      
    let parsePlus = ParseAtom .>> (pchar '+') .>>. ParseAtom  |>> (fun (a,b: expr) -> Plus (a,b))
    let parseMinus = ParseAtom .>> (pchar '-') .>>. ParseAtom  |>> (fun (a,b: expr) -> Minus (a,b))

    let parseNumber = pint32 |>> Num

    let parseLookup = (pchar '[') >>. ParseExpr .>> (pchar ']') |>> Lookup 
    let parseExpr = choice [parsePlus; parseMinus] // Parse addition and minus
          
    let parseAtom = choice [parseLookup; parseNumber] // Parse numbers and lookups

//    Uncomment the following two lines once you finish parseExpr and parseAtom             
    do aref := parseAtom  
    do eref := parseExpr 