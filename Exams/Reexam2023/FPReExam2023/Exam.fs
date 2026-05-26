module ReExam2023

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
    let rec eval (a : arith) = 
        match a with
        | Num i-> i
        | Add (num1, num2) -> eval num1 + eval num2
    
(* Question 1.2: Negation and subtraction *)
    let rec negate (a : arith) = 
        match a with
        | Num a -> Num -a
        | Add (a1, a2) -> Add (negate a1, negate a2)
    let rec subtract (a : arith) (b : arith) = 
        match a,b with
        | Num a, Num b -> Add (Num a, Num -b)
        | Add (a1, a2), Add (b1,b2) -> Add (Add (a1, a2), subtract b1 b2)
        | Add (a1, a2), Num b -> Add (Add (a1, a2), Num -b)
        | Num a, Add(b1, b2) -> Add(Num a, subtract b1 b2)

(* Question 1.3: Multiplication *)
        
    let rec multiply (a : arith) (b : arith) = 
        match a,b with
        | Num a, Num b -> Num (a*b)
        | Num a, Add(b1, b2) -> Add (multiply (Num a) b1, multiply (Num a) b2)
        | Add (a1, a2), b -> Add (multiply a1 b, multiply a2 b)
    
(* Question 1.4: Exponents *)

    let pow (a : arith) (b : arith) = 

        let rec aux (a : arith) (b : arith) (acc : arith) =
            match eval b with
            | 1 -> multiply a acc
            | _ -> aux a (subtract b (Num 1)) (multiply a acc)

        aux a b (Num 1)

    


(* Question 1.5: Iteration *)

    let rec iterate (f : ('a -> 'a)) (acc : 'a) (a : arith) = 
        match eval a with
        | 0 -> acc
        | _ -> iterate f (f acc) (subtract a (Num 1))
        
    let pow2 (a : arith) (b : arith) = iterate (fun x -> multiply a x) (Num 1) b
    
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

    A: foo and bar are mutually recursive. Foo and bar are tail recursive, using an int as an accumulator.

    Baz is type inferred and is also non tail recursive.
    Baz is not mutually recursive.


    Q: What do the function foo, bar, and baz do.
       Focus on what they do rather than how they do it.

    A: 
    foo returns true if it is an even number.
    bar returns true if it is an odd number.

    Baz takes in a list of integers and returns a tuple with two list. The first element is a list of even numbers.
    The second is a list of odd numbers.
    
    Q: What would be appropriate names for functions 
       foo, bar, and baz?
    
    A: They could be called, taken into account the name is what is called FIRST:
    foo = isEven
    bar = isOdd
    baz = SplitEvenOdd
        
    *)
        

(* Question 2.2: Code snippets *)

 
    (* 
    The function baz contains the following three code snippets. 

    * A: `baz xs`
    * B: `bar x`
    * C: `(ys, x::zs)`

    Q: In the context of the baz function, i.e. assuming that `x`, `xs`, `ys`, and `zs` all have the correct types,
       what are the types of snippets A, B, and C, expressed using the F# syntax for types, and what are they -- 
       focus on what they do rather than how they do it.
    
    A: 
        A is: list<int> * list<int> - Baz returns a tuple of list of integers.
            When calling baz, it recurses down until an empty list, 
            and splits each number of the input list into a tuple of either odd or even numbers.
        B is: bool - Bar returns a bool. Bar returns true if the input number is odd, and false if it is even.
        C is: has type list<int> * list<int>. 
            Here we create a tuple with ys in the fst, and in the snd we add the integer x to the head of list zs.
            We end up with a tuple of two lists of integers.

    
    Q: * Explain the use of the `and`-operator that connect the `foo` and the `bar` functions.
       * Argue if the program would work if you replaced `and` with `let rec`.

    A: and makes the two mutually recursive.
        This means they can both be used, and are compiled on the same "level"
        so they both can call eachother. Replacing 'and' with 'let rec' would not work,
        as foo would not be able to call bar, as bar is defined later down, so bar would be undefined.

    *)

(* Question 2.3: No recursion *) 

    let foo2 (a : int) = a % 2 = 0
    let bar2 (a : int) = a % 2 = 1

(* Question 2.4: Tail Recursion *)

    (*

    Q: The function `baz` is not tail recursive. Demonstrate why.
       To make a compelling argument you should evaluate a function call of the function,
       similarly to what is done in Chapter 1.4 of HR, and reason about that evaluation. 
       You need to make clear what aspects of the evaluation tell you that the function 
       is not tail recursive. Keep in mind that all steps in an evaluation chain must 
       evaluate to the same value ( (5 + 4) * 3 --> 9 * 3 --> 27 , for instance).
       
       You do not have to step through the foo- or the bar-functions. You are allowed to evaluate 
       those function immediately.
        let rec baz =
        function
        | []                 -> [], []
        | x :: xs when foo x ->
            let ys, zs = baz xs
            (x::ys, zs)
        | x :: xs ->
            let ys, zs = baz xs
            (ys, x::zs)

    A: 
        baz [1;2;5;7;9] -->
        let ys,zs = baz [2;5;7;9]
        (ys, x::zs) -->
        let ys,zs = baz [5;7;9]
        (x::ys, zs) -->
        let ys,zs = baz [7;9]
        (ys, x::zs) -->
        let ys,zs = baz [9]
        (ys, x::zs) -->
        let ys,zs = baz []
        (ys, x::zs) -->
        ([], []) -->
        ([], 9::[]) -->
        ([], 7::9::[]) -->
        ([], 5::7::9::[]) -->
        (2::[], 5::7::9::[]) -->
        (2::[], 1::5::7::9::[]) -->
        ([2], [1;5;7;9])

        I have tried to show the call chgain, which is extremely difficult since it is tupled
        stored in variables before returned, but i have tried to show it where you can see,
        that each recursive call to baz has an awaiting (::) operation, and only when all recursive calls has been made,
        can the list be made, with every "add to head" operation executing on each awaiting stack.

        
    
    *)
(* Question 2.5: Continuations *)

(*    let rec baz =
        function
        | []                 -> [], []
        | x :: xs when foo x ->
            let ys, zs = baz xs
            (x::ys, zs)
        | x :: xs ->
            let ys, zs = baz xs
            (ys, x::zs)*)

    let bazTail (list : int list) = 

        let rec aux (list : int list) (f) =
            match list with
            | [] -> f ([],[])
            | x :: xs when foo x -> aux xs (fun a -> f ( x::(fst a), snd a ))
            | x :: xs -> aux xs (fun a -> f ( fst a, x::(snd a) ))

        aux list id

(* 3: Balanced brackets *)

    let explode (str : string) = [for c in str -> c]
    let implode (lst : char list) = lst |> List.toArray |> System.String
    
(* Question 3.1: Balanced brackets *)
    
    let balanced (str : string) = 

        let pop (stack : char list) =
            match stack with
            | x::xs -> Some (x,xs)
            | [] -> None

        let push (c : char) (stack : char list) = c::stack

        let rec aux (chars : char list) (stack : char list) =
            match chars with
            | x::xs when x = '(' -> aux xs (push ')' stack)
            | x::xs when x = '{' -> aux xs (push '}' stack)
            | x::xs when x = '[' -> aux xs (push ']' stack)
            | x::xs when x = ')' || x = '}' || x = ']' -> 
                match pop stack with
                | Some (top, stack) -> 
                    match x = top with
                    | false -> false
                    | true -> aux xs stack
                | None -> false
            | _ -> chars.IsEmpty && stack.IsEmpty

        aux (explode str) []
            
         
(* Question 3.2: Arbitrary delimiters *)
    
    let balanced2 (m : Map<char, char>) (str : string) = 

        let peepDoesMatch (top : char) (stack : char list) =
            match stack with
            | x::xs -> top = x    
            | [] -> false

        let pop (stack : char list) =
            match stack with
            | x::xs -> Some (xs)
            | [] -> None

        let push (c : char) (stack : char list) = c::stack

        let rec aux (chars : char list) (stack : char list) =
            match chars with
            | x::xs -> 
                let bool = 
                    match m.TryFind x with
                    | Some v -> aux xs (push v stack)
                    | None -> false
                match bool with
                | true -> true
                | false -> 
                    match pop stack with
                        | Some stack -> aux xs stack
                        | None -> false
            | _ -> chars.IsEmpty && stack.IsEmpty
        
        aux (explode str) []
        
(* Question 3.3: Matching brackets and palindromes *)    
    
    (*let balanced3 (m : Map<char, char>) (str : string) = 
        let peepDoesMatch (top : char) (stack : char list) =
            match stack with
            | x::xs -> top = x    
            | [] -> false

        let pop (stack : char list) =
            match stack with
            | x::xs -> Some (xs)
            | [] -> None

        let push (c : char) (stack : char list) = c::stack*)
        


        

    
    let symmetric _ = failwith "not implemented"
        
(* Question 3.4: Parsing balanced brackets *)    
               
    open JParsec.TextParser
    
        
    let ParseBalanced, bref = createParserForwardedToRef<unit>()
    
    let parseBalancedAux = pstring "Your parser goes here"
        
    // uncomment after you have done parseBalancedAUX
    
    // let parseBalanced = parseBalancedAux .>> pstring "**END**"
    // do bref := parseBalancedAux
            
(* Question 3.5: Parallel counting *)

    let countBalanced _ = failwith "not implemented"

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
    
    let mkBasicProgram _ = failwith "not implemented"
    let getStmnt _ = failwith "not implemented"
    
    let nextLine _ = failwith "not implemented"
    
    let firstLine _ = failwith "not implemented"
    
(* Question 4.2: State *)

    type state = unit // Replace by your type type goes here
    
    let emptyState _ = failwith "not implemented"
    
    
    let goto _ = failwith "not implemented"

    let getCurrentStmnt _ = failwith "not implemented"
    
    let update _ = failwith "not implemented"
    
    let lookup _ = failwith "not implemented"
    
    
(* Question 4.3: Evaluation *)
    
    let evalExpr _ = failwith "not implemented"
    
    
    let step _ = failwith "not implemented"
  
        
    let evalProg _ = failwith "not implemented"
    
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

    let goto2 _ = failwith "not implemented"
    
    let getCurrentStmnt2 _ = failwith "not implemented"
    
    
    let lookup2 _ = failwith "not implemented"
    let update2 _ = failwith "not implemented"
    
    let step2 _ = failwith "not implemented"

(* Question 4.5: State monad evaluation *)

    type StateBuilder() =

        member this.Bind(f, x)    = bind x f
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Combine(a, b) = a >>= (fun _ -> b)

    let state = StateBuilder()

    let evalExpr2 _ = failwith "not implemented"
    
    let evalProg2 _ = failwith "not implemented"
        