module Exam2022
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
 module Exam2022 = 
 *)
open System

(* 1: Grayscale images *)

    type grayscale =
    | Square of uint8
    | Quad of grayscale * grayscale * grayscale * grayscale
    
    let img = 
      Quad (Square 255uy, 
            Square 128uy, 
            Quad(Square 255uy, 
                 Square 128uy, 
                 Square 192uy,
                 Square 64uy),
            Square 0uy)
    
(* Question 1.1 *)
    let rec countWhite (img : grayscale) =
        match img with
        | Square x ->
            match x with
            | 255uy -> 1
            | _ -> 0
        | Quad (a,b,c,d) ->
             countWhite a + countWhite b + countWhite c + countWhite d
    
(* Question 1.2 *)
    let rec rotateRight (img : grayscale) =
        match img with
        | Square x -> Square x
        | Quad (a,b,c,d) ->
            Quad (rotateRight d, rotateRight a, rotateRight b, rotateRight c)

(* Question 1.3 *)
    let rec map (f : (uint8 -> grayscale)) (img : grayscale) = 
        match img with
        | Square x-> f x
        | Quad (a,b,c,d) ->
            Quad (map f a, map f b, map f c, map f d)
    
    let bitmap (img : grayscale) = map (fun (i : uint8) -> if i <= 127uy then Square 0uy else Square 255uy) img

(* Question 1.4 *)

    let rec fold (folder : ('a -> uint8 -> 'a)) (acc : 'a) (img : grayscale) = 
        match img with
        | Square x -> folder acc x
        | Quad (a,b,c,d) ->
            acc
            |> fun acc -> fold folder acc a
            |> fun acc -> fold folder acc b
            |> fun acc -> fold folder acc c
            |> fun acc -> fold folder acc d


    let countWhite2 (img : grayscale) = fold (fun (acc : int) (color : uint8) -> if color = 255uy then acc+1 else acc) 0 img

    (* 2: Code Comprehension *)
    let rec foo =
        function
        | 0 -> ""
        | x when x % 2 = 0 -> foo (x / 2) + "0"
        | x when x % 2 = 1 -> foo (x / 2) + "1"

    let rec bar =
        function
        | []      -> []
        | x :: xs -> (foo x) :: (bar xs)
    
(* Question 2.1 *)

    (* 
    
    Q: What are the types of functions foo and bar?

    A: They are both non tail recursive, and they are both type inferred.


    Q: What does the function bar do.
       Focus on what it does rather than how it does it.

    A: Bar iterates through a list and converts each number in the list to binary using the foo function on each number, until it is empty.
    
    Q: What would be appropriate names for functions 
       foo and bar?

    A: bar = integerListToBinary
       foor = intToBinary
        
    Q: The function foo does not return reasonable results for all possible inputs.
       What requirements must we have on the input to foo in order to get reasonable results?
    
    A: The input must be positive, Any negative number returns a MatchFailureException, as the pattern match does not take negative numbers into account.
    *)
        

(* Question 2.2 *)

 
    (* 
    The function foo compiles with a warning. 

    
    Q: What warning and why?

    The warning is:

    A: warning FS0025: Incomplete pattern matches on this expression. For example, the value '1' may indicate a case not covered by the pattern(s). However, a pattern rule with a 'when' clause might successfully match this value.

        Above is the warning. It is produced as any all possible inputs are not accounted for in the pattern match, and it is therefor incomplete.

    *)

    let foo2 (num : int) = 

        let rec foo =
            function
            | 0 -> ""
            | x when x % 2 = 0 -> foo (x / 2) + "0"
            | x when x % 2 = 1 -> foo (x / 2) + "1"

        let getSignBit (num : int) = 
            match num < 0 with
            | true -> "1"
            | false -> "0"

        getSignBit num + foo (Math.Abs num)

(* Question 2.3 *) 

    let bar2 (list : int list) = List.map (fun (elem : int) -> foo2 elem) list
(* Question 2.4 *)

    (*

    Q: Neither foo nor bar is tail recursive. Pick one (not both) of them and explain why.
       To make a compelling argument you should evaluate a function call of the function,
       similarly to what is done in Chapter 1.4 of HR, and reason about that evaluation.
       You need to make clear what aspects of the evaluation tell you that the function is not tail recursive.
       Keep in mind that all steps in an evaluation chain must evaluate to the same value
       ((5 + 4) * 3 --> 9 * 3 --> 27, for instance).

    A: I will use foo as an example.
        We can see it is not tail recursive, since the expression can only be evaluated, when all computations
        of the recursive calls has been made. Below evaluation chain shows this clearly, using 5 as an input:
        foo 5 -->
        foo 5/2 + "1" -->
        foo 2/2 + "0" + "1" -->
        foo 1/2 + "1" + "0" + "1" -->
        "" + "1" + "0" + "1" -->
        "101"

        As you can see on the evaluation chain above, every step awaits on the stack until all recursive calls,
        and their respective computations have been made, and thereafter it can compute the answer.
    
    Q: Even though neither `foo` nor `bar` is tail recursive only one of them runs the risk of overflowing the stack.
       Which one and why does  the other one not risk overflowing the stack?

    A: bar is the one that risks stack overflow, as it takes in a list of int, so the maximum list length for an int32
        could be 2147483647, which it has to apply the :: operation on each element, which would stay on the stack.
        So bar could have a stack of 2147483647 operations waiting.
        foo is non tail recursive, but it divides the number by 2 each time, so even if you input the maximum int32 value being
        2147483647, the maximum number of recursive calls on the stack is 31, which realisticly will not
        cause a stack overflow.

    *)
(* Question 2.5 *)

    let fooTail (x : int) = 
        
        let rec fooTailAux (x : int) (acc : string) = 
            match x with
            | x when x <= 0 -> acc
            | x when x % 2 = 0 -> fooTailAux (x / 2) (acc + "0")
            | x when x % 2 = 1 -> fooTailAux (x / 2) (acc + "1")

        fooTailAux x ""

(* Question 2.6 *)
    let barTail (list : int list) = 

        let rec barTailAux (list : int list) (f) =
            match list with
            | [] -> f []
            | x :: xs -> barTailAux xs (fun a -> f (fooTail x :: a))

        barTailAux list id

        
(* 3: Matrix operations *)

    type matrix = int[,]

    let init f rows cols = Array2D.init rows cols f

    let numRows (m : matrix) = Array2D.length1 m
    let numCols (m : matrix) = Array2D.length2 m

    let get (m : matrix) row col = m.[row, col]
    let set (m : matrix) row col v = m.[row, col] <- v

    let print (m : matrix) =
        for row in 0..numRows m - 1 do
            for col in 0..numCols m - 1 do
                printf "%d\t" (get m row col)
            printfn ""

(* Question 3.1 *)

    let failDimensions (m1 : matrix) (m2 : matrix) = 
        failwithf "Invalid matrix dimensions: m1 rows = %d, m1 columns = %d, m2 roms = %d, m2 columns = %d" (numRows m1) (numCols m1) (numRows m2) (numRows m2)

(* Question 3.2 *)

    let add (m1 : matrix) (m2 : matrix) : matrix = 
        
        let rec addAux (m1 : matrix) (m2 : matrix) = 
            init (fun (i : int) (j : int) -> get m1 i j + get m2 i j) (numRows m1) (numCols m1)
            
        
        match numRows m1 = numRows m2 && numCols m1 = numCols m2 with
        | false -> failDimensions m1 m2
        | true -> addAux m1 m2


(* Question 3.3 *)
    
    let m1 = (init (fun i j -> i * 3 + j + 1) 2 3) 
    let m2 = (init (fun j k -> j * 2 + k + 1) 3 2)

    let dotProduct (a : matrix) (b : matrix) (aRow : int) (bCol : int) =
        let rec dotProductAux (product : int) (num : int) (aux : int) =
            match aux > num with
            | true -> product
            | false -> dotProductAux (product + get a aRow aux * get b aux bCol) num (aux+1)

        dotProductAux 0 (numRows a) 0
    let mult (a : matrix) (b : matrix) = init (fun i j -> dotProduct a b i j) (numRows a) (numCols b)

(* Question 3.4 *)
    let parInit f i j : matrix   = 
        let m = init (fun _ _ -> 0) i j
        
        let apply (num : int) =
            let row = num % i
            let col = num / i

            set m row col (f row col)

        seq[0..(i*j-1)]
        |> Seq.map (fun num -> async { apply num })
        |> Async.Parallel
        |> Async.Ignore
        |> Async.RunSynchronously

        m


(* 4: Stack machines *)

    

    type cmd = Push of int | Add | Mult
    type stackProgram = cmd list

(* Question 4.1 *)

    type stack = List<int>
    let emptyStack () : stack = List.empty

(* Question 4.2 *)

    let runStackProg (prog : stackProgram) =        

        let pop (stack : stack) =
            match stack with
            | [] -> failwith "empty stack"
            | x::xs -> (x, xs)

        let rec runStackProgramAux (prog : stackProgram) (stack : stack) =
            match prog with
            | [] -> fst (pop stack)
            | x::xs -> 
                match x with
                | Push num -> runStackProgramAux xs (num::stack)
                | Add -> 
                    let firstPop = pop stack
                    let secondPop = pop (snd firstPop)
                    runStackProgramAux xs (fst firstPop + fst secondPop::snd secondPop)
                | Mult -> 
                    let firstPop = pop stack
                    let secondPop = pop (snd firstPop)
                    runStackProgramAux xs (fst firstPop * fst secondPop::snd secondPop)

        runStackProgramAux prog (emptyStack ())
        

(* Question 4.3 *)
    
    type StateMonad<'a> = SM of (stack -> ('a * stack) option)

    let ret x = SM (fun s -> Some (x, s))
    let fail  = SM (fun _ -> None)
    let bind f (SM a) : StateMonad<'b> = 
        SM (fun s -> 
            match a s with 
            | Some (x, s') -> 
                let (SM g) = f x             
                g s'
            | None -> None)
        
    let (>>=) x f = bind f x
    let (>>>=) x y = x >>= (fun _ -> y)

    let evalSM (SM f) = f (emptyStack ())

    let push (inp : int) = 
        SM (fun s -> Some ((), inp::s))

    let pop = 
        SM (fun s -> 
            match s.IsEmpty with
            | true -> 
                let (SM f) = fail
                f s
            | false -> Some (s.Head, s.Tail))


(* Question 4.4 *)

    type StateBuilder() =

        member this.Bind(x, f)    = bind f x
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Combine(a, b) = a >>= (fun _ -> b)

    let state = new StateBuilder()

    let runStackProg2 (prog : stackProgram) = 
        
        let rec runStackProgAux prog =
            state {
                match prog with
                | [] ->
                    return! pop

                | Push num :: xs ->
                    do! push num
                    return! runStackProgAux xs

                | Add :: xs ->
                    let! x = pop
                    let! y = pop
                    do! push (x + y)
                    return! runStackProgAux xs

                | Mult :: xs ->
                    let! x = pop
                    let! y = pop
                    do! push (x * y)
                    return! runStackProgAux xs
            }

        runStackProgAux prog

    
(* Question 4.5 *)
    
    open JParsec.TextParser

    let spaces = many (pchar ' ')
    let spaces1 = many1 (pchar ' ')

    let parsePush =
        pstring "PUSH" >>. spaces1 >>. pint32
        |>> Push

    let parseAdd =
        pstring "ADD"
        |>> fun _ -> Add

    let parseMult =
        pstring "MULT"
        |>> fun _ -> Mult

    let parseCmd =
        choice [ parsePush; parseAdd; parseMult ]

    let parseNewline =
        spaces >>. pchar '\n' .>> spaces

    let parseStackProgram =
        sepBy1 parseCmd parseNewline .>> many parseNewline

    let parseStackProg (input : string) =
        run parseStackProgram input
