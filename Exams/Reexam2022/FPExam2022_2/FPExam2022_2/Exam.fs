module Exam2022_2
(* If you are importing this into F# interactive then comment out
   the line above and remove the comment for the line bellow.

   Do note that the project will not compile if you do this, but 
   it does allow you to work in interactive mode and you can just remove the '=' 
   to make the project compile again.

   Do not remove the module declaration (even though that does work) because you may inadvertently
   introduce indentation errors in your code that may be hard to find if you want
   to switch back to project mode. 

   Alternative, keep the module declaration as is, but load ExamInteractive.fsx into the interactive environment
   *)
(*
 module Exam2022_2 = 
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
    let maxDepth (img : grayscale) = 
        
        let rec aux (img : grayscale) (depth : int) =
            match img with
            | Square x -> depth
            | Quad (a,b,c,d) -> 
                let depths = [(aux a depth+1); (aux b depth+1); (aux c depth+1); (aux d depth+1)]
                List.fold (fun (acc : int) (elem : int) -> Int32.Max (elem, acc)) 0 depths

        aux img 0
    
(* Question 1.2 *)
    let rec mirror (img : grayscale) =
        match img with
        | Square (x: uint8) -> Square x
        | Quad (a,b,c,d) ->
            Quad (mirror b, mirror a, mirror d, mirror c)

(* Question 1.3 *)
    let rec operate (f : (grayscale -> grayscale -> grayscale -> grayscale -> grayscale)) (img : grayscale) = 
        match img with
        | Square v -> Square v
        | Quad (a,b,c,d) ->
            f (operate f a) (operate f b) (operate f c) (operate f d)


    let mirror2 (img : grayscale) = operate (fun a b c d -> Quad (b, a, d, c)) img

(* Question 1.4 *)

    let rec compress (img : grayscale) = 
        match img with
        | Square v -> Square v
        | Quad (a,b,c,d) -> 
            let compressed = [compress a; compress b; compress c; compress d]

            let allEqual = List.fold (fun (acc : bool) (elem : grayscale) -> 
                match elem, compressed.[0] with
                | (Square v, Square b) -> if v = b then true else false
                | (_,_) -> false ) false compressed

            if allEqual then compressed.[0] else Quad (a,b,c,d)
            

(* 2: Code Comprehension *)
    let rec foo f =
        function
        | []               -> []
        | x :: xs when f x -> x :: (foo f xs)
        | _ :: xs          -> foo f xs
            
    let rec bar fs xs =
        match fs with
        | []       -> xs
        | f :: fs' -> bar fs' (foo f xs)

(* Question 2.1 *)

    (* 
    
    Q: What are the types of functions foo and bar?

    A: foo is non tail recursive while bar is tail recursive using an accumulator xs 
    foo is a type inferred function, while bar is not. It has explicit type annotation.


    Q: What do the functions foo and  bar do. 
       Focus on what it does rather than how it does it.

    A: foo filters the list, and returns a list only with elements which are true for the inserted predicate (function f).
    bar takes a list of predicates, and returns a list only with the elements that are true for all the predicates in the list of predicates.


    Q: What would be appropriate names for functions 
       foo and bar?

    A: foo = filterList
        bar = filerListOnPredicates
        
    Q: The function foo uses an underscore `_` in its third case. 
       Is this good coding practice, if so why, and if not why not?
    
    A: I would argue it is good practice. The value of the head is not used.
        Inserting a value x instead of the underscore, the IDE even warns the value is unused.
        Having an underscore clearly tells anyone, that it is intended that the value is discarded.
    *)
        

(* Question 2.2 *)

    let bar2 (fs : ('a -> bool) list) (list : 'a list) = 
        List.fold (fun (acc : 'a list) (elem : ('a -> bool)) -> List.filter elem acc) list fs

(* Question 2.3 *) 

    let baz (fs : ('a -> bool) list) (elem : 'a) =
        List.fold (fun (acc : bool) (fs : ('a -> bool)) -> if acc = false then false else fs elem) true fs 
(* Question 2.4 *)

    (*

    Q: Only one of the functions `foo` and `bar` is tail recursive. Which one? 
       Demonstrate why the other one is not tail recursive.
       To make a compelling argument you should evaluate a function call of the function,
       similarly to what is done in Chapter 1.4 of HR, and reason about that evaluation.
       You need to make clear what aspects of the evaluation tell you that the function is not tail recursive.
       Keep in mind that all steps in an evaluation chain must evaluate to the same value
       ((5 + 4) * 3 --> 9 * 3 --> 27, for instance).

    A: foo is tail recursive, while bar is not. This can clearly be seen on an evaluation chain:

    Foo:
    foo f : (fun a -> a > 4) [2;3;5;17;19] -->
    (foo f [3;5;17;19]) -->
    (foo f [5;17;19]) -->
    5::(foo f [17;19]) -->
    5::17::(foo f [19]) -->
    5::17::19::(foo f []) -->
    5::17::19::[] -->
    [5;17;19]

    Bar:
    bar [fun a -> a > 5; fun a -> a < 20] [4;10;24] -->
    bar [fun a -> a < 20] [10;24] -->
    bar [] [10] -->
    [10]

    As you can see on the chain above, the foo has awaiting operations 
    for adding a element to the head of a list (::) until all recursive calls has been made,
    and therefor has operations waiting on the stack.

    For bar, it can be seen that for each recursive call, all operations are evaluated for each recursive call.
    Can be clearly seen as both the list of predicates and the list of numbers gets
    evaluated on each call.
    *)
(* Question 2.5 *)
    // only implement the one that is NOT already tail recursive
    let fooTail (predicate : ('a -> bool)) (list : 'a list) =

        let rec aux (predicate : ('a -> bool)) (list : 'a list) (f) =
            match list with
            | [] -> f []
            | x::xs -> aux predicate xs (fun a -> f (if predicate x then x::a else a))

        aux predicate list id

    let barTail (predicates : ('a -> bool) list) (list : 'a list) =
        
        let rec aux (predicates : ('a -> bool) list) (list : 'a list) (f) =       
            match predicates with
            | [] -> f list
            | x::xs -> aux xs list (fun a -> f (fooTail x a))

        aux predicates list id

(* 3: Guess a number *)

    type guessResult = Lower | Higher | Equal
    type oracle =
        { max : int
          f : int -> guessResult }

(* Question 3.1 *)

    let validOracle (oracle : oracle) = 
        let numbersToTest = [1..oracle.max]
        
        let equal =
            List.fold (fun (acc : int list) (elem : int) ->
                match oracle.f elem with
                | Equal -> elem::acc
                | _ -> acc) [] numbersToTest 
                |> fun a -> 
                    if a.Length = 1 && a.Head <= oracle.max && a.Head >= 1 then a.Head, true else -1,false

        let restIsCorrect = 
            match snd equal with
            | false -> false
            | true -> 
                List.fold (fun (acc : bool) (elem : int) -> 
                    match acc with
                    | false -> false
                    | true -> 
                        match oracle.f elem with
                        | Higher -> 
                            if elem >= 1 && elem < fst equal then 
                                true 
                            else 
                                false
                        | Lower -> 
                            if elem > fst equal && elem <= oracle.max then 
                                true 
                            else 
                                false
                        | Equal -> 
                            if elem = fst equal then 
                                true 
                            else 
                                false
                    ) true numbersToTest

        snd equal && restIsCorrect


(* THIS IS A LOT SIMPLER SOLUTION U FUCKING DONUT.
let validOracle (oracle : oracle) : bool =
    let numbersToTest = [1 .. oracle.max]

    let equalNumbers =
        numbersToTest
        |> List.filter (fun n -> oracle.f n = Equal)

    match equalNumbers with
    | [x] ->
        numbersToTest
        |> List.forall (fun n ->
            match oracle.f n with
            | Higher -> n < x
            | Equal  -> n = x
            | Lower  -> n > x)

    | _ ->
        false
*)

(* Question 3.2 *)

    let randomOracle (m : int) (oseed : int option) = 
        let r= 
            match oseed with
            | Some seed -> (System.Random(seed)).Next(1, m + 1)
            | None -> (System.Random()).Next(1, m + 1)

        let getOracleFunc (x : int) =
            match x with
            | x when x = r -> Equal
            | x when x >= 1 && x < r -> Higher
            | _ -> Lower

        {max = m; f = getOracleFunc}

(* Question 3.3 *)
    
    let findNumber (o : oracle) = 

        let rec aux (a : int) (b : int) (list : int list) =
            let g = ((a + ((b - a) / 2)))
            
            match o.f g with
            | Equal -> g::list
            | Lower -> aux a (g-1) (g::list)
            | Higher -> aux (g+1) b (g::list)

        List.rev (aux 1 o.max [])

(* Question 3.4 *)
    let evilOracle (m : int) (oseed : int option) =
        let random =
            match oseed with
            | Some seed -> System.Random(seed)
            | None -> System.Random()

        let getRandomHigherOrLower () = 
            match random.Next(0,2) with
            | 0 -> Higher
            | _ -> Lower
        

        let mutable range= [1..m]

        let updateRange (x : int) (result : guessResult) =
            match result with
            | Higher -> range <- [(x+1)..range[range.Length-1]]
            | Lower -> range <- [range[0]..(x-1)]

        let getOracleFunc (x : int) =
            match range.Length = 1 with
            | true -> Equal
            | false ->
                match x with
                | x when x < range[0] -> Lower
                | x when x > range[range.Length-1] -> Higher
                | _ ->
                    let split = List.splitAt (x-range[0]) range
                    match (fst split).Length, (snd split).Length-1 with
                    | (f,s) when f = s -> 
                        let rand = getRandomHigherOrLower()
                        updateRange x rand
                        rand
                    | (f,s) when f > s ->
                        updateRange x Lower
                        Lower
                    | _ -> 
                        updateRange x Higher
                        Higher
        {max = m; f = getOracleFunc}
    
(* Question 3.5 *)
    let parFindNumber (os : oracle list) = 
        List.map (fun (o : oracle) -> async { return findNumber o } ) os
        |> Async.Parallel 
        |> Async.RunSynchronously
        |> Array.toList



(* 4: Assembly *)

    type register = R1 | R2 | R3
    type address = uint

    type assembly =
    | MOVI of register * int
    | MULT of register * register * register
    | SUB of register * register * register
    | JGTZ of register * address
    
     
    let factorial x =           // Address
        [MOVI (R1, 1)           // 0
         MOVI (R2, x)           // 1
         MOVI (R3, 1)           // 2
         MULT (R1, R1, R2)      // 3 (Loop starts here)
         SUB  (R2, R2, R3)      // 4
         JGTZ (R2, 3u)]         // 5 (Loop ends here)
    
(* Question 4.1 *)

    type program = Map<uint, assembly>
    let assemblyToProgram (list : assembly list) =

        let rec aux (list : assembly list) (i : uint) (program : program) = 
            match list with
            | [] -> program
            | x::xs -> aux xs (i+1u) (program.Add (i,x))

        aux list 0u Map.empty
            


(* Question 4.2 *)

    type state = {
        register: Map<register, int>
        counter: uint32
        source: program
    }
    let emptyState (list : assembly list) = 
        let register = 
            Map.empty.Add(R1, 0).Add(R2,0).Add(R3,0)

        {register = register; counter = 0u; source = assemblyToProgram list}
        
    

(* Question 4.3 *)

    let setRegister (r : register) (v : int) (st : state) = 
        {register = st.register.Add(r,v); counter = st.counter; source = st.source}
            
    let getRegister (r : register) (st : state) = 
        match st.register.TryFind r with
        | Some v -> v
        | None -> 0
        
    
    let setProgramCounter (addr : uint) (st : state) = 
        {register = st.register; counter = addr; source = st.source}
    
    let getProgramCounter (st : state) = st.counter
    
    let getProgram (st : state) = st.source
    
(* Question 4.4 *)
    
    type StateMonad<'a> = SM of (state -> 'a * state)

    let ret x = SM (fun s -> x, s)
    let bind f (SM a) : StateMonad<'b> = 
      SM (fun s -> 
      let x, s' = a s
      let (SM g) = f x
      g s')

    let (>>=) x f = bind f x
    let (>>>=) x y = x >>= (fun _ -> y)

    let evalSM prog (SM f) = f (emptyState prog)

    let setReg (r : register) (v  : int) = 
        SM (fun st -> ((),setRegister r v st))
    
    let getReg (r : register) = 
        SM (fun st -> (getRegister r st, st)) 
    
    let setPC (addr : uint) = 
        SM (fun st -> ((), setProgramCounter addr st))
    
    let incPC = SM (fun st -> ((), setProgramCounter (st.counter+1u) st))
    
    let lookupCmd = 
        SM (fun st -> 
            match st.source.TryFind st.counter with
            | Some v -> (Some v, st)
            | None -> (None, st))
    


(* Question 4.5 *)

    type StateBuilder() =

        member this.Bind(f, x)    = bind x f
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Combine(a, b) = a >>= (fun _ -> b)

    let state = new StateBuilder()

    let rec runProgram () = state {

        let! lookUp = lookupCmd

        match lookUp with
        | None -> return ()
        | Some cmd ->
            do! incPC
            match cmd with
            | MOVI (r,v) -> 
                do! setReg r v
                return! runProgram ()
            | MULT (r1,r2,r3) ->
                let! num1 = getReg r2
                let! num2 = getReg r3
                do! setReg r1 (num1 * num2)
                return! runProgram ()
            | SUB (r1, r2, r3) ->
                let! num1 = getReg r2
                let! num2 = getReg r3

                do! setReg r1 (num1 - num2)
                return! runProgram ()
            | JGTZ (r,a) -> 
                let! v = getReg r
                match v > 0 with
                | true -> 
                    do! setPC a
                    return! runProgram ()
                | false -> return! runProgram ()
    }
    