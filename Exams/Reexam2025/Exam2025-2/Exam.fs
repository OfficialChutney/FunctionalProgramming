module Exam2025_Template.Exam

    open JParsec.TextParser
    open System
    (* Question 1: Triangle numbers *)
    
    (* Question 1.1 *)
    
    let triangleNumber (n : int)  =
        let rec aux (i : int) (lastCompute : int) = 
            match i > n with
            | true -> []
            | false -> lastCompute+i::aux (i+1) (lastCompute+i)

        aux 1 0

    (* Question 1.2 *)
 
    let triangleNumberAcc (n : int) = 

        let rec aux (i : int) (acc : int list) =
            match i > n with
            | true -> acc
            | false ->
                match acc with
                | [] -> aux (i+1) [i]
                | x::xs -> aux (i+1) (x+i::acc)
                

        List.rev (aux 1 [])

    (* Question 1.3 *)
 
    let triangleNumberList (n : int) = 
        let lists = List.map (fun i -> [1..i]) [1..n]
        List.rev (List.fold (fun (acc : int list) (elem : int list) -> List.sum elem::acc) [] lists)
        
    (* Question 1.4 *)
 
    let sequence (f : (int -> 'a -> 'a)) (i : 'a) (n : int) = 
        
        let rec aux (list : 'a list) (index : int) =
            match index > n with
            | true -> list
            | false -> 
                match list with
                | [] -> aux [i] (index+1)
                | x::xs -> aux ((f index x)::list) (index+1)

        List.rev (aux [] 1)


     (* Question 1.5 *)
 
    let alternatingTriangle (n : int) = sequence (fun (i : int) (acc : int) -> if i % 2 = 0 then acc - i else acc + i) 1 n
           
    (* Question 2: Code comprehension *)
    
    let rec foo a =  
        match a with  
        | []            -> []  
        | [_]           -> []  
        | x :: y :: xs  -> (x, y) :: foo (y :: xs)
          
    let rec bar f a =  
        match a with  
        | (x, y) :: xs when f x y -> bar f xs  (* 1 *)
        | _      :: xs            -> false     (* 2 *)
        | []                      -> true      (* 3 *)

    let baz = foo >> bar (fun x y -> (x + y) % 2 <> 0)
    
    
    (* Quesiton 2.1 *)
    
    (*
    
    Q: The type of `baz` is `int list -> bool`. Give two different input lists,
       of length greater than five, that when you call `baz` give different results.
    A: 
        baz [1;2;3;4;5];; gives true
        baz [2;4;6;8;10];; gives false
    
    Q: What do the functions foo, bar and baz do? Focus on what they do rather than how they do it.
    A: 
        foo creates a list of pairs from a list, where each pair is (i, i+1), so for a list [1;2;3] list is [(1,2);(2,3)]
        bar iterates through a list of tuples and applies a predicate which takes two inputs and outputs a boolean, and
        ensures all elements in the list of tuples are true based on that predicate.
        
        baz checks if all integers in a list, if their pairs i + (i+1) are odd. If true it outputs true, if not it outputs false.

    Q: What would be appropriate names for functions foo, bar and baz?
    A: 
        foo: splitListToTuples
        bar: tupleListForAll
        baz: isListOdd

    Q: In the foo function, what would happen if you replaced the match [x] with [_]? Motivate your answer.
    A: Nothing would happen. This is because the underscore still tells the pattern match we want to match on the list,
        iff there is exactly one element, but the underscore just tells F# and more importantly a reader, that we intentionally,
        do not wish to evaluate/use the element.
    *)
    
    (* Question 2.2 *)
    
    (*
    
    Q: What would happen if you swap lines (* 1 *) and (* 2 *) in the `baz` function?
    A: It would, as long as the input list is not empty, always print false.
 
    Q: What would happen if you swap  lines (* 2 *) and (* 3 *) in the `baz` function?
    A: Nothing. This is because ( * 2 * ) checks if the list is not empty, and (* 3 *) checks if it is empty.
        Since both pattern matches check on explicit opposites, the order does not matter for these two.
     
     *)
    
    
    (* Question 2.3 *)


    let baz2 (a : int list) = 
        List.init (a.Length-1) (fun (i : int) -> a[i], a[i+1])
        |> List.fold (fun (acc : bool) (elem : (int * int)) -> if acc then (fst elem + snd elem) % 2 <> 0 else acc) true
    
    (* Question 2.4 *)
    
    (*

        
    
    Q: The function foo from Question 2.1 is not tail recursive. Explain why. To make a compelling argument you
       must evaluate a function call of the function, similarly to what is done in Chapter 1.4 of HR, and
       reason about that evaluation You need to make clear what aspects of the evaluation tell you that the
       function is not tail recursive. Keep in mind that all steps in an evaluation chain must evaluate to the
       same value ( (5 + 4) * 3 --> 9 * 3 --> 27, for instance).
       
    A: Foo is not tail recursive, as the :: operation will await on each stack. Below chain evaluation shows this clearly

        foo [1;2;3;4;5] -->
        (1,2)::(foo [2;3;4;5]) -->
        (1,2)::(2,3)::(foo [3;4;5]) -->
        (1,2)::(2,3)::(3,4)::(foo [4;5]) -->
        (1,2)::(2,3)::(3,4)::(4,5)::(foo [5]) -->
        (1,2)::(2,3)::(3,4)::(4,5)::[] -->
        [(1,2);(2,3);(3,4);(4,5)]

        As seen on the chain above, the "add list to head" (::) operation is awaited on each stack, until all recusive calls of foo
        has been evaluated, and only then can the list be assembled with the :: operation.
    
    *)
    
    (* Question 2.5 *)

    (*
    let rec foo a =  
        match a with  
        | []            -> []  
        | [_]           -> []  
        | x :: y :: xs  -> (x, y) :: foo (y :: xs)
    
    *)

    let fooTail (list : int list) = 

        let rec aux (list : int list) (f) =
            match list with
            | [] | [_] -> f []
            | x :: y :: xs -> aux (y::xs) (fun a -> f ((x,y)::a))

        aux list id
        
    (* Question 3: Locks *)
    
    type 'a store = {
        data  : 'a
        owner : int option
    }
    
    (* Question 3.1 *)

    let newStore (v : 'a) = 
        {data = v; owner = None}
    let lock (pid : int) (st : 'a store) = 
        match st.owner with
        | None -> { data = st.data; owner = Some pid}
        | Some pid2 -> 
            printfn "process %d to lock but the lock is held by %d" pid pid2
            st
    let unlock (pid : int) (st : 'a store) = 
        match st.owner with
        | Some pid2 when pid2 = pid -> { data = st.data; owner = None }
        | Some pid2 -> 
            printfn "process %d tried to unlock but the lock is held by %d" pid pid2
            st
        | None -> 
            printfn "process %d tried to unlock but no one holds the lock" pid
            st
    
    (* Question 3.2 *)
        
        
    let read (st : 'a store) = st.data
    let write (pid : int) (v : 'a) (st : 'a store) = 
        match st.owner with
        | Some pid2 when pid2 <> pid ->
            printfn "process %d tried to write %s but the lock is held by %d" pid (string v) pid2
            st
        | _ -> { data = v; owner = st.owner; }
    let isLocked (st : 'a store) = 
        match st.owner with
        | Some _ -> true
        | None -> false
    
    (* Question 3.3 *)
    
    type 'a message =
    | Lock of int * AsyncReplyChannel<unit>
    | Unlock of int
    | Read of AsyncReplyChannel<'a>
    | Write of int * 'a
    
    type 'a storeServer = Store of MailboxProcessor<'a message>
    
    let inbox x (mbox : MailboxProcessor<'a message>) =
        let rec messageLoop (st : 'a store) (pending : (int * AsyncReplyChannel<unit>) list) = async {
            let! message = mbox.Receive()


            match message with
            | Lock (pid, rc) -> 
                match isLocked st with
                | true -> return! messageLoop st ((pid,rc)::pending)
                | false -> 
                    let st = lock pid st
                    rc.Reply()
                    return! messageLoop st pending
            | Unlock pid -> 
                let st = unlock pid st
                match pending with
                | (pid, rc)::xs ->
                    let st = lock pid st
                    rc.Reply()
                    return! messageLoop st xs
                | [] -> return! messageLoop st []
            | Read rc -> 
                rc.Reply st.data
                return! messageLoop st pending
            | Write (pid, value) ->
                let st = write pid value st
                return! messageLoop st pending

        }
           
            
        messageLoop (newStore x) []
        
    (* Question 3.4 *)
        
    let createStore (v : 'a) : 'a storeServer = 
        Store (MailboxProcessor.Start (fun st -> inbox v st ))

    let storeLock (pid : int) (st : 'a storeServer) = 
        match st with
        | Store mbox -> 
            mbox.PostAndReply (fun rc -> Lock (pid, rc))
    let storeUnlock (pid : int) (st : 'a storeServer) = 
        match st with
        | Store mbox ->
            mbox.Post (Unlock pid)
    let storeRead (st : 'a storeServer) = 
        match st with
        | Store mbox -> 
            mbox.PostAndReply (fun rc -> Read (rc))
    let storeWrite (pid : int) (v : 'a) (st : 'a storeServer) = 
        match st with
        | Store mbox -> 
            mbox.Post (Write (pid, v))
    (* Question 3.4 *)
    let inc (pid : int) (st : 'a storeServer) = 
        match st with
        | Store mbox ->
            storeLock pid st
            let data = storeRead st
            storeWrite pid (data+1) st
            storeUnlock pid st
    
    let countTo (size : int) = 
        let st = createStore 0

        seq [1..size]
        |> Seq.map (fun pid -> async { inc pid st })
        |> Async.Parallel
        |> Async.Ignore
        |> Async.RunSynchronously

        match st with
        | Store mbox -> 
            mbox.PostAndReply (fun rc -> Read(rc))
    
    (* Question 4: Tic Tac Toe *)
    
    (* Question 4.1 *)
    
    type row = 
        | UpperRow
        | MiddleRow
        | BottomRow 

    type col = 
        | RightCol
        | MiddleCol
        | LeftCol

    type player =  
        | X
        | O 
    
    let X = X
    let O = O
    
    let topRow = UpperRow
    let midRow = MiddleRow
    let botRow = BottomRow
    
    let leftCol = LeftCol
    let midCol = MiddleCol
    let rightCol = RightCol
    
    type board = Map<(row * col),player>
    
    let empty : board = Map.empty
    
    (* Question 4.2 *)
    
    type error =  
        | PlayerTurn  of player
        | SquareTaken of row * col * player
        
    type state =
        | Running of player * board
        | Win of player * board
        | Draw of board

    let doMove (p : player) (r : row) (c : col) (st : state) = 

        let place (p : player) (r : row) (c : col) (b : board) : state = 
        
            let determineState (b : board) =
                let winningCombinations = 
                    [
                    [(topRow, leftCol); (midRow, leftCol); (botRow, leftCol)];
                    [(topRow, midCol); (midRow, midCol); (botRow, midCol)];
                    [(topRow, rightCol); (midRow, rightCol); (botRow, rightCol)];
                    [(topRow, rightCol); (topRow, midCol); (topRow, leftCol)];
                    [(midRow, rightCol); (midRow, midCol); (midRow, leftCol)]
                    [(botRow, rightCol); (botRow, midCol); (botRow, leftCol)];
                    [(topRow, leftCol); (midRow, midCol); (botRow, rightCol)];
                    [(topRow, rightCol); (midRow, midCol); (botRow, leftCol)]                
                    ]

                let rec checkIfWon (winStates : (row * col) list list) =
                    match winStates with
                    | [] -> Draw b
                    | x::xs -> 
                        let p1 =  b.TryFind x[0]
                        let p2 = b.TryFind x[1]
                        let p3 = b.TryFind x[2]

                        match p1, p2, p3 with
                        | Some p1', Some p2', Some p3' -> 
                            match p1' = p2' && p1' = p3' with
                            | true -> Win (p1', b)
                            | false -> checkIfWon xs
                        | _, _,_ -> checkIfWon xs
                checkIfWon winningCombinations

            match determineState (Map.add (r,c) p b) with
            | Win(p,b) -> Win (p,b)
            | Draw b -> 
                match b.Keys.Count = 9 with
                | true -> Draw b
                | false -> 
                    let newPlayer = if p = X then O else X
                    Running(newPlayer, b)
                    

        match st with
        | Win (p, b) -> Ok(Win(p,b))
        | Draw b -> Ok(Draw b)
        | Running (p', b) ->
             match p = p' with
             | false -> Error (PlayerTurn p')
             | true -> 
                match Map.tryFind (r,c) b with
                | Some p'' -> Error (SquareTaken(r,c,p''))
                | None -> Ok (place p r c b)

                
            
    (* Question 4.3 *)
    
    type ticTacToeMonad<'a> = TTT of (state -> Result<'a * state, error>)  

    let ret x = TTT (fun h -> (Ok (x, h)))  
    let fail err = TTT (fun _ -> Error err)  
    let bind f (TTT a)  =  
        TTT (fun h ->  
            match a h with  
            | Ok (x, h') ->  
                let (TTT g) = f x  
                g h'        
            | Error err -> Error err)
        
    let (>>=) a f = bind f a  
    let (>>>=) a b = a >>= (fun _ -> b)

    let evalTTT (TTT f) =
        Running(X, empty) |> f |> Result.map fst        
    
    let doMove2 (p : player) (r : row) (c : col) = 
        TTT (fun st -> 
            match doMove p r c st with
            | Ok (st') -> Ok ((), st') 
            | Error e -> 
                let (TTT f) = fail e
                f st)
        
    let gameOver = 
        TTT (fun st ->
            match st.IsWin || st.IsDraw with
            | true -> Ok (true,st)
            | false -> Ok (false,st))
        
    (*
    type state =
        | Running of player * board
        | Win of player * board
        | Draw of board
    
    *)


    let getBoard = 
        TTT (fun st -> 
            match st with
            | Running(_,b) -> Ok(b,st)
            | Win (_,b) -> Ok(b,st)
            | Draw b -> Ok(b,st))
         
    (* Question 4.4 *)
    
    type TicTacToeBuilder() =

        member this.Bind(f, x)    = bind x f
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Combine(a, b) = a >>= (fun _ -> b)

    let ttt = new TicTacToeBuilder()
    
    let rec playGame (moves : (player * row * col) list) = ttt {
        let! isGameOver = gameOver
        match isGameOver, moves with
        | true,_  | _, [] -> return! getBoard
        | false, x::xs ->
            match x with
            | (p,r,c) ->
                do! doMove2 p r c
                return! playGame xs
    }

    
    (* Question 4.5 *)
    
    let spaces = many pwhitespace

    let parseX = pstring "X" |>> (fun _ -> X)
    let parseO = pstring "O" |>> (fun _ -> O)
    let parsePlayer = pstring "Player" >>. spaces >>. choice [parseX; parseO] 
    
    let parseTopRow = pstring "topRow" |>> (fun _ -> topRow)
    let parseMidRow = pstring "midRow" |>> (fun _ -> midRow)
    let parseBotRow = pstring "botRow" |>> (fun _ -> botRow)
    
    let parseLeftCol = pstring "leftCol" |>> (fun _ -> leftCol)
    let parseMidCol = pstring "midCol" |>> (fun _ -> midCol)
    let parseRightCol = pstring "rightCol" |>> (fun _ -> rightCol)
    
    let parseRow = choice [parseTopRow; parseMidRow; parseBotRow]
    let parseCol = choice [parseLeftCol; parseMidCol; parseRightCol]
    let parseMove = 
        parsePlayer .>> 
        spaces .>> 
        pstring "places a tile on row" .>> 
        spaces .>>. 
        parseRow .>> 
        spaces .>>
        pstring "and column"
        .>> spaces
        .>>. parseCol |>> (fun ((p, r), c) -> (p,r,c))
        
    let parseMoves = many (choice [spaces >>. parseMove .>> spaces])