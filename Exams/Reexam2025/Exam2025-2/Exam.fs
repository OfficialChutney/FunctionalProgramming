module Exam2025_Template.Exam

    open JParsec.TextParser
    
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
 
    let triangleNumberList x _ = failwith "not implemented"
        
    (* Question 1.4 *)
 
    let sequence f i l= failwith "not implemented"
        
     (* Question 1.5 *)
 
    let alternatingTriangle _ = failwith "not implemented"
           
    (* Question 2: Code comprehension *)
    
    let rec foo a =  
        match a with  
        | []            -> []  
        | [x]           -> []  
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
    A: <Your answer goes here>
    
    Q: What do the functions foo, bar and baz do? Focus on what they do rather than how they do it.
    A: <Your answer goes here>     

    Q: What would be appropriate names for functions foo, bar and baz?
    A: <Your answer goes here>     

    Q: In the foo function, what would happen if you replaced the match [x] with [_]? Motivate your answer.
    A: <Your answer goes here>     

    *)
    
    (* Question 2.2 *)
    
    (*
    
    Q: What would happen if you swap lines (* 1 *) and (* 2 *) in the `baz` function?
    A: <Your answer goes here>
 
    Q: What would happen if you swap  lines (* 2 *) and (* 3 *) in the `baz` function?
    A: <Your answer goes here>
     
     *)
    
    
    (* Question 2.3 *)
    
    let baz2 _ = failwith "not implemented"
    
    (* Question 2.4 *)
    
    (*
    
    Q: The function foo from Question 2.1 is not tail recursive. Explain why. To make a compelling argument you
       must evaluate a function call of the function, similarly to what is done in Chapter 1.4 of HR, and
       reason about that evaluation You need to make clear what aspects of the evaluation tell you that the
       function is not tail recursive. Keep in mind that all steps in an evaluation chain must evaluate to the
       same value ( (5 + 4) * 3 --> 9 * 3 --> 27, for instance).
       
    A: <Your answer goes here>
    
    *)
    
    (* Question 2.5 *)

    let fooTail _ = failwith "not implemented"
        
    (* Question 3: Locks *)
    
    type 'a store = {
        data  : 'a
        owner : int option
    }
    
    (* Question 3.1 *)

    let newStore _ = failwith "not implemented"
    let lock _ = failwith "not implemented"
    let unlock _ = failwith "not implemented"
    
    (* Question 3.2 *)
        
        
    let read _ = failwith "not implemented"
    let write _ = failwith "not implemented"
    let isLocked _ = failwith "not implemented"
    
    (* Question 3.3 *)
    
    type 'a message =
    | Lock of int * AsyncReplyChannel<unit>
    | Unlock of int
    | Read of AsyncReplyChannel<'a>
    | Write of int * 'a
    
    type 'a storeServer = Store of MailboxProcessor<'a message>
    
    let inbox x (mbox : MailboxProcessor<'a message>) =
        let rec messageLoop (st : int store) (pending : (int * AsyncReplyChannel<unit>) list) =
            failwith "not implemented"
            
        messageLoop (newStore x) []
        
    (* Question 3.4 *)
        
    let createStore _ = failwith "not implemented"

    let storeLock _ = failwith "not implemented"
    let storeUnlock _ = failwith "not implemented"
    let storeRead _ = failwith "not implemented"
    let storeWrite _ = failwith "not implemented"
    
    (* Question 3.4 *)
    let inc _ = failwith "not implemented"
    
    let countTo _ = failwith "not implemented"     
    
    (* Question 4: Tic Tac Toe *)
    
    (* Question 4.1 *)
    
    type row = unit // your type goes here
    type col = unit // your type goes here
    type player = unit // your type goes here
    
    let X = ()
    let O = ()
    
    let topRow = ()
    let midRow = ()
    let botRow = ()
    
    let leftCol = ()
    let midCol = ()
    let rightCol = ()
    
    type board = unit // your type goes here
    
    let empty = ()
    
    (* Question 4.2 *)
    
    type error =  
        | PlayerTurn  of player
        | SquareTaken of row * col * player
        
    type state =
        | Running of player * board
        | Win of player * board
        | Draw of board

    let doMove _ = failwith "not implemented"
            
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

    
    
    
    let doMove2 _ = failwith "not implemented"
        
    let gameOver _ = failwith "not implemented"
        
    let getBoard _ = failwith "not implemented"
         
    (* Question 4.4 *)
    
    type TicTacToeBuilder() =

        member this.Bind(f, x)    = bind x f
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Combine(a, b) = a >>= (fun _ -> b)

    let ttt = new TicTacToeBuilder()
    
    let rec playGame _ = failwith "not implemented"
    
    (* Question 4.5 *)
    
    let parseX _ = failwith "not implemented"
    let parseO _ = failwith "not implemented"
    let parsePlayer _ = failwith "not implemented"
    
    let parseTopRow _ = failwith "not implemented"
    let parseMidRow _ = failwith "not implemented"
    let parseBotRow _ = failwith "not implemented"
    
    let parseLeftCol _ = failwith "not implemented"
    let parseMidCol _ = failwith "not implemented"
    let parseRightCol _ = failwith "not implemented"
    
    let parseRow _ = failwith "not implemented"
    let parseCol _ = failwith "not implemented"
    let parseMove _ = failwith "not implemented"
        
    let parseMoves _ = failwith "not implemented"