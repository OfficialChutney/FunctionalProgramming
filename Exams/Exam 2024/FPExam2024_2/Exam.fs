module Exam2024

(* If you are importing this into F# interactive then comment out
   the line above and remove the comment for the line bellow.

   Do note that the project will not compile if you do this, but 
   it does allow you to work in interactive mode and you can just remove the '=' 
   to make the project compile again.

   You will also need to load JParsec.fs. Do this by typing
   #load "JParsec.fs" 
   in the interactive environment. You may need the entire path.

   Do not remove the module declaration (even though that does work) because you may inadvertently
   introduce indentation errors in your code that may be hard to find when switching back to project mode.

   Alternative, keep the module declaration as is, but load ExamInteractive.fsx into the interactive environment
   *)
(*
 module Exam2024 = 
 *)

(* 1: Transactions *)

    type transactions =
        | Empty
        | Pay     of string * int * transactions
        | Receive of string * int * transactions
        
    let rec balance (trs : transactions) : int = 
        match trs with
        | Empty -> 0
        | Pay(name, amount, trs) -> balance trs - amount 
        | Receive(name, amount, trs) -> balance trs + amount
        
    let balanceAcc (trs : transactions) : int = 
        let rec aux (acc : int) newTrs =
            match newTrs with
            | Empty -> acc
            | Pay(name, amount, newTrs) -> aux (acc - amount) newTrs
            | Receive(name, amount, newTrs) -> aux (acc + amount) newTrs
        aux 0 trs
        
    let rec participants (trs : transactions) : Set<string> * Set<string> =
            match trs with
            | Empty -> (Set.empty<string>, Set.empty<string>)
            | Pay(name, amount, trs) -> 
                let (fPaid, fReceived) = participants trs
                (Set.add name fPaid, fReceived)
            | Receive(name, amount, trs) -> 
                let (fPaid, fReceived) = participants trs
                (fPaid, Set.add name fReceived)
    
    let rec balanceFold (payFolder : 'a -> string -> int -> 'a) (receiveFolder : 'a -> string -> int -> 'a) (acc : 'a) (trs : transactions) : 'a =
        match trs with
        | Empty -> acc
        | Pay(name, amount, rest) -> 
            let newAcc = payFolder acc name amount
            balanceFold payFolder receiveFolder newAcc rest
        | Receive (name, amount, rest) -> 
            let newAcc = receiveFolder acc name amount
            balanceFold payFolder receiveFolder newAcc rest

    
    let collect (trs : transactions) : Map<string, int> = 
       balanceFold 
        (fun acc name amount ->  
            match Map.tryFind name acc with
            | Some exsAmount -> 
                let newAmount = exsAmount - amount
                Map.add name newAmount acc
            | None -> 
                let start = 0 - amount
                Map.add name start acc)
        (fun acc name amount -> 
            match Map.tryFind name acc with
            | Some exsAmount -> 
                let newAmount = exsAmount + amount
                Map.add name newAmount acc
            | None -> 
                let start = 0 + amount
                Map.add name start acc) 
        Map.empty<string, int> trs
    
(* 2: Code Comprehension *)
        
    let foo (x : char) = x |> int |> fun y -> y - (int '0')
    
    let bar (x : string) = [for c in x -> c]
            
    let rec baz =
        function
        | [] -> 0
        | x :: xs -> x + 10 * baz xs
    
(* Question 2.1 *)

    (* 
    
    Q: What are the types of functions foo, bar, and baz?

    A: foo: char -> int
       bar: string -> char list
       baz: int list -> int


    Q: What do the function foo, bar, and baz do.
       Focus on what they do rather than how they do it.

    A: foo takes a char and returns it's actual numerical value by using ascii.
       bar takes a string and return a list of chars with all elements from the string.
       baz takes an int list and returns the list of numbers as a single base 10 number.
    
    Q: What would be appropriate names for functions 
       foo, bar, and baz?

    A: foo: charToInt
       bar: stringToCharList (or explode)
       baz: listToBaseTen
    
    Q: The function foo only behaves reasonably if certain 
       constraint(s) are met on its argument. 
       What is/are these constraints?
        
    A: It only makes sense when giving foo the char 0-9, otherwise the value
       it returns, is not the equivalent value.
    
    Q: The function baz only behaves reasonably if certain 
       constraint(s) are met on its argument. 
       What is/are these constraints?
        
    A: The baz function can onyl build base 10 numbers if it is giving the int from 0-9
    in the list, otherwise the logic of the math is lost. You will also have an overflow
    if you pass it too long of a list.  *)
    
(* Question 2.2 *)
    
    let stringToInt =
        bar >> List.map foo >> List.rev >> baz

(* Question 2.3 *)
    
    let baz2 intList = 
        List.foldBack (fun x acc -> x + 10 * acc) intList 0

    let baz3 = List.foldBack (fun x acc -> x + 10 * acc)
    
(* Question 2.4 *)

    (*

    Q: The function `bar` from Question 2.1 is not tail recursive. Demonstrate why.
       To make a compelling argument you should evaluate a function call of the function,
       similarly to what is done in Chapter 1.4 of HR, and reason about that evaluation. 
       You need to make clear what aspects of the evaluation tell you that the function 
       is not tail recursive. Keep in mind that all steps in an evaluation chain must 
       evaluate to the same value ( (5 + 4) * 3 --> 9 * 3 --> 27 , for instance).
       
       You do not have to step through the foo-function. You are allowed to evaluate 
       that function immediately.

    A: 
    let rec baz =
        function
        | [] -> 0
        | x :: xs -> x + 10 * baz xs

    baz [1; 2; 3] -->
    1 + 10 * baz [2; 3] -->
    1 + 10 * (2 + 10 * baz [3]) -->
    1 + 10 * (2 + 10 * (3 + 10 * baz [])) -->
    1 + 10 * (2 + 10 * (3 + 10 * 0)) -->
    1 + 10 * (2 + 10 * (3 + 0))
    1 + 10 * (2 + 10 * 3) -->
    1 + 10 * (2 + 30) -->
    1 + 10 * 32 -->
    1 + 320 -->
    321 

    The baz function is not tail recursive because it still has to perform calculations 
    after the recursive call returns, as seen in my trace above. For a function to be
    tail recursive, the recursive call must be the final action the function performs, 
    with no pending math waiting for the result. 

    *)
    
(* Question 2.5 *)

    let bazTail = 
        let rec aux cont =
            function
            | [] -> cont 0
            | x :: xs -> aux (fun res -> cont(x + 10 * res)) xs
        aux id
        
(* 3: Caesar Ciphers *)

(* Question 3.1 *)
    
    let encrypt (text : string) (offset : int) : string = 
        String.map (fun c -> 
            if c = ' ' then ' ' else 
                let a = int 'a' // base value of a in ascii
                let newC = int c // current value of actual char in the string
                let zero = newC - a // subtract the actual value from base value, to give it normal index and not ascii
                let shiftC = zero + offset //apply cipher by adding offset
                let wrap = ((shiftC % 26) + 26) % 26 // takes modulo to make char shift back to start if larger than 25
                let restoredChar = wrap + a // add base value back to get ascii value
                char restoredChar // converts it to char again to be put back into String.map
            ) text
                
    
(* Question 3.2 *)
    let decrypt (text : string) (offset : int) : string = 
        String.map (fun c -> 
            if c = ' ' then ' ' else 
                let a = int 'a' // base value of a in ascii
                let newC = int c // current value of actual char in the string
                let zero = newC - a // subtract the actual value from base value, to give it normal index and not ascii
                let shiftC = zero - offset //apply cipher by adding offset
                let wrap = ((shiftC % 26) + 26) % 26 // takes modulo to make char shift back to start if larger than 25
                let restoredChar = wrap + a // add base value back to get ascii value
                char restoredChar // converts it to char again to be put back into String.map
            ) text

    let decrypt2 text offset =
        encrypt text (-offset)
    
(* Question 3.3 *)
    let decode (plainText : string) (encryptedText : string) : int option = 
        if plainText = encryptedText then Some 0 else
            let rec helper pList eList =
                match pList, eList with
                | ' ' :: ps, ' ' :: es -> helper ps es
                | p :: ps, e :: es -> 
                    let newP = int p
                    let newE = int e
                    let distance = newE - newP
                    let sOffset = ((distance % 26) + 26) % 26
                    if encrypt plainText sOffset = encryptedText then Some sOffset
                    else None
                | _ -> None
            helper (Seq.toList plainText) (Seq.toList encryptedText)
            
    let decodeSeq (plainText : string) (encryptedText : string) : int option = 
        if plainText = encryptedText then Some 0 else

            let zip = Seq.zip plainText encryptedText
            let fPair = zip |> Seq.tryFind (fun (p, e) -> p <> ' ')

            match fPair with
            | Some (p, e) -> 
                let distance = int e - int p
                let sOffset = ((distance % 26) + 26) % 26
                if encrypt plainText sOffset = encryptedText then Some sOffset
                else None
            | None -> None

    let decodeBruteForce (plainText : string) (encryptedText : string) : int option =
        List.tryFind (fun sOffset -> encrypt plainText sOffset = encryptedText) [0 .. 25]
    
(* Question 3.4 *)
    let parEncrypt (text : string) (offset : int) : string =
        text.Split(' ') |> Array.map (fun word -> async { return encrypt word offset }) |>
        Async.Parallel |> Async.RunSynchronously |>
        String.concat " "


    
(* Question 3.5 *)
        
    open JParsec.TextParser

    let parseEncrypt (offset : int) : Parser<string> = 
        many (satisfy (fun c -> c >= 'a' &&  c <= 'z' || c = ' ')) |>> 
        (fun text -> encrypt (System.String.Concat(text)) offset)



(* 4: Letterboxes *)
    
(* Question 4.1 *)
    
    type letterbox = unit // Replace with your type
    
    let empty _ = failwith "not imlpemented"

(* Question 4.2 *)

    let post _ = failwith "not imlpemented"
    
    let read _ = failwith "not imlpemented"

    
(* Question 4.3 *)
    type StateMonad<'a> = SM of (letterbox -> ('a * letterbox) option)  
      
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
      
    let evalSM (SM f) = f (empty ())
    
    let post2 _ = failwith "not implemented"
    let read2 _ = failwith "not implemented"

(* Question 4.4 *)

    type StateBuilder() =

        member this.Bind(f, x)    = bind x f
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Combine(a, b) = a >>= (fun _ -> b)

    let state = StateBuilder()

    type MType =
        | Post of string * string
        | Read of string
    type log = MType list
    
    let trace _ = failwith "not implemented"