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
        
    let rec balance (trs: transactions) : int = 
        match trs with
        | Empty -> 0
        | Pay(_,x,y) -> - x + balance y
        | Receive(_,x,y) -> x + balance y
        
    let balanceAcc (trs: transactions) : int = 
        let rec loop trs acc = 
            match trs with
            | Empty -> acc
            | Pay (_,x,y) -> loop y (acc - x)
            | Receive (_,x,y) -> loop y (acc + x)
        loop trs 0


    let rec participants (trs: transactions) : Set<string> * Set<string> = 
        match trs with
        | Empty -> (Set.empty,Set.empty)
        | Pay(x,_,y) -> 
            let (payers, receivers) = participants y
            (Set.add x payers,receivers)
        | Receive(x,_,y) -> 
            let (payers, receivers) = participants y
            (payers,Set.add x receivers)


    let rec balanceFold (payFolder: 'a -> string -> int -> 'a) (receiveFolder: 'a -> string -> int -> 'a) acc (trs: transactions) =
        match trs with
        | Empty -> acc
        | Pay(name,amount,trs') -> (balanceFold payFolder receiveFolder (payFolder acc name amount) trs')
        | Receive(name, amount, trs') ->  (balanceFold payFolder receiveFolder (receiveFolder acc name amount) trs')
    
    let collect (trs: transactions) : Map<string,int> = 
        balanceFold
            (fun acc name amount -> 
                let current = Map.tryFind name acc 
                match current with
                | Some x -> Map.add name (x + amount) acc
                | None   -> Map.add name amount acc
            )
            (fun acc name amount -> 
                let current = Map.tryFind name acc
                match current with
                | Some x -> Map.add name (x + amount) acc
                | None   -> Map.add name amount acc
            )
            Map.empty
            trs
    

            
    
    
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

    A:  foo has type char -> int
        bar has type string -> list<char>
        baz has type list<int> -> int


    Q: What do the function foo, bar, and baz do.
       Focus on what they do rather than how they do it.

    A:  foo: Makes a char into an int
        bar: Makes a string into a list of chars
        baz: Takes an int adds 10 to it and multiplies it 
    
    Q: What would be appropriate names for functions 
       foo, bar, and baz?

    A:  foo: CharToInt
        bar: StringToList
        baz: 

    Q: The function foo only behaves reasonably if certain 
       constraint(s) are met on its argument. 
       What is/are these constraints?
        
    A: The char has to be a number and not any other type 
    
    Q: The function baz only behaves reasonably if certain 
       constraint(s) are met on its argument. 
       What is/are these constraints?
        
    A: ????    *)
    
(* Question 2.2 *)
    
    let stringToInt (str: string) = str |> bar |> List.map foo |> List.rev |> baz


(* Question 2.3 *)
    
    let baz2 xs = List.foldBack (fun x acc -> x + 10 * acc) xs 0

(*  let rec baz =
        function
        | [] -> 0
        | x :: xs -> x + 10 * baz xs
        *)



(* Question 2.4 *)

    (*

    Q: The function `baz` from Question 2.1 is not tail recursive. Demonstrate why.
       To make a compelling argument you should evaluate a function call of the function,
       similarly to what is done in Chapter 1.4 of HR, and reason about that evaluation. 
       You need to make clear what aspects of the evaluation tell you that the function 
       is not tail recursive. Keep in mind that all steps in an evaluation chain must 
       evaluate to the same value ( (5 + 4) * 3 --> 9 * 3 --> 27 , for instance).
       
       You do not have to step through the foo-function. You are allowed to evaluate 
       that function immediately.

    A: <Your answer goes here>

    *)
    
(* Question 2.5 *)

    let bazCont lst =
        let rec bazTail lst c  = 
            match lst with
            | x :: xs -> bazTail xs (fun r -> c (x + 10 * r))
            | [] -> c 0
        bazTail lst (fun x -> x)

(* 3: Caesar Ciphers *)

(* Question 3.1 *)
    
    let encrypt (text: string) (offset: int) = String.map (fun c -> if c = ' ' then ' ' else char ((int c - int 'a' + offset) % 26 + int 'a')) text

(* Question 3.2 *)
    let decrypt(text: string) (offset: int)  = String.map (fun c -> if c = ' ' then ' ' else char ((int c - int 'a' - (offset % 26) + 26) % 26 + int 'a')) text
    
(* Question 3.3 *)
    let decode (plainText: string) (encryptedText: string) = 
        List.tryFind(fun offset -> encrypt plainText offset = encryptedText) [0..25]
    
(* Question 3.4 *)
    let parEncrypt (text: string) (offset: int) = text.Split(' ') |> Array.Parallel.map (fun word -> encrypt word offset) |> String.concat " "
    
(* Question 3.5 *)
        
    open JParsec.TextParser

    let parseEncrypt (offset: int) : Parser<string> = 
        many (satisfy (fun c -> c = ' ' || (c >= 'a' && c <= 'z')))
        |>> (fun chars -> 
        let str = System.String(Array.ofList chars)
        encrypt str offset)


(* 4: Letterboxes *)
    
(* Question 4.1 *)
    
    type letterbox = Map<string, string list>
   
    
    let empty () : letterbox = Map.empty 

(* Question 4.2 *)

    let post (sender: string) (message: string) (mb: letterbox) : letterbox = 
        match Map.tryFind sender mb with
        | Some msg -> Map.add sender (msg @ [message]) mb
        | None -> Map.add sender [message] mb
        
    let read (sender: string) (mb: letterbox)  = 
        match Map.tryFind sender mb with
        | None -> None
        | Some [] -> None
        | Some (first :: rest) -> Some (first, Map.add sender rest mb)

    
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
    
    let post2 (sender: string) (message: string) = 
        SM(fun s ->
            let curr = Map.tryFind sender s
            let newList = 
                match curr with
                | Some xs -> xs @ [message]
                | None -> [message]
            let updated = Map.add sender newList s
            Some ((), updated))
            
            
            
    let read2 (sender: string) = 
        SM(fun s ->
                match Map.tryFind sender s with
                | Some (msg :: rest) -> Some (msg, Map.add sender rest s)
                | _ -> None 
            )

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
    
    let trace (l: log) : StateMonad<string list> =
        let rec loop l acc =
            match l with
            | [] -> ret acc
            | Post(sender, message) :: rest ->
                post2 sender message >>= fun _ ->
                loop rest acc
            | Read(sender) :: rest ->
                read2 sender >>= fun msg ->
                loop rest (acc @ [msg])
        loop l []