module Exam2025_Template.Exam

    open JParsec.TextParser

    (* Quesiton 1: Coordinates and rectangles (25%) *)
    
    type coord     = C of int * int  
    type rectangle = R of coord * int * int
    
    (* Question 1.1 *)
    
    let valid (r : rectangle) = 
        match r with
        | R (_, h, w) -> h > 0 && w > 0
    
    (* Question 1.2 *)
    
    let coords (r : rectangle) = 

        let startX, startY, width, height =
            match r with
            | R(C(x,y),w,h) -> x,y,w-1,h-1

        let maxX = startX + width
        let maxY = startY + height


        let rec aux (c : coord) =
            match c with
            | C (x,y) when x = maxX && y = maxY -> Set.empty.Add (C (x, y))
            | C (x,y) when x = maxX && y < maxY -> (aux (C (startX, y+1))).Add(C (x,y))
            | C (x,y) when x < maxX && y <= maxY -> (aux (C (x+1, y))).Add(C (x,y))

        aux (C (startX, startY))

    
    (* Question 1.3 *)
    
    let coordsAcc (r : rectangle) = 
        let startX, startY, width, height =
            match r with
            | R(C(x,y),w,h) -> x,y,w-1,h-1

        let maxX = startX + width
        let maxY = startY + height

        let rec aux (c : coord) (coords : Set<coord>) =
            match c with
            | C (x,y) when x = maxX && y = maxY -> coords.Add (C (x, y))
            | C (x,y) when x = maxX && y < maxY -> aux (C (startX, y+1)) (coords.Add(C (x,y)))
            | C (x,y) when x < maxX && y <= maxY -> aux (C (x+1, y)) (coords.Add(C (x,y)))   

        aux (C (startX, startY)) Set.empty
    
    (* Question 1.4 *)
    
    let merge (list : rectangle list) = 
        List.fold (fun (acc : Set<coord>) (r : rectangle) -> 
            match valid r with
            | true -> Set.union acc (coordsAcc r)
            | false -> acc) Set.empty list
            

    (* Question 1.5 *)
    
    let rectFold (f : ('b -> coord -> 'b)) (acc : 'b) (r : rectangle) =
        let startX, startY, width, height =
            match r with
            | R(C(x,y),w,h) -> x,y,w-1,h-1

        let maxX = startX + width
        let maxY = startY + height

        let rec aux (f : ('b -> coord -> 'b)) (acc : 'b) (c : coord) =
            match c with
            | C (x,y) when x = maxX && y = maxY -> f acc (C (x,y))
            | C (x,y) when x = maxX && y < maxY -> aux f (f acc (C (x,y))) (C (startX, y+1))
            | C (x,y) when x < maxX && y <= maxY -> aux f (f acc (C (x,y))) (C (x+1, y))

        aux f acc (C (startX, startY))
    let coords2 (r : rectangle) = rectFold (fun (acc : Set<coord>) (elem : coord) -> acc.Add elem) Set.empty r
    
    (* Question 2: Code comprehenision (25%) *)
    
    let rec foo a b =  
        match a, b with  
        | a :: c, b :: d when a < b -> a :: foo c (b :: d)  
        | a :: c, b :: d            -> b :: foo (a :: c) d  
        | [], _ -> b  
        | _, [] -> a  
    
    let rec bar a =  
        match List.length a with  
        | 0 -> []                                              (* 1 *)
        | 1 -> a                                               (* 2 *)
        | l -> foo (bar a.[0.. l / 2 - 1]) (bar a.[l / 2 .. l - 1]) (* 3 *)

    printfn "%d" (1 / 2 - 1)

    (* Question 2.1 *)
    (*
     
     Q: What do the functions `foo` and `bar`? Focus on what they do rather than how they do it.
     A: 
        foo merges two lists, and as long as both inputted lists are sorted, ensures the resulting list is sorted.

        bar splits the input list in two equal chunks recursively and calls foo on those two lists.

        bar and foo combined is mergesort, which sorts a list.

     
     Q: What would be appropriate names for functions functions `foo` and `bar`?
     A: 
        foo = merge.
        bar = mergesort.     
     
     Q: The function `foo` only behaves reasonably if certain constraint(s)
        are met on its argument. What is/are these constraints?
     A: Both input lists needs to be sorted beforehand for it to produce a correctly sorted list.
    *)
    
    (* Question 2.2 *)
    
    (*
    
    Q: Consider the match statement in `bar`. What would happen if you

       * Swap lines `(* 1 *)` and `(* 2 *)`
       * Swap lines `(* 1 *)` and `(* 3 *)`
       * Swap lines `(* 2 *)` and `(* 3 *)`

       You must motivate your answer.
       
    A: 
        if one and two are swapped, nothing would happen. They both pattern match on specific numbers,
        so if it is 0 the match statement for 1 would still not run

        If one and three are swapped, it would always try to mergesort, even though the list only has a single element,
        or none at all. Since the statement has [0..l / 2 - 1], if l is zero, we would have 0 / 2 - 1, which is -1. Therefor
        an exception would be thrown as no -1 index exists.

        If two and three are swapped, the 0 base case would be caught, but when only one element,

        we would again have l / 2 -1. Since l is 1, we would have 1 / 2 - 1. Due to integer division, 1/2 = 0, and therefor again,
        we have an index of -1, which would throw an exception.
    
    *)
    
    (* Question 2.3 *)
    (*
    
    Q: The line 

       match List.length a with
     
      in the bar function causes  the function to not be as performant as it could be.
      Explain why.
    
    A: This is because a list implementation is a Linked List, and therefor the length is stored at the end node.
        Everytime this is called, it has to check the length og the list.
        Since we compute the length when rezising, it would be better to have an auxilery recursive function,
        which takes a length as a parameter, and in the auxillery function we check on the parameter.
        Then we only check the length once, and not for every recursive call, when we start the auxillery function.
    
    *)
    
    let bar2 (a : 'a list) = 

        let rec aux (a : 'a list) (length : int) =
            match length with  
            | 0 -> []                                             
            | 1 -> a                                               
            | l ->
                let lengthA = l / 2
                let lengthB = l - lengthA
                foo (aux a.[0.. l / 2 - 1] lengthA) (aux a.[l / 2 .. l - 1] lengthB) 

        aux a (List.length a)
    
    (* Question 2.4 *)

    (*
      Q: The function  `foo` from Question 2.1 is not tail recursive. Explain why.
         To make a compelling argument you must evaluate a function call of the function,
         similarly to what is done in Chapter 1.4 of HR, and reason about that evaluation.
         You need to make clear what aspects of the evaluation tell you that the function
         is not tail recursive. Keep in mind that all steps in an evaluation chain must
         evaluate to the same value (```(5 + 4) * 3 --> 9 * 3 --> 27```, for instance).
         
      A: 
        It is not tail recursive because the "adding to head of list" operation is awaiting on the stack every time
        we recursively call foo. Here is an evaluation chain to make it clear:
        foo [2;3;4] [6;7;8] -->
        2::(foo [3;4] [6;7;8]) -->
        2::3::(foo [4] [6;7;8]) -->
        2::3::4::(foo [] [6;7;8]) -->
        2::3::4::6::7::8 -->
        [2;3;4;6;7;8]

        As we can see on the chain above, the :: operation is awaited on the stack until all
        recursive calls on foo has been made, before we can evaluate the resulting list only when
        all recursive calls has been made.


    *)
    
    (* Question 2.5 *)
    
    (*    let rec foo a b =  
        match a, b with  
        | a :: c, b :: d when a < b -> a :: foo c (b :: d)  
        | a :: c, b :: d            -> b :: foo (a :: c) d  
        | [], _ -> b  
        | _, [] -> a  *)


    let footail (a : 'a list) (b : 'a list) = 

        let rec aux (a : 'a list) (b : 'a list) (f) =
            match a, b with  
            | a :: c, b :: d when a < b -> aux c (b::d) (fun x -> a::x)(*a :: foo c (b :: d)  *)
            | a :: c, b :: d            -> aux (a::c) d (fun x -> b::x) (*b :: foo (a :: c) d  *)
            | [], _ -> f b  
            | _, [] -> f a

        aux a b id



    
    (* Question 3: Dining philosophers (25%) *)
    
    (* Question 3.1 *)
    
    type table = T of bool []
    
    let setTable (size : int) : table = T (Array.create size false)
    
    let getLeftFork (t : table) (p : int) = 
        match t with
        | T a ->
            match a[p] with
            | false -> a[p] <- true
            | true -> printfn "The philosopher %d tried to pick up their left fork, but it is already taken" p
    let getRightFork (t : table) (p : int) = 
        match t with
        | T a ->
            let wrapNum = (p+1) % a.Length
            match a[wrapNum] with
            | false -> a[wrapNum] <- true
            | true -> printfn "The philosopher %d tried to pick up their right fork, but it is already taken" p
    
    let putLeftFork (t : table) (p : int) = 
        match t with
        | T a ->
            match a[p] with
            | true -> a[p] <- false
            | false -> printfn "The philosopher %d tried to put down their left fork, but it is already on the table" p
        
    let putRightFork (t : table) (p : int) = 
        match t with
        | T a ->
            let wrapNum = (p+1) % a.Length
            match a[wrapNum] with
            | true -> a[wrapNum] <- false
            | false -> printfn " The philosopher %d tried to put down their right fork, but it is already on the table" p

    (* Question 3.2 *)
    
    let eat (t : table) (p : int) = 
        getLeftFork t p
        getRightFork t p
    
    let think (t : table) (p : int) = 
        putLeftFork t p
        putRightFork t p
    
    let canEat (t : table) (p : int) = 
        match t with
        | T a -> 
            let wrapNum = (p+1) % a.Length
            match a[p], a[wrapNum] with
            | false, false -> true
            | _ -> false


    (*
    let agent =
    MailboxProcessor.Start(fun inbox ->

        // state loop
        let rec loop (state : int) = async {

            // receive next message
            let! msg = inbox.Receive()

            match msg with

            // fire-and-forget message
            | Msg1 n ->
                printfn "Received number: %d" n
                return! loop (state + n)

            // another message type
            | Msg2 text ->
                printfn "Received text: %s" text
                return! loop state

            // request/reply message
            | GetValue rc ->
                rc.Reply state
                return! loop state

            // terminate agent
            | Stop ->
                printfn "Stopping"
                return ()
        }

        // initial state
        loop 0
    )
    
    
    *)

    (* Question 3.3 *)
    
    type message =
        | Eat of int * AsyncReplyChannel<unit>
        | Think of int
        
    type philosopherTable = Phil of MailboxProcessor<message>

    let inbox size (mbox : MailboxProcessor<message>) =  
        let t = setTable size
        
        let getTableArray =
            match t with
            | T a -> a


        let rec messageLoop (pending : (int * AsyncReplyChannel<unit>) list) = async {
            let! message = mbox.Receive()
                    
            let rec letAllEat (pending : (int * AsyncReplyChannel<unit>) list) =
                match pending with
                | [] -> []
                | x::xs -> 
                    match canEat t (fst x) with
                    | true -> 
                        eat t (fst x)
                        (snd x).Reply ()
                        printfn "philosopher %d is eating" (fst x)
                        letAllEat xs
                    | false -> x::(letAllEat xs)

            match message with
            | Eat (pid, rc) -> 
                match canEat t pid with
                | true -> 
                    eat t pid
                    rc.Reply ()
                    printfn "philosopher %d is eating" pid
                    return! messageLoop pending
                | false -> return! messageLoop ((pid, rc)::pending)
            | Think pid -> 
                think t pid
                printfn "philosopher %d is thinking" pid
                return! messageLoop (letAllEat pending)

        }
        
        messageLoop []
        
    (* Question 3.4 *)
    
    let newTable (size : int) : philosopherTable = Phil (MailboxProcessor.Start(fun mbox -> (inbox size mbox)))

    
    let philEat (pt : philosopherTable) (p : int) = 
        match pt with
        | Phil mbox ->
            mbox.PostAndReply (fun rc -> Eat(p, rc))
    
    let philThink (pt : philosopherTable) (p : int) = 
        match pt with
        | Phil mbox -> 
            mbox.Post (Think p)
    
    (* Question 3.5 *)
    
    let random = System.Random(42)
    
    let philosopher (p : int) (meals : int) (t : int) (pt : philosopherTable) = 
        async {
            let rec doPhilThings (i : int) =
                match i = meals with
                | true -> ()
                | false -> 
                    philEat pt p
                    do Async.Sleep (random.Next (t+1)) |> Async.RunSynchronously
                    philThink pt p
                    do Async.Sleep (random.Next (t+1)) |> Async.RunSynchronously
                    doPhilThings (i+1)
            return doPhilThings 0
        }
    
    let diningPhilosophers (phils : int) (meals : int) (t : int) = 
        let pt = newTable phils

        Seq.init phils (fun (i : int) -> philosopher i meals t pt)
        |> Async.Parallel
        |> Async.Ignore
        |> Async.RunSynchronously

        printfn "Everyone is done eating"

    (* Question 4: The Towers of Hanoi (25%) *)
    
    (* Question 4.1 *)
    
    type peg = 
        | Start
        | Middle
        | Goal
        
    type disc = int
    type hanoi = Map<peg, disc list>
    
    let start = Start // your value goes here
    let middle = Middle // your value goes here
    let goal = Goal // your value goes here
    
    let size (d : disc) : int = d 
    
    let newGame (numDiscs : int) : hanoi = 
        let makeDisc (i : int) : disc = i

        Map.empty.Add(start, List.init numDiscs (fun (i : int) -> makeDisc (numDiscs - i))).Add(middle, List.empty).Add(goal, List.empty)

    let newGame2 (numDiscs : int) : hanoi = 
        let makeDisc (i : int) : disc = i

        Map.empty.Add(start, List.empty).Add(middle, List.empty).Add(goal, List.init numDiscs (fun (i : int) -> makeDisc (numDiscs - i)))
    
    let isFinished (h : hanoi) = 
        let goalPeg = Map.find goal h
        let middlePeg = Map.find middle h
        let startPeg = Map.find start h


        let rec isDecreasing (list : disc list) (i : int) =
            match list[i]-1 = list[i+1] with
            | true -> 
                match i = 1 with
                | true -> true
                | false -> isDecreasing list (i+1)
            | false -> false

        match goalPeg.Length = 0 with
        | true -> false
        | false -> goalPeg[goalPeg.Length-1] = 1 && isDecreasing goalPeg 0 && startPeg.Length = 0 && middlePeg.Length = 0
    
    (* Question 4.2 *)
    
    type error =  
        | Empty of peg  
        | Invalid of peg * disc * disc
        
    let take (p : peg) (h : hanoi) : Result<disc * hanoi, error> = 
        let pegArray = Map.find p h

        match pegArray with
        | [] -> Error (Empty p)
        | d::ds-> Ok (d, Map.add p ds h)
    
    let place (p : peg) (d : disc) (h : hanoi) : Result<hanoi, error>  = 
        let list = Map.find p h

        match list.IsEmpty || list[list.Length-1] < d with
        | true -> Ok (Map.add p (d::list) h)
        | false -> Error(Invalid(p,list[list.Length-1],d))
    
    (* Question 4.3 *)
    
    type hanoiMonad<'a> = HM of (hanoi -> Result<'a * hanoi, error>)  
  
    let ret x = HM (fun h -> (Ok (x, h)))  
    let fail err = HM (fun _ -> Error err)  
    let bind f (HM a)  =  
        HM (fun h ->  
            match a h with  
            | Ok (x, h') ->  
                let (HM g) = f x  
                g h'        
            | Error err -> Error err)  
            
    let (>>=) a f = bind f a  
    let (>>>=) a b = a >>= (fun _ -> b)

    let evalHM l (HM f) = f (newGame l)
    
    let take2 (p : peg) : hanoiMonad<disc> = 
        HM (fun h ->
            match take p h with
            | Ok (d, h') -> Ok (d, h')
            | Error e -> 
                let (HM f) = fail e
                f h)
    
    let place2 (p : peg) (d : disc)  = 
        HM (fun h ->
            match place p d h with
            | Ok h' -> Ok ((), h')
            | Error e -> 
                let (HM f) = fail e
                f h)

    (* Question 4.4 *)
    
    type HanoiBuilder() =

        member this.Bind(f, x)    = bind x f
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Combine(a, b) = a >>= (fun _ -> b)

    let han = new HanoiBuilder()
    
    let move (fromPeg : peg) (toPeg : peg) = han {
        let! disc = take2 fromPeg
        return! place2 toPeg disc
    }

    
    let rec doMoves (list : (peg * peg) list) = han {
        match list with
        | [] -> return ()
        | (fromPeg, toPeg)::xs -> 
            do! move fromPeg toPeg
            return! doMoves xs
    }

    
    let rec solveHanoi size from via dest =
        match size with
        | 0 -> []
        | x -> solveHanoi (x - 1) from dest via @
               (from, dest) ::
               solveHanoi (x - 1) via from dest
               
    (* Question 4.5 *)
    
    let pstart = pstring "start" |>> (fun _ -> Start)  // your parser goes here
    let pmiddle = pstring "middle" |>> (fun _ -> Middle) // your parser goes here
    let pgoal = pstring "goal" |>> (fun _ -> Goal)  // your parser goes here
    
    let parsePeg = choice [pstart; pmiddle; pgoal] // your parser goes here
    let parseMove = parsePeg .>> pstring "->" .>>. parsePeg // your parser goes here
    let parseMoves = many (choice [parseMove .>> pstring ";"])// your parser goes here

     