module Exam2024

open System
open JParsec
open JParsec.TextParser    
    
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
    
    type shape =
        | Rectangle of float * float
        | Circle of float
        | Triangle of float * float
         
    type shapeList =
        | Empty
        | AddShape of shape * shape * shapeList
        
    let rect = Rectangle(2., 3.)
    let circ = Circle 4.
    let trig = Triangle(2., 3.)
    let area (s: shape) : float = 
        match s with
        | Rectangle(w,h) -> w * h
        | Circle(r) -> System.Math.PI * System.Math.Pow(r,2)
        | Triangle (b,h) -> (b*h)/2.0
    
    let circumference (s: shape) = 
        match s with
        | Rectangle(w, h) -> 2.0*w + 2.0*h
        | Circle (r) -> 2.0 * System.Math.PI * r
        | Triangle (b, h) -> b + h + System.Math.Sqrt(System.Math.Pow(b,2) + System.Math.Pow(h,2))
        
    let rec totalArea (sl: shapeList) = 
        match sl with
        | Empty -> 0.0
        | AddShape(x, y, sl) -> area x + area y + totalArea sl
        
    let totalCircumference (sl: shapeList) = 
        let rec loop sl acc = 
            match sl with
            | Empty -> acc
            | AddShape(x, y, sl) -> loop sl (acc + circumference x + circumference y)
        loop sl 0
        
        
    let rec shapeListFold  (f: 'a -> shape -> 'a) acc (sl: shapeList) = 
        match sl with
        | Empty -> acc
        | AddShape(x,y,sl) -> shapeListFold f (f (f acc x)y) sl

    let isCircle =
        function
        | Circle _ -> true
        | _        -> false

    let containsCircle trs = 
        shapeListFold (fun acc c -> acc || isCircle c) false trs
        
    let totalArea2 (sl: shapeList) = shapeListFold (fun acc s -> acc + area s)0.0 sl


    let totalCircumference2 (sl: shapeList) = shapeListFold (fun acc s -> acc + circumference s) 0.0 sl
    
(* 2: Code Comprehension *)
        
    let foo =
        function
        | c when Char.IsWhiteSpace c -> c 
        | c when c > 'w'             -> char (int c - 23)
        | c when c < 'x'             -> char (int c + 3)
        
    let bar (str : string) = [for c in str -> c]
    
    let baz str =
        let rec aux = 
            function
            | [] -> ""
            | c :: cs -> string (foo c) + (aux cs)
            
        aux (bar str)
    
(* Question 2.1 *)

    (* 
    
    Q: What are the types of functions foo, bar, and baz?

    A:  foo: char -> char
        bar: string -> list<char>
        baz: string -> string

    Q: What do the function foo, bar, and baz do.
       Focus on what they do rather than how they do it.

    A:  foo: returning a character of a location (taking the idexnumber of a specific char and either adding 3 or subtracting 23 to get a new location and take the char from that)
        bar: making a string into a list of characters  
        baz: encrypting each character in the string
    
    Q: What would be appropriate names for functions 
       foo, bar, and baz?

    A:  foo: changeCharWithOffset
        bar: stringToStringList
        baz: encryptString
        
*)
    
(* Question 2.2 *)
    (*The function generates a warning about incomplete pattern match because compiler cannot make sure that every case is covered*)

    let foo2 =
        function
        | c when Char.IsWhiteSpace c -> c 
        | c when c > 'w'             -> char (int c - 23)
        | c when c < 'x'             -> char (int c + 3)
        | c -> c

(* Question 2.3 *)
    
    let baz2 (str: string) = str |> List.ofSeq |> List.fold (fun acc x -> acc + string (foo x) ) ""

    
(* Question 2.4 *)

    (*

    Q: The function `baz` from Question 2.1 is not tail recursive. Demonstrate why.
       To make a compelling argument you should evaluate a function call of the function,
       similarly to what is done in Chapter 1.4 of HR, and reason about that evaluation. 
       You need to make clear what aspects of the evaluation tell you that the function 
       is not tail recursive. Keep in mind that all steps in an evaluation chain must 
       evaluate to the same value ( (5 + 4) * 3 --> 9 * 3 --> 27 , for instance).
       
       You do not have to step through the foo- or the bar functions. 
       You are allowed to evaluate these function immediately.
       
    A: 

    *)
    
(* Question 2.5 *)
    
    let bazTail (str: string)  = 
        let rec aux str c =
            match str with 
            | x :: xs -> aux xs (fun r -> c (string(foo x) + r))
            | [] -> c "" 
        aux (List.ofSeq str) (fun x -> x)


(* 3: Atbash Ciphers *)

(* Question 3.1 *)
    (*let encrypt (text: string) (offset: int) = String.map (fun c -> if c = ' ' then ' ' else char ((int c - int 'a' + offset) % 26 + int 'a')) text*)
    let encrypt (text: string) = String.map (fun c -> if System.Char.IsWhiteSpace c then c else char (int 'z' - (int c - int 'a'))) text
        
(* Question 3.2 *)

    let decrypt (text: string) = String.map (fun c -> if c = ' ' then ' ' else char (int 'z' - ((int c - int 'a') % 26))) text
    (*let decrypt(text: string) (offset: int)  = String.map (fun c -> if c = ' ' then ' ' else char ((int c - int 'a' - (offset % 26) + 26) % 26 + int 'a')) text*)
(* Question 3.3 *)

    let splitAt (i: int) (str: string) = str |> Seq.chunkBySize i |> Seq.map (fun chars -> System.String(chars)) |> Seq.toList
    
(* Question 3.4 *)
    
    let parEncrypt (str: string) (i: int) = str |> splitAt i |> List.toArray |> Array.Parallel.map encrypt |> String.concat ""
    
(* Question 3.5 *)

    let parseEncrypt : Parser<string> = 
        many (satisfy (fun c -> c = ' ' || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')))
            |>> (fun chars -> 
            let str = System.String (Array.ofList(List.map Char.ToLower chars))
            encrypt str)

    (*let parseEncrypt (offset: int) : Parser<string> = 
        many (satisfy (fun c -> c = ' ' || (c >= 'a' && c <= 'z')))
        |>> (fun chars -> 
        let str = System.String(Array.ofList chars)
        encrypt str offset)
    *)
    
(* 4: Letterboxes *)
    
(* Question 4.1 *)
    
    type clicker = char list * int list // insert your own type here
    
    let newClicker (wheel: char list) (numWheels: int) : clicker = (wheel, List.replicate numWheels 0)
        
(* Question 4.2 *)
    (*  Recursively increment from the right by going to the end of the list first
        If the rightmost position wrapped to 0, carry to the left
        Otherwise leave the current position unchanged *)
    let click (cl: clicker) =
        let (wheel, positions) = cl
        let size = List.length wheel
        let rec carry positions =
            match positions with
            | [] -> []
            | [x] -> [(x + 1) % size]
            | x :: xs ->
                let newXs = carry xs
                if List.head newXs = 0 then (x + 1) % size :: newXs
                else x :: newXs
        (wheel, carry positions)
        
          
        
    let read (cl: clicker) = 
        let (wheel, positions) = cl
        List.map (fun pos -> List.item pos wheel) positions |> Array.ofList |> System.String

    let cl = newClicker ['a'; 'b'; 'c'] 2


(* Question 4.3 *)
    type StateMonad<'a> = SM of (clicker -> 'a * clicker)  
      
    let ret x = SM (fun cl -> (x, cl))
    
    let bind f (SM a) : StateMonad<'b> =
        SM (fun cl ->
               let x, cl'  = a cl
               let (SM g) = f x
               g cl')
          
    let (>>=) x f = bind f x  
    let (>>>=) x y = x >>= (fun _ -> y)  
      
    let evalSM cl (SM f) = f cl
    
    let click2 : StateMonad<Unit>  = 
        SM (fun cl -> ((), click cl))

    
    let read2 : StateMonad<string> = 
        SM(fun cl -> (read cl, cl))


(* Question 4.4 *)
    
    let multipleClicks (x: int) : StateMonad<string list> = 
        let rec loop n=
            if n = 0 then
                ret []                                   // base case — no more reads needed
            else
                read2        >>= fun s  ->      // read current state
                click2       >>>=                        // advance clicker
                loop (n - 1) >>= fun rest ->    // recurse for remaining clicks
                ret (s :: rest)                          // prepend this read to the list
        read2    >>= fun s  ->                   // read initial state (before any clicks)
        click2   >>>=                                    // first click
        loop (x - 1) >>= fun rest ->        // x-1 more reads
        ret (s :: rest)


(* Question 4.5 *)

    type StateBuilder() =

        member this.Bind(f, x)    = bind x f
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Combine(a, b) = a >>= (fun _ -> b)

    let state = StateBuilder()
    
    let multipleClicks2 (x: int) : StateMonad<string list> = 
        let rec loop n =
            if n = 0 then
                state { return [] }
            else
                state { let!  s    = read2                   // read current state
                        do!          click2                           // advance (unit, so do! not let!)
                        let!  rest  = loop (n - 1)      // recurse
                        return (s :: rest) }                          // build list on unwind
        loop x