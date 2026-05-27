module Assignment1

open System

// GREEN EXERCISES
// _________________________________________________________

// 1.1 - Write a function sqr : int -> int that given an integer x returns x squared    
   
let sqr x = x * x


// 1.2 - Write a function pow : float -> float -> float that given two floating point numbers x 
// and n returns x to the power of n
   
let pow x n = System.Math.Pow(x, n)


// 1.3 - Write a recursive function sum : int -> int such that given an integer n such that n >= 0 returns the sum of all integers from 0 to n inclusive.
// sum n = 0 + 1 + 2 + 3 + ... + n
// Hint: Use two clauses with 0 and n as patterns.
   
let rec sum n =
    match n with
    | 0 -> 0
    | n -> n + sum (n-1)


// 1.4 - Fibonacci Sequence *See description
// Write a function fib : int -> int that given an integer n such that n=>0 computes . 
// Use a declaration with three clauses, where the patterns correspond to the three cases of the above definition.
    
let rec fib n = 
    match n with
    | 0 -> 0
    | 1 -> 1
    | n -> fib (n-1) + fib (n-2)


// 1.5 - Write a function dup : string -> string that given a string s concatenates s with itself. You can use + to concatenate strings.
// Example: dup "Hi " = "Hi Hi "

let dup (s: string) = s + s


// 1.6 - Write a function dupn : string -> int -> string that given a string s and an integer n concatenates s with itself n times.
// Example: dupn "Hi " 3 = "Hi Hi Hi "

let rec dupn (s: string) (n: int) = 
    match n with
    | 0 -> ""
    | n -> s + dupn s (n-1)


// 1.7 - Pascal's Triangle *See description*
// Declare a function bin : int * int -> int that given a pair (n, k) computes (n / k).

let rec bin (n, k) =
    match (n, k) with
    | (n, 0) -> 1
    | (n, k) when n = k -> 1
    | (n, k) -> bin (n-1, k-1) + bin (n-1, k)


// 1.8 - Create a function readInt of type unit -> int that reads input from the console, line by line, until an integer is 
// received at which point that integer is returned. For every input that is not an integer the function must print <input> is not an integer, 
// where <input> is what the user entered into the console.

let readFromConsole () = System.Console.ReadLine().Trim()
let tryParseInt (str : string) = System.Int32.TryParse str

let rec readInt () =
    let input = readFromConsole ()
    let (success, result) = tryParseInt input
    if success then 
        result
    else
        printfn "%s" (input + " is not an integer")
        readInt ()

// For at teste om dette virker, kør koden -> skriv readInt ();; -> Herefter skriv enten et ord eller et tal og verificer at output er korrekt.


// YELLOW EXERCISES
// _________________________________________________________

(* 1.9 - Assume the time of day is represented as a pair (hh, mm) : int * int where hh represents the hour (a number betwen 0 and 23) and mm represents the minutes (a number between 0 and 59).
Write a function timediff : int * int -> int * int->int so that timediff t1 t2 computes the difference in minutes between t1 and t2, i.e., t2-t1. *)
   
// Note: Calculate the total number of minutes since midnight by hours * 60 + amount of minutes. 

let timediff (hh1, mm1) (hh2, mm2) =
    (hh2 * 60 + mm2) - (hh1 * 60 + mm1)


(* 1.10 - Write a function minutes : int * int -> int that computes the number of minutes since midnight. Hint: This is easily done using the function timediff.*)

let minutes (hh, mm) =
    timediff (00, 00) (hh, mm)


// RED EXERCISES
// _________________________________________________________

(* The curry function takes a function f of type 'a * 'b -> 'c as its first argument. This function f takes a single tuple (a, b), 
where a and b have the types 'a and 'b respectively, and returns a value of type 'c. The curry function then returns a function 
of type 'a -> 'b -> 'c that given two arguments a and b, applies them to f and returns the result. *)

let curry f x y =
    f (x, y)

(* The uncurry does the reverse. It takes a function f of type 'a -> 'b -> 'c as its first argument. This function f takes two arguments a and b,
that have the types 'a and 'b respectively, and returns a value of type 'c. The uncurry function then returns a function of type 'a * 'b -> 'c that
 given a tuple (a, b) takes a and b, applies them to f, and returns the result. *)

let uncurry f (x, y) =
    f x y