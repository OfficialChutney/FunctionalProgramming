module Assignment2

// GREEN EXERCISES
// _________________________________________________________

// 2.1 Write a function downto1 : int -> int list that given an integer n returns the n-element list [n; n-1; ...; 1] if n > 0 and [] otherwise. 
// You must use if-then-else expressions to define the function.
let rec downto1 n =
    if n > 0 then n :: downto1(n-1)
    else []

// Secondly define the function downto2 which behaves in exactly the same way as downto1. This time you must use pattern matching using match.
let rec downto2 n =
    match n with
    | n when n > 0 -> n :: downto2 (n - 1)
    | _ -> []

// Finally, define the function downto3 which behaves in exactly the same way as downto1 and downto2. This time you you must use pattern matching using function.
let rec downto3 =
    function
    | n when n > 0 -> n :: downto3 (n - 1)
    | _ -> []

// 2.2 - Write a function removeOddIdx : 'a list -> 'a list that given a list xs returns a list where all odd-indexed elements of xs have been removed.
let rec removeOddIdx xs = 
    match xs with
    | [] -> []
    | [x] -> [x]
    | x :: _ :: rest -> x :: removeOddIdx rest


// 2.3 - Write a function combinePair : 'a list -> ('a * 'a) list that given a list xs returns the list with elements from xs combined into pairs. 
// If xs contains an odd number of elements, then the last element is thrown away. Hint: Use pattern matching
let rec combinePair xs =
    match xs with
    | [] -> []
    | [x] -> []
    | x :: y :: rest -> (x, y) :: combinePair rest


// 2.4 Define a type complex that represents complex numbers with floating point components.
type complex = float * float

// Define a function mkComplex : float -> float -> complex that given two floating point numbers return the corresponding complex number.
// Note: Complex numbers are numbers with two parts. C = a + bi, where a is the real part and bi is the imaginary part

let mkComplex (a: float) (b: float) :  complex = (a, b)

// Define a function complexToPair : complex -> float * float that given a complex number (a,b) returns the pair (a, b).
let complexToPair (c: complex) : float * float = c


// Addition: (a, b) + (c,d) = (a + c, b + d)
let (|+|) (c1: complex) (c2: complex) : complex =
    let (a, b) = c1
    let (c, d) = c2
    (a + c, b + d) 

// Multiplication: (a,b) * (c,d) = (ac - bd, bc + ad)
let (|*|) (c1: complex) (c2: complex) : complex =
    let (a, b) = c1
    let (c, d) = c2
    let realPart = a * c - b * d
    let imagPart = b * c + a * d
    (realPart, imagPart)

// Subtraction: -(a,b) = (-a, -b)
let (|-|) (c1: complex) (c2: complex) : complex =
    let (c, d) = c2
    c1 |+| (-c, -d)

// Division
let (|/|) (c1: complex) (c2: complex) : complex =
    let (c, d) = c2
    let denominator = c * c + d * d
    let inverse = (c / denominator, -d / denominator)
    c1 |*| inverse


// 2.5 - Write a non-recursive function explode1 : string -> char list that given a string s returns the list of characters in s.
let explode1 (s: string) = s.ToCharArray() |> List.ofArray


// Write a recursive function explode2 : string -> char list that behaves the same as explode except that you now have to use the
// string function s.Chars (or .[index]), where s is a string. You can also make use of s.Remove(0,1)

let rec explode2 (s: string) =
    match s with
    | "" -> []
    | _ -> s.[0] :: explode2 (s.Remove(0,1))

// 2.6 - Write a function implode : chat list -> string that given a list of characters cs returns a string with all characters of cs in the same order.

let implode (cs: char list) : string = System.String(List.toArray cs)

// Write a function implodeRev : char list -> string that given a list of characters cs returns a string with all characters of cs in reverse order.
// Do not use List.rev

let rec implodeRev (cs: char list) : string =
    match cs with
    | [] -> ""
    | x :: xs -> implodeRev xs + string x


// 2.7 - Write a function toUpper : string -> string that given a string s returns s with all characters in upper case.

// Helping function:
let rec toUpperChars (cs: char list) : char list =
    match cs with
    | [] -> []
    | x :: xs -> System.Char.ToUpper x :: toUpperChars xs

let toUpper (s: string) : string =
    implode (toUpperChars (explode1 s))

// 2.8 - Write the function ack : int * int -> int that given an integer pair (m, n) implements the Ackermann function using pattern matching on the cases of A(m,n) as given below.

let rec ack (m, n) =
    match (m, n) with
    | (0, n) -> n + 1
    | (m, 0) when m > 0 -> ack (m - 1, 1)
    | (m, n) when m > 0 && n > 0 -> ack (m - 1, ack (m, n - 1))
    | _ -> failwith "Ackermann is only defined for non-negative integers"

// YELLOW EXERCISES
// _________________________________________________________

// 2.9 - Create a function reverse that given a list lst returns lst reversed. Your function must not use any auxiliary, internal, or standard library functions.
let rec reverse lst =
    match lst with
    | [] -> []
    | x :: xs -> reverse xs @ [x]

// 2.10 - Create a funciton palindrome of type string -> bool that given a string str returns true if str is the same forwards as backwards.

let palindrome (str: string) : bool =
    explode1 str = reverse (explode1 str)

// 2.11 - Create a function keepLetters of type char list -> char list that given a list of characters lst removes all characters in lst that
// are not a letter in the alphabet according to the standard library function System.Char.IsAsciiLetter. You may not use any auxiliary, internal,
// or standard library functions (other than System.Char.IsAsciiLetter).

let rec keepLetters (lst: char list) : char list =
    match lst with
    | [] -> []
    | x :: xs ->
        if System.Char.IsAsciiLetter x then
            x :: keepLetters xs
        else
            keepLetters xs

// 2.12 - Create a function palindrome2 that works exactly the same as palindrome but that treats lower- and upper case characters the same and 
// ignores non-letters, according to the System.Char.IsAsciiLetter function from the standard library.

let palindrome2 (str: string) : bool =
    let letters = keepLetters (explode1 (toUpper str))
    letters = reverse letters

// RED EXERCISES
// _________________________________________________________

// 2.13 - Create a function palindrome3 that works exactly like palindrome2 but that.. 

let palindrome3 (str: string) : bool =
    let rec check i j =
        if i >= j then
            true
        elif not (System.Char.IsAsciiLetter str.[i]) then
            check (i + 1) j
        elif not (System.Char.IsAsciiLetter str.[j]) then
            check i (j - 1)
        elif System.Char.ToUpper str.[i] <> System.Char.ToUpper str.[j] then
            false
        else
            check (i + 1) (j - 1)

    check 0 (str.Length - 1)