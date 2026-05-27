module Assignment3
    
// GREEN EXERCISES
// _________________________________________________________

// Exercise 3.1

// Create a function add5 : int -> int, that given an integer x adds 5 to x.
let add5 x = x + 5

// Create a function mul3 : int -> int, that given an integer x multiplies x by 3.
let mul3 x = x * 3

// Create a function add5mul3 : int -> int that given an integer x first adds 5 to x and then multiplies the result by 3. 
// You must use piping (|>).
let add5mul3 x = (x |> add5) |> mul3

// Create a function add5mul3_2 : int -> int that behaves exactly like add5mul3. 
// You must use function composition.
let add5mul3_2 x = (add5 >> mul3) x
    
// Exercise 3.2

// Create a function add5_2 : ('a -> int) -> a' -> int that given a function f : a' -> int 
// and a value x of type 'a returns f x plus 5.
let add5_2 f x =  f x + 5

// Test this function by running 
// > add5_2 String.lenght "hello";;
// > add5_2 (fun x -> x * 3) 7

// Create a function mul3_2 : ('a -> int) -> 'a int that given a function f : 'a -> int
// and a value x of type 'a returns f x multiplied by 3
let mul3_2 f x = f x * 3
   
// Exercise 3.3 

// Declare a function downto4 : (int -> 'a -> 'a) -> int -> 'a -> 'a such that (see description)
// Test this by running the addNumber function:
//let addNumber i acc = i + acc
// > downto4 addNumber 4 0;; 
// Expected output = 10
let rec downto4 f n e =
    match n with
    | n when n <= 0 -> e
    | _ -> downto4 f (n - 1) (f n e)


// Declare the factorial function fac : int -> int by use of downto4. The factorial function
// is defined as follows: 
// !0 = 1
// !n = n * !(n-1)
let fac n =
    downto4 (fun i acc -> i * acc) n 1


// Use downto4 to declare a function range : (int -> 'a) -> int -> 'a list that given a function g and
// an integer n returns the list of [g 1, g 2, ..., g n] if n is positive, and the empty list otherwise.

// Test this by running range with square
// let square x = x * x 
// > range square 5;; 
// Should do: [square 1; square 2; square 3; square 4; square 5]
// Expected output = [1; 4; 9; 16; 25]
let range g n =
    downto4 (fun i acc -> g i :: acc) n []

// Challenge: An interesting solution to downto4 makes use of partial application of the f function and
// function composition with the recursive call. Try it out if you like (it is not mandatory). The id function
// (which is just fun x -> x) may come in handy in that case.

(* 
let rec downto4 f n =
    match n with
    | n when n <= 0 -> id
    | _ -> f n >> downto4 f (n - 1)
*) 

// Exercise 3.4
// Create a recursive function, without using any higher-order functions, double : int list -> int list 
// that given an integer list lst returns a list with all elements in lst doubled.
let rec double lst = 
    match lst with
    | [] -> []
    | x :: xs -> (x * 2) :: double xs

// Create a non-recursive function, with a single use of map, double_2 : int list -> int list
// that behaves exactly the same as double.
let double_2 lst = List.map (fun x -> x * 2) lst

// Exercise 3.5
// Create a recursive function, without using any higher-order functions, stringLength : string list
// -> int list that given a string list lst returns a list with the lenght of all elements in lst.    
let rec stringLength lst = 
    match lst with
    | [] -> []
    | x :: xs -> String.length x :: stringLength xs

// Man skal skrive String.length og ikke x.length that de er for brede.

// Create a non-recursive function, with a single use of map, stringLength_2 : string list ->
// int list that behaves exactly the same as stringLength
let stringLength_2 lst = List.map (fun x -> String.length x) lst


// Exercise 3.6
// Create a recursive function, without using any higher-order functions, keepEven : int list ->
// int list that given an integer list lst returns a list containing all elements of lst that are even.
let rec keepEven lst = 
    match lst with
    | [] -> []
    | x :: xs when x % 2 = 0 -> x :: keepEven xs
    | _ :: xs -> keepEven xs

// Create a non-recursive function, with a single use of filter, keepEven_2 : int list -> int list
// that behaves exactly the same as keepEven    
let keepEven_2 lst = List.filter (fun x -> x % 2 = 0) lst
    
// Exercise 3.7
// Create a recursive function, without using any higher-order functions, keepLengthGTH : string list -> string list
// that given an string list lst returns a list containing all elements of lst that have length strictly greater than 5.
let rec keepLengthGT5 lst =
    match lst with
    | [] -> []
    | x :: xs when String.length x > 5 -> x :: keepLengthGT5 xs
    | _ :: xs -> keepLengthGT5 xs

// Create a non-recursive function, with a single use of filter, keepLengthGT5_2 : string list -> string list 
// that behaves exactly the same as keepLengthGT5.
let keepLengthGT5_2 lst = List.filter (fun x -> String.length x > 5) lst

// Important: Take a long look at the four functions from Q3.6 and Q3.7 and see if you can spot the patterns 
// between the recursive and the higher-order variants of the functions. 


// Exercise 3.8
// Create a recursive function, without using any higher-order functions, sumPositive : int list -> int that given an
// integer list lst sums all positive elements in lst 
let rec sumPositive lst =
    match lst with
    | [] -> 0
    | x :: xs when x > 0 -> x + sumPositive xs
    | _ :: xs -> sumPositive xs
        
// Create a non-recursive function, with a single use of fold, sumPositive_2 : int list -> int 
// that behaves exactly the same as sumPositive  
let sumPositive_2 (lst : int list) =
    List.fold
        (fun acc x ->
            match x with
            | x when x > 0 -> acc + x
            | _ -> acc)
        0
        lst

// Create a non-recursive function sumPositive_3 : int list -> int, that behaves exactly the same as
// sumPositive and sumPositive_2, but that removes all negative elements from lst using filter and
// then sums them using fold. Your folding function should be much simpler than the one you needed 
// for sumPositive_2 since all negative elements are already gone.
let sumPositive_3 lst = 
    lst
    |> List.filter (fun x -> x > 0)
    |> List.fold (fun acc x -> acc + x) 0
        
// Piping works in this case since it is "value |> function" so the value lst is applied to both functions.


// YELLOW EXERCISES
// _________________________________________________________

// Exercise 3.9
// Create a function add5mul3_3 : ('a -> int) -> 'a -> int that given a function f : 'a -> int
// and a value x of type 'a returns f x plus 5 and then multiplies the result by 3. 
// You must use the add5_2 and mul3_2 functions from E3.2 and either piping or function composition.

// Function composition solution
let add5mul3_3 f x =
    (add5_2 f >> mul3_2 id) x

// Piping solution
let add5mul3_4 f x =
    x
    |> add5_2 f
    |> mul3_2 id

// Explanation:
// f is a function given as input, e.g. String.length.
// add5_2 f x applies f to x and adds 5.
//
// id means: fun x -> x
// It just returns the same value again.
//
// We use id with mul3_2 because after add5_2
// we already have an int that just needs to be multiplied by 3.
//
// x |> add5_2 f |> mul3_2 id
// means: apply f to x, add 5, then multiply by 3.


// Exercise 3.10
// Create a non-recursive function mergeFuns : ('a -> 'a) list -> 'a -> 'a that given a list of functions fs
// returns a function that is the composition of all functions in fs.
let mergeFuns fs = 
    List.fold (fun acc f -> acc >> f) id fs


// Exercise 3.11
// Create a non-recursive function, using a single instance of fold, removeOddIdx : 'a list -> 'a list
// that behaves exactly the same as removeOddIdx from E2.2 in Assignment 2.
let removeOddIdx (xs: 'a list) =
    let (result, _) =
        List.fold
            (fun (acc, index) x ->
                match index % 2 with
                | 0 -> (acc @ [x], index + 1)
                | _ -> (acc, index + 1))
            ([], 0)
            xs

    result

// @ means list append and essentially joins two list together.

(* 
2.2 - Write a function removeOddIdx : 'a list -> 'a list that given a list xs returns a list where all odd-indexed elements of xs have been removed.
let rec removeOddIdx xs = 
    match xs with
    | [] -> []
    | [x] -> [x]
    | x :: _ :: rest -> x :: removeOddIdx rest
*)


// RED EXERCISES
// _________________________________________________________

// Exercise 3.12
// Create a function facFuns : int -> (int -> int) list that given an integer x returns a list of functions 
// that, when combined, become the factorial function. More precisely,
// facFuns x = [fun y -> x * y; fun y -> x -  1 * y ...; fun y -> 1 * y]

// Hint: You can make this function recursive, but you can also use downto4.
let rec facFuns x = 
    match x with
    | 0 -> []
    | x -> (fun y -> x * y) :: facFuns (x - 1)


// Create a non-recursive function fac_2 : int -> int, using mergeFuns and facFuns, that given an 
// integer x returns the factorial of x.
let fac_2 (x : int) = mergeFuns (facFuns x) 1


// Exercise 3.13
// Create a non-recursive function weird : string list -> string that given a list of strings strs
// removes all strings of odd length, calculates the lengths of each individual string, and appends 
// these lengths in order into a new string

let weird (strs: string list) =
    strs
    |> List.filter (fun s -> s.Length % 2 = 0)
    |> List.map (fun s -> string s.Length)
    |> String.concat ""


// Exercise 3.14
// Create a function insert : 'a -> 'a list -> 'a list list that given an element x and a list xs
// returns a list of lists that contains all possible ways to insert x into xs.
let insert x xs =
    let rec insertHelper before after =
        match after with
        | [] ->
            [List.rev (x :: before)]
        | y :: ys ->
            (List.rev before @ (x :: after)) :: insertHelper (y :: before) ys

    insertHelper [] xs

// Create a function permutations : 'a list -> 'a list list that given a list lst returns a list
// containing all possible permutations of lst.
let rec permutations lst =
    match lst with
    | [] -> [[]]
    | x :: xs ->
        permutations xs
        |> List.collect (fun perm -> insert x perm)