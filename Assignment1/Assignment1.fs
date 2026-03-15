module Assignment1

    open System
    
    let sqr x = x * x

    let pow x n = System.Math.Pow(x, n)

    let rec fib = function
    | 0 -> 0
    | 1 -> 1
    | n -> fib(n-1) + fib(n-2)

    let rec sum n =
        match n with
        | n when n <= 0 -> 0
        | _ -> n + sum(n - 1)

    let dup s = (string s) + (string s)

    let rec dupn s n =
        match n with
        | n when n <= 0 -> ""
        | _ -> s + dupn s (n - 1)
    
    let readFromConsole () = System.Console.ReadLine().Trim()
    let tryParseInt (str : string) = System.Int32.TryParse str
    
    let rec readInt () =
        let input = readFromConsole ()
        match tryParseInt input with
        | (true, value) -> value
        | (false, _) ->
            printfn "%s" (input + " is not an integer")
            readInt ()

    let rec bin (n, k) =
        match (n, k) with
        | (_, 0) -> 1
        | (n, k) when n = k -> 1
        | (n, k) when k < 0 || k > n -> 0
        | (n, k) -> bin (n - 1, k - 1) + bin (n - 1, k)



    let timediff (hh1, mm1) (hh2, mm2) = (hh2 - hh1) * 60 + (mm2 - mm1);;

    let minutes (hh, mm) = timediff (00,00) (hh, mm)

    let curry f a b = f(a,b)

    let uncurry (f : ('a -> 'b -> 'c)) (a : 'a ,b : 'b ) = f a b

    let uncurry2 f (a,b) = f a b