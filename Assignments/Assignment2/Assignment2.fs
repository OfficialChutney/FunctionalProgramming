    module Assignment2

    let rec downto1 n =
        if n > 0 then
            n::downto1(n-1)
        else
            []
    
    let rec downto2 n =
        match n with
        | n when n <= 0 -> []
        | _ -> n::downto2(n-1)

    let rec downto3 = function
        | x when x <= 0 -> []
        | n -> n::downto3(n-1)

    let rec removeOddIdx = function 
        | x :: _ :: xs -> x :: removeOddIdx xs 
        | xs -> xs
        

    let rec combinePair = function
        | x :: y :: xs -> (x,y)::combinePair(xs)
        | _ -> []


    type complex = { 
        Re : float 
        Im : float 
    }

    let mkComplex a b = { Re = a; Im = b }
    let complexToPair ( c : complex ) = (c.Re, c.Im )
    let (|+|) (a : complex) ( b : complex ) = { Re = a.Re + b.Re; Im = a.Im + b.Im }
    let (|*|) (a : complex) (b : complex) = { Re = a.Re * b.Re - a.Im * b.Im; Im = a.Im * b.Re + a.Re * b.Im }
    let neg (a : complex) =  { Re = -a.Re; Im = -a.Im }
    let inv (a : complex) = { Re = a.Re / ( a.Re ** 2 + a.Im ** 2); Im = -a.Im / (a.Re ** 2 + a.Im ** 2)}
    let (|-|) (a : complex) (b : complex ) = a |+| neg(b)
    let (|/|) (a : complex) (b : complex ) = a |*| inv(b)

    let explode1 (s : string) = s.ToCharArray() |> List.ofArray

    let rec explode2 (s : string) =
        match s with
        | s when s.Length <= 0 -> []
        | s -> s.[0]::explode2(s.Remove(0,1))

    let rec implode (lst : char list) = 
        match lst with
        | [] -> ""
        | x::xs -> string(x) + implode(xs)


    let implodeRev lst = 
        let rec reverse = function 
            | [] -> []
            | x::xs -> reverse xs @ [x]  
        
        implode (reverse lst)
    
    let rec upperChars = function
        | [] -> []
        | x::xs -> (System.Char.ToUpper x)::upperChars(xs)
        

    let toUpper = explode1 >> upperChars >> implode

    let rec ack = function
        | (0,n) -> n + 1
        | (m,0) when m > 0 -> ack (m - 1, 1)
        | (m,n) when m > 0 && n > 0 -> ack (m - 1, ack(m, n - 1))
        | _ -> 0
    
    let rec reverse = function
        | [] -> []
        | x::xs -> reverse xs @ [x]


    let palindrome s = s = (explode1 s |> reverse |> implode)
        
    let rec keepLetters = function
        | [] -> []
        | x::xs when System.Char.IsAsciiLetter(x) -> x::keepLetters(xs)
        | x::xs -> keepLetters(xs)


    let palindrome2 s = (explode1 s |> upperChars |> keepLetters |> implode) = (explode1 s |> upperChars |> keepLetters |> reverse |> implode)
    
    let palindrome3 (s : string) = 

        let rec removeNonLetters (str : string) (index : int) =
            match str with
                | str when str.Length = 0 || index >= str.Length -> str
                | str when System.Char.IsAsciiLetter str.[index] = false -> removeNonLetters (str.Remove(index, 1)) (index)
                | str when index = (str.Length - 1) -> str
                | str -> removeNonLetters str (index + 1)
                
       
        let rec helper (str : string) (index : int) =
            match str with
            | str when index >= (str.Length / 2) - 1  -> true
            | str when System.Char.ToUpper str.[str.Length - index - 1] <> System.Char.ToUpper str.[index] -> false
            | _ -> helper str (index + 1)

        helper (removeNonLetters s 0) 0