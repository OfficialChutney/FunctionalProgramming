module Interpreter.Eval

// GREEN EXERCISES
// _________________________________________________________

    open Result
    open Language

// Exercise 4.1
// Create a function aexprToString of type aexpr -> string that given an arithmetic expression a
// returns a string version of a where all sub-expressions except numbers are parethesised and
// there is a space between the infix operators and the operands.
    

    let rec aexprToString aexpr = 
        match aexpr with
        | Num n -> string n
        | Add (a, b) -> sprintf "(%s + %s)" (aexprToString a) (aexprToString b)
        | Mul (a, b) -> sprintf "(%s * %s)" (aexprToString a) (aexprToString b)
        | Div (a, b) -> sprintf "(%s / %s)" (aexprToString a) (aexprToString b)

// Exercise 4.2
// Create a function bexprToString of type bexpr -> string that given an boolean expression b
// returns a string version of b where all sub-expressiongs except TT are parethesised and 
// there is a space between the infix operators and the operands and
// TT is translated to true
// Eq is translated to =
// Lt is translated to <
// Conj is translated to /\
// Not is translated to not

    let rec bexprToString bexpr =
        match bexpr with
        | TT -> "true"
        | Eq (a, b) -> sprintf "(%s = %s)" (aexprToString a) (aexprToString b)
        | Lt (a, b) -> sprintf "(%s < %s)" (aexprToString a) (aexprToString b)
        | Conj (a, b) -> sprintf "(%s /\\ %s)" (bexprToString a) (bexprToString b)
        | Not b -> sprintf "(not %s)" (bexprToString b)


// Exercise 4.3
// Create a function aexprEval of type aexpr -> int option that given an arithmetic expression
// a pattern matches on a and, without using Option.bind, returns
// Some x if a is equal to Num x
// Some (x + y) if a is equal to Add(b, c) and b evaluates to Some x and c evaluates to Some y.
// Some (x * y) if a is equal to Mul(b, c) and b evaluates to Some x and c evaluates to Some y.
// Some (x / y), where / is integer division, if a is equal to Div(b, c) and b evaluates to Some x and c evaluates to Some y, where y is not equal to 0.
// None otherwise

    let rec aexprEval aexpr =
        match aexpr with
        | Num x -> Some x
        | Add (b, c) -> 
            match aexprEval b, aexprEval c with
            | Some x, Some y -> Some (x + y)
            | _ -> None
        | Mul (b, c) ->
            match aexprEval b, aexprEval c with
            | Some x, Some y -> Some (x * y)
            | _ -> None
        | Div (b, c) ->
            match aexprEval b, aexprEval c with
            | Some x, Some y when y <> 0 -> Some (x / y)
            | _ -> None


// Exercise 4.4
// Create a function aexprEval2 that behaves exactly the same as aexprEval but which uses Option.bind. You may not pattern match on options in any way, 
// such as the ones obtained by using recursive calls to aexprEval2, and you may not use aexprEval.

    let rec aexprEval2 aexpr =
        match aexpr with
        | Num x -> Some x
        | Add (b, c) ->
            aexprEval2 b
            |> Option.bind (fun x ->
                aexprEval2 c
                |> Option.bind (fun y ->
                    Some (x + y)))
        | Mul (b, c) ->
            aexprEval2 b
            |> Option.bind (fun x ->
                aexprEval2 c
                |> Option.bind (fun y ->
                    Some (x * y)))
        | Div (b, c) ->
            aexprEval2 b
            |> Option.bind (fun x ->
                aexprEval2 c
                |> Option.bind (fun y ->
                    if y <> 0 then
                        Some (x / y)
                    else
                        None))


// Exercise 4.5
// Create a function bexprEval of the type bexpr  -> bool option that given a boolean expresion b returns,
// using aexprEval or aexprEval2 where necessary, returns
// Some true if b is equal to TT
// Some(x = y) if b is equal to Eq(a, c) and a evaluates to Some x and c evaluates to Some y.
// Some(x < y) if b is equal to Lt(a, c) and a evaluates to Some x and c evaluates to Some y.
// Some(x && y) if b is equal to Conj(a, c) and a evaluates to Some x and c evaluates to Some y.
// Some(not x) if b is equal to Not a and a evaluates to Some x.
// None otherwise

    let rec bexprEval bexpr =
        match bexpr with
        | TT -> Some true
        | Eq(a, c) -> 
            match aexprEval a, aexprEval c with
            | Some x, Some y -> Some (x = y)
            | _ -> None
        | Lt(a, c) ->
            match aexprEval a, aexprEval c with
            | Some x, Some y -> Some (x < y)
            | _ -> None
        | Conj (a, c) -> 
            match bexprEval a, bexprEval c with
            | Some x, Some y -> Some (x && y)
            | _ -> None
        | Not a -> 
            match bexprEval a with
            | Some x -> Some (not x)
            | _ -> None

// Exercise 4.6 (Long text - See description)
// Create a function aexprToString2 that behaves exactly the same as aexprToString but which 
// only includes the parentheses that are strictly necessary.

    let aexprToString2 aexpr =

        let parenthesiseIf condition str =
            if condition then sprintf "(%s)" str
            else str

        let rec helper aexpr =
            match aexpr with
            | Num n ->
                (0, string n)

            | Add (a, b) ->
                let (levelA, strA) = helper a
                let (levelB, strB) = helper b

                let left = parenthesiseIf (2 < levelA) strA
                let right = parenthesiseIf (2 < levelB) strB

                (2, sprintf "%s + %s" left right)
                 
            | Mul (a, b) ->
                let (levelA, strA) = helper a
                let (levelB, strB) = helper b

                let left = parenthesiseIf (1 < levelA) strA
                let right = parenthesiseIf (1 < levelB) strB
 
                (1, sprintf "%s * %s" left right)

            | Div (a, b) ->
                let (levelA, strA) = helper a
                let (levelB, strB) = helper b

                let left = parenthesiseIf (1 < levelA) strA
                let right = parenthesiseIf (1 <= levelB) strB
    
                (1, sprintf "%s / %s" left right)

        let (_, str) = helper aexpr
        str


// Exercise 4.7
// Create a function bexprToString2 that behaves exactly the same as bexprToString 
// but which only includes the parentheses that are strictly necessary.

    let bexprToString2 bexpr =

        let parenthesiseIf condition str =
            if condition then sprintf "(%s)" str
            else str

        let rec helper bexpr =
            match bexpr with
            | TT -> (0, "true")

            | Eq (a, b) ->
                (2, sprintf "%s = %s" (aexprToString a) (aexprToString2 b))

            | Lt (a, b) ->
                (2, sprintf "%s < %s" (aexprToString a) (aexprToString b))

            | Conj (a, b) ->
                let (levelA, strA) = helper a
                let (levelB, strB) = helper b

                let left = parenthesiseIf (3 < levelA) strA
                let right = parenthesiseIf (3 < levelB) strB

                (3, sprintf "%s /\\ %s" left right)

            | Not a ->
                let (levelA, strA) = helper a

                let inside = parenthesiseIf (1 <= levelA) strA

                (1, sprintf "not %s" inside)

        let (_, str) = helper bexpr
        str


// YELLOW EXERCISES
// _________________________________________________________

// All error handling up to this point has been handled through the option type, which either gives you an answer or tells you that something went wrong but you don't know what.

// Exercise 4.8
// Solve Question 4.1 from the green exercises.

// This is solved.

// Exercise 4.9
// Solve Question 4.2 from the green exercises.

// This is solved.
    
// Exercise 4.10
// Refactor aexprEval from Question 4.3 so that it returns DivisionByZero if the user attempts by divide by zero. 
// In 4.3 you were not allowed to use Option.bind, and similarly, you must not use Result.bind here.

    let rec aexprEval3 aexpr =
        match aexpr with
        | Num x -> Ok x

        | Add (b, c) -> 
            match aexprEval3 b, aexprEval3 c with
            | Ok x, Ok y -> Ok (x + y)
            | Error e, _ -> Error e
            | _, Error e -> Error e

        | Mul (b, c) ->
            match aexprEval3 b, aexprEval3 c with
            | Ok x, Ok y -> Ok (x * y)
            | Error e, _ -> Error e
            | _, Error e -> Error e

        | Div (b, c) ->
            match aexprEval3 b, aexprEval3 c with
            | Ok _, Ok 0 -> Error DivisionByZero
            | Ok x, Ok y -> Ok (x / y)
            | Error e, _ -> Error e
            | _, Error e -> Error e


// Exercise 4.11 
// Create a function aexprEval4 that behaves exactly like aexprEval but which uses Result.bind. You may not pattern
// match on results in any way, such as the ones obtained by using recursive calls to aexprEval4, and you may not use aexprEval.

    let rec aexprEval4 aexpr =
        match aexpr with
        | Num x -> Ok x
        | Add (b, c) ->
            aexprEval4 b
            |> Result.bind (fun x ->
                aexprEval4 c
                |> Result.bind (fun y ->
                    Ok (x + y)))
        | Mul (b, c) ->
            aexprEval4 b
            |> Result.bind (fun x ->
                aexprEval4 c
                |> Result.bind (fun y ->
                    Ok (x * y)))
        | Div (b, c) ->
            aexprEval4 b
            |> Result.bind (fun x ->
                aexprEval4 c
                |> Result.bind (fun y ->
                    if y <> 0 then
                        Ok (x / y)
                    else
                        Error DivisionByZero))

// Exercise 4.12
// Refactor bexprEval from 4.5 to return a Result<bool, Error> rather than a bool option. 
// Boolean expressions cannot generate errors on their own, but you must forward anyone
// generated by aexprEval (or aexprEval2)

    let rec bexprEval2 bexpr = 
        match bexpr with
        | TT -> Ok true
        | Eq(a, c) -> 
            match aexprEval3 a, aexprEval3 c with
            | Ok x, Ok y -> Ok (x = y)
            | Error e, _ -> Error e
            | _, Error e -> Error e
        | Lt (a, c) ->
            match aexprEval3 a, aexprEval3 c with 
            | Ok x, Ok y -> Ok (x < y)
            | Error e, _ -> Error e
            | _, Error e -> Error e
        | Conj (a, c) -> 
            match bexprEval2 a, bexprEval2 c with
            | Ok x, Ok y -> Ok (x && y)
            | Error e, _ -> Error e
            | _, Error e -> Error e
        | Not a ->
            match bexprEval2 a with
            | Ok x -> Ok (not x)
            | Error e -> Error e


// Exercise 4.13
// Solve Question 4.6 from the green exercises.

// This is solved.

// Exercise 4.14
// Solve Question 4.7 from the green exercises.

// This is solved.


// RED EXERCISES
// _________________________________________________________

// Exercise 4.15 (See description)
// Create a function aexprFold of type (see description) that given a folding function fnum for numbers,
// fadd for additions, fmul for multiplications, and fdiv for division, and an arithmetic expression a, returns

// fnum v, if a is equal to Num a
// fadd acc1 acc2, if a is equal to Add(e1, e2) and folding e1 computes acc1 and folding e2 computes acc2
// fmul acc1 acc2, if a is equal to Mul(e1, e2) and folding e1 computes acc1 and folding e2 computes acc2
// fdiv acc1 acc2, if a is equal to Div(e1, e2) and folding e1 computes acc1 and folding e2 computes acc2

    let rec aexprFold fnum fadd fmul fdiv aexpr =
        match aexpr with
        | Num v -> fnum v
        | Add (e1, e2) ->
            fadd
                (aexprFold fnum fadd fmul fdiv e1)
                (aexprFold fnum fadd fmul fdiv e2)
        | Mul (e1, e2) ->
            fmul
                (aexprFold fnum fadd fmul fdiv e1)
                (aexprFold fnum fadd fmul fdiv e2)
        | Div (e1, e2) ->
            fdiv
                (aexprFold fnum fadd fmul fdiv e1)
                (aexprFold fnum fadd fmul fdiv e2)

// This function can be used to evaluate an expression without error handling.
// It can also be used to make a string version 

// This is an easily readable solution to 4.15:
    let rec aexprFold2 fnum fadd fmul fdiv aexpr = 
        match aexpr with
        | Num v -> fnum v

        | Add (e1, e2) -> 
            let acc1 = aexprFold fnum fadd fmul fdiv e1
            let acc2 = aexprFold fnum fadd fmul fdiv e2
            fadd acc1 acc2
        
        | Mul (e1, e2) ->
            let acc1 = aexprFold fnum fadd fmul fdiv e1
            let acc2 = aexprFold fnum fadd fmul fdiv e2
            fmul acc1 acc2

        | Div (e1, e2) -> 
            let acc1 = aexprFold fnum fadd fmul fdiv e1
            let acc2 = aexprFold fnum fadd fmul fdiv e2
            fdiv acc1 acc2

// The following example creates a function simplAEval that works like aexprEval except that it doesn't use options, 
// but just crashes if we try to divide by zero.

    let simpleAEval = aexprFold id (+) ( * ) (/)

// Exercise 4.16
// Create the non-recursive functions aexprToString3, aexprEval4 and aexprToString4 that behaves like 
// the green or yellow functions aexprToString, aexprEval, and aexprToString2 respectively. The only 
// recursive function you may call is aexprFold. If your folding functions (fadd for example) gets too long,
// then define it as an external (or internal) function.

// Hint: For the aexprToString4 function it helps if your accumulator has the type (int * string) where 
// the integer is the level of the outermost operator of the expression that the string represents.

// Non-recursive version of aexprToString
    let aexprToString3 aexpr =
        aexprFold
            string
            (fun x y -> sprintf "(%s + %s)" x y)
            (fun x y -> sprintf "(%s * %s)" x y)
            (fun x y -> sprintf "(%s / %s)" x y)
            aexpr

// Non-recursive version of aexprEval
    let aexprEval5 aexpr =
        aexprFold
            Some
            (fun x y ->
                match x, y with
                | Some a, Some b -> Some (a + b)
                | _ -> None)
            (fun x y ->
                match x, y with
                | Some a, Some b -> Some (a * b)
                | _ -> None)
            (fun x y ->
                match x, y with
                | Some a, Some b when b <> 0 -> Some (a / b)
                | _ -> None)
            aexpr

// Non-recursive version of aexprToString2
    let aexprToString4 aexpr =

        let parenthesiseIf condition str =
            if condition then sprintf "(%s)" str
            else str

        let fnum n =
            (0, string n)

        let fadd (levelA, strA) (levelB, strB) =
            let left = parenthesiseIf (2 < levelA) strA
            let right = parenthesiseIf (2 < levelB) strB

            (2, sprintf "%s + %s" left right)

        let fmul (levelA, strA) (levelB, strB) =
            let left = parenthesiseIf (1 < levelA) strA
            let right = parenthesiseIf (1 < levelB) strB

            (1, sprintf "%s * %s" left right)

        let fdiv (levelA, strA) (levelB, strB) =
            let left = parenthesiseIf (1 < levelA) strA
            let right = parenthesiseIf (1 <= levelB) strB

            (1, sprintf "%s / %s" left right)

        let (_, str) =
            aexprFold fnum fadd fmul fdiv aexpr 
        str


// Exercise 4.17 (See description)
// Create a function bexprFold of type (see description) that given a folding function faexpr
// for arithmetic expressions, an initial value acc for TT, a folding function feq for equality, 
// flt for less than, fconj for conjunction, fnot for not, and a boolean expression b, returns

// acc if b is TT
// feq acc1 acc2 if b is equal to Eq(e1, e2) and folding e1 computes acc1 and folding e2 computes acc2
// flt acc1 acc2 if b is equal to Lt(e1, e2) and folding e1 computes acc1 and folding e2 computes acc2
// fconj acc1 acc2 if b is equal to Conj(e1, e2) and folding e1 computes acc1 and folding e2 computes acc2
// fnot acc1 if b is equal to Not e and folding e computes to acc1

    let rec bexprFold faexpr acc feq flt fconj fnot bexpr =
        match bexpr with
        | TT -> acc
        | Eq (e1, e2) -> feq (faexpr e1) (faexpr e2)
        | Lt (e1, e2) -> flt (faexpr e1) (faexpr e2)
        | Conj (e1, e2) ->
            fconj
                (bexprFold faexpr acc feq flt fconj fnot e1)
                (bexprFold faexpr acc feq flt fconj fnot e2)
        | Not e ->
            fnot (bexprFold faexpr acc feq flt fconj fnot e)

// A more easily readable version:
    let rec bexprFold2 faexpr acc feq flt fconj fnot bexpr =
        match bexpr with
        | TT -> acc

        | Eq (e1, e2) ->
            let acc1 = faexpr e1
            let acc2 = faexpr e2
            feq acc1 acc2

        | Lt (e1, e2) ->
            let acc1 = faexpr e1
            let acc2 = faexpr e2
            flt acc1 acc2

        | Conj (e1, e2) ->
            let acc1 = bexprFold2 faexpr acc feq flt fconj fnot e1
            let acc2 = bexprFold2 faexpr acc feq flt fconj fnot e2
            fconj acc1 acc2

        | Not e ->
            let acc1 = bexprFold2 faexpr acc feq flt fconj fnot e
            fnot acc1

// Similarly as for aexprFold the following example creates a function simplBEval that works like bexprEval 
// except that it doesn't use options or results, but just crashes if we try to divide by zero.

    let simpleBEval = bexprFold simpleAEval true (=) (<) (&&) not


// Exercise 4.18
// Create the non-recursive functions bexprToString3, bexprEval3, and bexprToString4 that behaves like 
// the green (or yellow) functions bexprToString, bexprEval, and bexprToString2 respectively. The only 
// recursive function you may call is bexprFold. If your folding functions (fconj for example) gets 
// too long, then define it as an external (or internal) function.

// Hint: For the bexprToString4 function it helps if your accumulator has the type (int * string) where
// the integer is the level of the outermost operator of the expression that the string represents.
    
// Non-recursive version of bexprToString
    let bexprToString3 bexpr =
        bexprFold
            aexprToString
            "true"
            (fun x y -> sprintf "(%s = %s)" x y)
            (fun x y -> sprintf "(%s < %s)" x y)
            (fun x y -> sprintf "(%s /\\ %s)" x y)
            (fun x -> sprintf "(not %s)" x)
            bexpr

// Non-recursive version of bexprEval
    let bexprEval3 bexpr =
        bexprFold
            aexprEval
            (Some true)
            (fun x y ->
                match x, y with
                | Some a, Some b -> Some (a = b)
                | _ -> None)
            (fun x y ->
                match x, y with
                | Some a, Some b -> Some (a < b)
                | _ -> None)
            (fun x y ->
                match x, y with
                | Some a, Some b -> Some (a && b)
                | _ -> None)
            (fun x ->
                match x with
                | Some a -> Some (not a)
                | None -> None)
            bexpr


// Non-recursive version of bexprToString2
    let bexprToString4 bexpr =

        let parenthesiseIf condition str =
            if condition then sprintf "(%s)" str
            else str

        let feq strA strB =
            (2, sprintf "%s = %s" strA strB)

        let flt strA strB =
            (2, sprintf "%s < %s" strA strB)

        let fconj (levelA, strA) (levelB, strB) =
            let left = parenthesiseIf (3 < levelA) strA
            let right = parenthesiseIf (3 < levelB) strB

            (3, sprintf "%s /\\ %s" left right)

        let fnot (levelA, strA) =
            let inside = parenthesiseIf (1 <= levelA) strA

            (1, sprintf "not %s" inside)

        let (_, str) =
            bexprFold
                aexprToString4
                (0, "true")
                feq
                flt
                fconj
                fnot
                bexpr

        str
