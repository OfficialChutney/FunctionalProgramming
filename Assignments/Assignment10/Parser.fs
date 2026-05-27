    module Interpreter.Parser

    open Interpreter.Language

    (*

    The interfaces for JParsec and FParsecLight are identical and the implementations should always produce the same output
    for successful parses although running times and error messages will differ. Please report any inconsistencies.

    *)

    // open JParsec.TextParser          // Example parser combinator library.
    open FParsecLight.TextParser        // Industrial parser-combinator library. Use if performance gets bad
    

    let pif       : Parser<string> = pstring "if"
    let pelse     : Parser<string> = pstring "else"
    let palloc    : Parser<string> = pstring "alloc"
    let pfree     : Parser<string> = pstring "free"
    let pwhile    : Parser<string> = pstring "while"
    let pdo       : Parser<string> = pstring "pdo"
    let pdeclare  : Parser<string> = pstring "declare"
    let ptrue     : Parser<string> = pstring "true"
    let pfalse    : Parser<string> = pstring "false"
    let pprint    : Parser<string> = pstring "print"
    let prandom   : Parser<string> = pstring "random"
    let pread     : Parser<string> = pstring "read"
    let pfunction : Parser<string> = pstring "not implemented"
    let pret      : Parser<string> = pstring "not implemented"
    
    let pwhitespaceChar = (satisfy System.Char.IsWhiteSpace)
    let pletter         = (satisfy System.Char.IsLetter) 
         
    let palphanumeric   = (satisfy System.Char.IsLetterOrDigit) 

    let spaces         = many pwhitespaceChar
    let spaces1        = many1 pwhitespaceChar

    let (.>*>.) p1 p2 = (.>>.) ((.>>) p1 spaces) p2

    let (.>*>) p1 p2  = (.>>) ((.>>) p1 spaces) p2
    let (>*>.) p1 p2  = (>>.) ((.>>) p1 spaces) p2

    let parenthesise (p1 : Parser<'a>) = (.>*>) ((>*>.) ((>>.) spaces (satisfy (fun c -> c = '('))) p1) (satisfy (fun c -> c = ')'))
    let curlyBrackethise (p1 : Parser<'a>) = (.>*>) ((>*>.) ((>>.) spaces (satisfy (fun c -> c = '{'))) p1) (satisfy (fun c -> c = '}'))
    let squareBrackethise (p1 : Parser<'a>) = (.>*>) ((>*>.) ((>>.) spaces (satisfy (fun c -> c = '['))) p1) (satisfy (fun c -> c = ']'))
        
    let charToString (a : (char * char list))  =

        let rec charToStringAux (chars : char list) (str : string) =
            match chars with
            | [] -> str
            | x::xs -> charToStringAux xs (str + string(x)) 

        charToStringAux ([fst a] @ (snd a)) ""

    let pid = ((.>>.) ((<|>) (satisfy (fun c -> c = '_')) pletter) (many ((<|>) palphanumeric (satisfy (fun c -> c = '_'))))) |>> charToString

    let parseString = (.>>) ((>>.) (satisfy (fun c -> c = '\"')) (many (satisfy (fun c -> c <> '\"')))) (satisfy (fun c -> c = '\"')) |>> (fun chars -> (new string [|for c in chars -> c|]))
    
    let unop (op : Parser<'a>) (a : Parser<'b>) = (>*>.) op a 

    let binop (op : Parser<'a>) (a : Parser<'b>) (b : Parser<'c>) : Parser<'b * 'c> = (.>*>.) ((.>*>) a op) b

    let LevelOneParseAexpr, oneAexprRef = createParserForwardedToRef<aexpr>()
    let LevelTwoParseAexpr, twoAexprRef = createParserForwardedToRef<aexpr>()
    let LevelThreeParseAexpr, threeAexprRef = createParserForwardedToRef<aexpr>()
    let LevelFourParseAexpr, fourAexprRef = createParserForwardedToRef<aexpr>()
    let LevelOneParseBexpr, oneBexprRef = createParserForwardedToRef<bexpr>()
    let LevelTwoParseBExpr, twoBexprRef = createParserForwardedToRef<bexpr>()

    //Level 1
    let CondParse =
        (.>*>.) ((.>*>) LevelOneParseBexpr (pchar '?')) ((.>*>.) ((.>*>) LevelOneParseAexpr (pchar ':')) LevelOneParseAexpr)
        |>> (fun (a, (b, c)) -> Cond(a, b, c))
        <?> "Cond"
    do oneAexprRef := choice [CondParse; LevelTwoParseAexpr]

    //Level 2

    let AddParse = binop (pchar '+') LevelThreeParseAexpr LevelTwoParseAexpr |>> Add <?> "Add"
    let SubParse = binop (pchar '-') LevelThreeParseAexpr LevelTwoParseAexpr |>> (fun (left, right) -> Add(left, Mul(right, Num -1))) <?> "Sub"

    do twoAexprRef := choice [AddParse; SubParse; LevelThreeParseAexpr]

        
    //Level 3

    let MulParse = binop (pchar '*') LevelFourParseAexpr LevelThreeParseAexpr |>> Mul <?> "Mul"

    let DivParse = binop (pchar '/') LevelFourParseAexpr LevelThreeParseAexpr |>> Div <?> "Div"

    let ModParse = binop (pchar '%') LevelFourParseAexpr LevelThreeParseAexpr |>> (fun (a,b) -> Add(a, Mul(Mul(Div(a, b), b), Num -1))) <?> "Mod"

    do threeAexprRef := choice [MulParse; DivParse; ModParse; LevelFourParseAexpr]


    //Level 4

    let negationAExprParse = unop (pchar '-') LevelFourParseAexpr |>> (fun a -> Mul(Num -1, a))

    let ParParse = parenthesise LevelOneParseAexpr <?> "ParAExpr"
    let BraParse = squareBrackethise LevelOneParseAexpr |>> MemRead <?> "Square"

    let readParse = pread |>> (fun _ -> Read) <?> "Read"

    let randomParse = prandom |>> (fun _ -> Random) <?> "Random"

    let NParse   = pint32 |>> Num <?> "Int"

    let VParse = pid |>> Var <?> "Var" 

    do fourAexprRef := choice [negationAExprParse; ParParse; BraParse; readParse; randomParse; NParse; VParse]


    //BEXPR



    //Level 2

    let trueParse = ptrue |>> (fun _ -> TT) <?> "true"
    let falseParse = pfalse |>> (fun _ -> Not TT) <?> "false"
    let negateBexprParse = unop (pchar '~') LevelTwoParseBExpr |>> (fun e -> Not e) <?> "Negation"

    let eqParse = binop (pchar '=') LevelTwoParseAexpr LevelOneParseAexpr |>> Eq <?> "Equal"

    let notEqParse = binop (pstring "<>") LevelTwoParseAexpr LevelOneParseAexpr |>> (fun (a, b) -> Not (Eq (a,b))) <?> "Not Equal"

    let lessThanParse = binop (pchar '<') LevelTwoParseAexpr LevelOneParseAexpr |>> Lt <?> "Less Than"
    let lessThanOrEqualParse =
        binop (pstring "<=") LevelTwoParseAexpr LevelOneParseAexpr
        |>> (fun (a,b) -> Not (Conj (Not (Lt(a, b)), Not (Not (Not (Eq(a, b)))))))
        <?> "Less Than or equal to"
    
    let biggerThanParse =
        binop (pchar '>') LevelTwoParseAexpr LevelOneParseAexpr
        |>> (fun (a,b) -> Conj (Not (Eq (a, b)), Not (Lt (a, b))))
        <?> "Bigger Than"

    let biggerThanOrEqualTo =
        binop (pstring ">=") LevelTwoParseAexpr LevelOneParseAexpr
        |>> (fun (a,b) -> Not (Lt(a, b)))
        <?> "Bigger Than or equal to"

    let ParBParse = parenthesise LevelOneParseBexpr <?> "ParBExpr"

    //Level one

    let AndExpr = binop (pstring "/\\") LevelTwoParseBExpr LevelOneParseBexpr |>> Conj <?> "AND"

    let ORExpr = binop (pstring "\\/") LevelTwoParseBExpr LevelOneParseBexpr |>> (fun (a,b) -> Not (Conj (Not a, Not b))) <?> "OR"
    
    do oneBexprRef := choice [AndExpr; ORExpr; LevelTwoParseBExpr]

    do twoBexprRef := choice [trueParse; falseParse; negateBexprParse; ParBParse; eqParse; notEqParse; lessThanOrEqualParse; lessThanParse; biggerThanOrEqualTo; biggerThanParse]

    let LevelOneParseStmnt, oneStmntRef = createParserForwardedToRef<stmnt>()
    let LevelTwoParseStmnt, twoStmntRef = createParserForwardedToRef<stmnt>()

    //Level 2 statements

    let CurlyParse = curlyBrackethise LevelOneParseStmnt <?> "StmntCurlyParse" 

    let parseVStmnt =
        (.>>.) pid (spaces >>. pstring ":=" >>. spaces >>. LevelOneParseAexpr)
        |>> (fun (a, b) -> Assign (a, b))
        <?> "StmntVar"

    let parseDeclare = (>>.) ((.>>.) pdeclare spaces1) pid |>> Declare <?> "Declare"

    let parseCondition = (<|>) (parenthesise LevelOneParseBexpr) LevelOneParseBexpr

    let parseIfElse = (.>*>.) ((.>*>) ( (.>*>.) ((>*>.) pif parseCondition) CurlyParse) pelse) CurlyParse |>> (fun ((a, b), c) -> If (a, b, c)) <?> "IfElse"

    let parseIf = (.>*>.) ((>*>.) pif parseCondition) CurlyParse |>> (fun ((a, b)) -> If (a, b, Skip)) <?> "If"

    let parseWhile = (.>*>.) ((>*>.) pwhile parseCondition) CurlyParse |>> (fun (a,b) -> While (a, b))

    let parseAllocSpaced = (>>.) ((.>>) palloc spaces1) ((.>*>.) pid LevelOneParseAexpr) |>> (fun (a,b) -> Alloc (a, b))
    let parseAllocParenthesised = (>>.) palloc (parenthesise ((.>>.) ((.>>) pid (pstring ", ")) LevelOneParseAexpr )) |>> (fun (a,b) -> Alloc (a, b))
    let parseAlloc = (<|>) parseAllocSpaced parseAllocParenthesised

    let parseFree = (>>.) pfree (parenthesise ((.>>.) ((.>>) LevelOneParseAexpr (pstring ", ")) LevelOneParseAexpr )) |>> (fun (a,b) -> Free (a, b))

    let parsePrint = (>>.) (pstring "print") (parenthesise ( (.>>.) parseString (many ( (>>.) (pstring ", ") LevelOneParseAexpr  )) )) |>> (fun (a, b) -> Print (b, a))

    let parseMem =
        (.>>.) (squareBrackethise LevelOneParseAexpr) (spaces >>. pstring ":=" >>. spaces >>. LevelOneParseAexpr)
        |>> (fun (a, b) -> MemWrite (a, b))
        <?> "MemWrite"

    let parseSeq =
        choice [
            (.>>.) LevelTwoParseStmnt (spaces >>. pchar ';' >>. spaces >>. LevelOneParseStmnt)
            |>> (fun (head, tail) -> Seq (head, tail))
            LevelTwoParseStmnt
        ]
        <?> "Seq"

    

    do oneStmntRef := parseSeq

    do twoStmntRef := choice [parseVStmnt; parseDeclare; parseIfElse; parseIf; parseWhile; parseAlloc; parseFree; parsePrint; parseMem]

    let paexpr = LevelOneParseAexpr

    let pbexpr = LevelOneParseBexpr

    let pstmnt = LevelOneParseStmnt
    
    let pprogram = pstmnt |>> (fun s -> (Map.empty : program), s)
    
    let run = run
       
    let runProgramParser = run (pprogram .>> spaces .>> eof)  
