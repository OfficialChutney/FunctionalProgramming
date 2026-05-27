module parserNotes

// ============================================================================
// EXAM NOTES: Parser Combinators
// Functional Programming 2026, ITU Copenhagen (Lecture 11)
// ============================================================================
//
// KEY CONCEPTS
// ------------
//
// WHAT IS A PARSER?
//   A parser is a function that takes unstructured input (a string)
//   and produces structured output (an AST / data type).
//   "3+5*2" → Add (Num 3, Mul (Num 5, Num 2))
//
// HOW IT WORKS INTERNALLY:
//   type Parser<'a> = string -> Result<'a * string, ParseError>
//   A parser takes a string, and returns either:
//     Ok (value, remainingInput)  — parsed something, here's what's left
//     Error msg                   — parsing failed
//
// PARSER COMBINATORS:
//   Small parsers for simple things (single chars, integers)
//   combined into bigger parsers using combinator operators.
//   Parsers are first-class values — they can be passed as arguments.
//
// CONTEXT-FREE GRAMMARS:
//   Grammars describe what strings belong to a language.
//   Terminal symbols: actual characters (+, *, digits)
//   Non-terminal symbols: grammar rules (E, T, N)
//   Production rules: how non-terminals expand (E ::= T + E | T)
//
// PRECEDENCE:
//   Handled by splitting the grammar into levels.
//   Lower precedence operators are HIGHER in the grammar.
//   pE (addition, lowest precedence) calls down to
//   pT (multiplication) calls down to
//   pN (atoms, highest precedence)
//
// LEFT RECURSION:
//   A grammar like T ::= T + T causes infinite loops.
//   Fix by making it right-recursive: T ::= N + T
//
// DEFINITION vs EXECUTION ORDER:
//   We DEFINE bottom-up: atoms first (pN), then pT, then pE
//   We RUN top-down: start from pE, it delegates down to pT, then pN
//
// ============================================================================


// ============================================================================
// PART 1: THE COMBINATOR OPERATORS
// ============================================================================
//
// BASIC PARSERS (building blocks):
//
//   pint32              Parser<int>         parse an integer
//   pchar 'x'           Parser<char>        parse a specific character
//   pstring "hello"     Parser<string>      parse a specific string
//   asciiLetter          Parser<char>        parse any letter
//   digit                Parser<char>        parse any digit
//   satisfy f            Parser<char>        parse a char where f returns true
//
//
// SEQUENCING (run parsers one after another):
//
//   p1 .>>. p2          keep BOTH results as a tuple
//   p1 .>> p2           keep only LEFT result
//   p1 >>. p2           keep only RIGHT result
//
//   The dots point to which side you keep.
//
//   Examples on input "ab":
//     pchar 'a' .>>. pchar 'b'   → ('a', 'b')
//     pchar 'a' .>> pchar 'b'    → 'a'
//     pchar 'a' >>. pchar 'b'    → 'b'
//
//
// MAPPING (transform the parsed result):
//
//   p |>> f             run parser p, apply function f to the result
//
//   Examples:
//     pint32 |>> Num                    parse int, wrap in Num constructor
//     pstring "true" |>> fun _ -> True  parse "true", return AST value True
//     many pchar 'a' |>> fun l -> l.Length   count how many a's
//
//
// CHOICE (try alternatives):
//
//   p1 <|> p2           try p1 first, if it fails try p2
//
//   Example:
//     pNum <|> pPar      try parsing a number, otherwise try parentheses
//
//
// REPETITION:
//
//   many p               repeat p zero or more times → Parser<'a list>
//   many1 p              repeat p one or more times → Parser<'a list>
//
//   Example:
//     many (pchar 'a') on "aaab" → ['a'; 'a'; 'a']
//     many1 (pchar 'a') on "bbb" → fails (needs at least one)
//
//
// UTILITY:
//
//   between l p r        parse l, then p, then r, keep only p
//                        between (pchar '(') pE (pchar ')')
//
//   p <?> "label"        attach error label to parser
//
//   opt p                try p, return Some result or None
//
//   sepBy p sep          parse p separated by sep, zero or more times
//   sepBy1 p sep         same but at least one
//
//
// RUNNING A PARSER:
//
//   run : Parser<'a> -> string -> Result<'a, string>
//   run pE "3+5*2" → Ok (Add (Num 3, Mul (Num 5, Num 2)))
//
//
// ============================================================================
// PART 2: BUILDING A PARSER FROM A GRAMMAR
// ============================================================================
//
// STEP 1: Define the AST type
// STEP 2: Write the grammar with precedence levels
// STEP 3: Remove left recursion
// STEP 4: Create forward references for recursive parsers
// STEP 5: Write parsers bottom-up (atoms first)
// STEP 6: Set the forward references
//
// ============================================================================

// EXAMPLE: Arithmetic expression parser

// STEP 1: AST type
type calc =
    | Num of int
    | Add of calc * calc
    | Mul of calc * calc

// STEP 2 & 3: Grammar (already non-left-recursive)
// E ::= T + E | T       (addition, lowest precedence)
// T ::= N * T | N       (multiplication, higher precedence)
// N ::= integer | (E)   (atoms, highest precedence)

// STEP 4: Forward references for recursive parsers
// Needed because pE and pT are used before they're fully defined.
// createParserForwardedToRef creates a placeholder parser and a
// reference to fill in later.
//
// let pE, eref = createParserForwardedToRef<calc>()
// let pT, tref = createParserForwardedToRef<calc>()

// STEP 5: Write parsers bottom-up

// N ::= integer | (E)
// let pNum = pint32 |>> Num                           // parse int, wrap in Num
// let pPar = pchar '(' >>. pE .>> pchar ')'           // parse (E), keep E
// let pN   = pNum <|> pPar                             // try integer, else (E)

// T ::= N * T | N
// let pMul = pN .>> pchar '*' .>>. pT |>> Mul         // N then * then T, wrap in Mul
                                                        // .>> throws away *
                                                        // .>>. keeps both N result and T result
                                                        // |>> Mul wraps the tuple in Mul

// E ::= T + E | T
// let pAdd = pT .>> pchar '+' .>>. pE |>> Add         // T then + then E, wrap in Add

// STEP 6: Set the forward references
// do tref := pMul <|> pN                               // T is: try Mul, else just N
// do eref := pAdd <|> pT                               // E is: try Add, else just T


// ============================================================================
// PART 3: HOW TO READ PARSER EXPRESSIONS
// ============================================================================
//
// Read left to right. Each operator does one thing:
//
//   pN .>> pchar '*' .>>. pT |>> Mul
//   ^^                                   parse an N
//       ^^^                              then parse *, throw it away
//                    ^^^^                then parse a T, keep both N and T
//                          ^^^^^^        wrap the tuple in Mul
//
//   pchar '(' >>. pE .>> pchar ')'
//   ^^^^^^^^^^                           parse (, throw it away
//              ^^^                       parse E, keep it
//                   ^^^^^^^^^^^^         parse ), throw it away
//                                        result: just E
//
//   pstring "true" |>> fun _ -> True
//   ^^^^^^^^^^^^^^^                      parse the string "true"
//                  ^^^^^^^^^^^^^^^^^^^^  ignore the string, return AST True
//
//
// ============================================================================
// PART 4: COMMON PATTERNS
// ============================================================================
//
// BINARY OPERATOR (general pattern):
//   grammar:  X ::= Y op Z
//   parser:   let pOp = pY .>> pchar 'op' .>>. pZ |>> Constructor
//
// UNARY OPERATOR (general pattern):
//   grammar:  X ::= op Y
//   parser:   let pOp = pchar 'op' >>. pY |>> Constructor
//
// PARENTHESIZED EXPRESSION:
//   grammar:  X ::= ( Y )
//   parser:   let pPar = pchar '(' >>. pY .>> pchar ')'
//
// CHOICE BETWEEN ALTERNATIVES:
//   grammar:  X ::= A | B | C
//   parser:   let pX = pA <|> pB <|> pC
//
// HANDLING SPACES:
//   Insert spaces >>. before or .>> after operators:
//   pT .>> (spaces >>. pchar '+' >>. spaces) .>>. pE |>> Add
//
// RECURSIVE PARSERS:
//   let pX, xref = createParserForwardedToRef<MyType>()
//   // ... define parsers using pX ...
//   do xref := pSomething <|> pSomethingElse
//
//
// ============================================================================
// QUICK REFERENCE
// ============================================================================
//
//   .>>.    sequence, keep both       ('a * 'b)
//   .>>     sequence, keep left       'a
//   >>.     sequence, keep right      'b
//   |>>     map result                transform 'a to 'b
//   <|>     choice                    try first, then second
//   many    repeat 0+                 'a list
//   many1   repeat 1+                 'a list
//   <?>     label                     error messages
//   run     execute parser            Result<'a, string>
//
//   Define bottom-up, run top-down.
//   Dots point to the side you keep.
//   |>> transforms what was parsed into your AST.
//   <|> tries left first, falls back to right.
//
// ============================================================================