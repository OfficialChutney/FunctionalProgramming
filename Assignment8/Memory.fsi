module Interpreter.Memory
    
    open Interpreter.Language
    
    type memory

    val empty : int -> memory

    val alloc : int -> memory -> Result<(memory * int),error>

    val free : int -> int -> memory -> Result<memory,error>

    val setMem : int -> int -> memory -> Result<memory,error>

    val getMem : int -> memory -> Result<int,error>