module Interpreter.State

    open Interpreter.Language

    type state = {
        state : Map<string, int>
        memory : Interpreter.Memory.memory
        rand : System.Random
    }

    val mkState : int -> int option -> state
    val declare : string -> state -> Result<state, error>
    val getVar : string -> state -> Result<int, error>
    val setVar : string -> int -> state -> Result<state,error>
    val free : int -> int -> state -> Result<state,error>
    val setMem : int -> int -> state -> Result<state,error>
    val getMem : int -> state -> Result<int,error>
    val alloc : string -> int -> state -> Result<state,error>