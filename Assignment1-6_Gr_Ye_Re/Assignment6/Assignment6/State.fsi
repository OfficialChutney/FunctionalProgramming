module Interpreter.State

open Interpreter.Language
open Interpreter.Memory

// Exercise 6.1 
// Create a file State.fsi, that contains the signatures of the state type and select functions from state.fs 

type state

val mkState : int -> state

val declare : string -> state -> Result<state, error>

val getVar : string -> state -> Result<int, error>

val setVar : string -> int -> state -> Result<state, error>

val alloc  : string -> int -> state -> Result<state, error>

val free   : int -> int -> state -> Result<state, error>

val getMem : int -> state -> Result<int, error>

val setMem : int -> int -> state -> Result<state, error>

val push : state -> state

val pop : state -> state