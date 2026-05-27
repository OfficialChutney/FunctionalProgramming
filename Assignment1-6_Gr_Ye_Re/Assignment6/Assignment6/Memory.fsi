module Interpreter.Memory

open Language

// Exercise 6.8
// Add a reference to the memory type, but don't declare the type here as well. 
// The only implementation should be in the Memory.fs.

type memory

val empty : int -> memory

val alloc : int -> memory -> Result<memory * int, error>

val free : int -> int -> memory -> Result<memory, error>

val setMem : int -> int -> memory -> Result<memory, error>

val getMem : int -> memory -> Result<int, error>