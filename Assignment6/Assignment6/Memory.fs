module Interpreter.Memory
    
    open Interpreter.Language

    type memory = {
        memory : Map<int, int>
        next : int
    }

    let empty (memSize : int) = { memory = Map.empty; next = 0 }

    let alloc (size : int) (memory : memory) = 

        let rec runAllocation (size : int) (memory : memory) (index : int) =
            match index with
            | index when index <= memory.next+size-1 -> 
                runAllocation size { memory with memory = memory.memory.Add (index, 0) } (index + 1)
            | _ -> { memory with next = memory.next + size }

        match size with
        | x when x > 0 -> 
            let ptr = memory.next
            let newMemory = runAllocation size memory ptr
            Ok (newMemory, ptr)
        | x -> Error (NegativeMemoryAllocated x)
    
    let free (ptr : int) (size : int) (mem : memory) =

        let rec allHasMemory (ptr : int) (size : int) (mem : memory) (index : int) =
            match index with
            | i when i >= ptr + size ->
                Ok ()
            | i when mem.memory.ContainsKey i ->
                allHasMemory ptr size mem (i + 1)
            | _ ->
                Error (MemoryNotAllocated index)

        let rec freeA (ptr : int) (size : int) (mem : memory) (index : int) =
            match index with
            | i when i < ptr ->
                Ok mem
            | i when mem.memory.ContainsKey i ->
                freeA ptr size { mem with memory = mem.memory.Remove i } (i - 1)
            | _ ->
                Error (MemoryNotAllocated index)

        match allHasMemory ptr size mem ptr with
        | Ok () ->
            freeA ptr size mem (ptr + size - 1)
        | Error e ->
            Error e
        
    let setMem (ptr : int) (v : int) (mem : memory) = 
        match mem.memory.ContainsKey ptr with
        | true -> Ok ({mem with memory = mem.memory.Add (ptr, v)})
        | false -> Error (MemoryNotAllocated ptr)
        
    let getMem (ptr : int) (mem : memory) = 
        match mem.memory.TryFind ptr with
        | Some v -> Ok(v) 
        | None -> Error (MemoryNotAllocated ptr)
