module Interpreter.Memory

    open Language
    
// Exercise 6.2
// Create a type memory that contains your memory as
// - A map from integers (memory addresses) to integers (values)
// - An integer containing the next available free memory address, which we call next.

    type memory0 = {mem : Map<int, int>; next : int}


// Exercise 6.3
// Create a function empty of type int -> memory that given an integer memSize returns
// a blank memory represented as the empty map and next set to 0. The memSize argument
// is not used, but it must be there to be compatible with the Red exercises. But here,
// you must include it, but the body of the function should just ignore it.

    let empty0 (memSize : int) =
        {mem = Map.empty; next = 0}
    

// Exercise 6.4
// Create a function alloc of type int -> memory -> (memory * int) option that given an 
// amount of memory to allocate size and memory returns

//  - Some(mem', next) where mem' is identical to  mem but where all addresses in from next 
// to next+size-1 are set to 0, if size is strictly greater than 0 and where the next pointer is updated to next + size
//  - None otherwise (if size is smaller than or equal to 0)

    let alloc0 (size : int) (memory : memory0) =
        if size <= 0 then
            Error (NegativeMemoryAllocated size)
        else
            let startAddress = memory.next
            let endAddress = memory.next + size - 1

            let newMem =
                [startAddress .. endAddress]
                |> List.fold (fun m address -> Map.add address 0 m) memory.mem

            let newMemory =
                {mem = newMem; next = memory.next + size}

            Ok (newMemory, startAddress)


// Exercise 6.5
// Create a function free of type int -> int -> memory -> memory option that given a memory position ptr, a size size, and memory mem returns

// - Some mem' where mem' is identical to mem, but where all addresses from ptr to ptr + size - 1 have been removed, as long as all of these addresses are allocated in mem
// - None otherwise Note that free does not decrease the next pointer.
    let free0 (ptr : int) (size : int) (memory : memory0) =
        if size <= 0 then
            Error (NegativeMemoryAllocated size)
        else
            let addresses = [ptr .. ptr + size - 1]

            let firstNotAllocated =
                addresses
                |> List.tryFind (fun address -> not (Map.containsKey address memory.mem))

            match firstNotAllocated with
            | Some address ->
                Error (MemoryNotAllocated address)

            | None ->
                let newMem =
                    addresses
                    |> List.fold (fun m address -> Map.remove address m) memory.mem

                Ok {memory with mem = newMem}

// Exercise 6.6 
// Create a function setMem of type int -> int -> memory -> memory option that given an address ptr, a value v, and memory mem returns

// - Some mem' where mem' is equal to mem but where the the address ptr has been mapped to v, if ptr is allocated.
// - None othrewise
    let setMem0 (ptr : int) (v : int) (memory : memory0) =
        if Map.containsKey ptr memory.mem then
            let newMem = Map.add ptr v memory.mem
            Ok {memory with mem = newMem}
        else
            Error (MemoryNotAllocated ptr)

// Exercise 6.7
// Create a function getMem of type int -> memory -> int option that given an address ptr and memory mem returns

// - Some v, where v is the value stored in mem at address ptr, if ptr is allocated
// - None otherwise
    let getMem0 (ptr : int) (memory : memory0) =
        match Map.tryFind ptr memory.mem with
        | Some value ->
            Ok value
        | None ->
            Error (MemoryNotAllocated ptr)


// RED EXERCISES
// _________________________________________________________


// Red memory type:
// - arr is the fixed-size memory array
// - freeMap keeps track of free chunks:
//   key = chunk size
//   value = list of start addresses for chunks of that size
    type memory =
        {
            arr : int array
            freeMap : Map<int, int list>
        }


// Helper: add a free chunk to the free map
    let private addFreeChunk (start : int) (size : int) (freeMap : Map<int, int list>) =
        if size <= 0 then
            freeMap
        else
            match Map.tryFind size freeMap with
            | Some starts ->
                Map.add size (start :: starts) freeMap
            | None ->
                Map.add size [start] freeMap


// Helper: remove one free chunk from the free map
    let private removeFreeChunk (start : int) (size : int) (freeMap : Map<int, int list>) =
        match Map.tryFind size freeMap with
        | None ->
            freeMap

        | Some starts ->
            let newStarts = List.filter (fun s -> s <> start) starts

            if List.isEmpty newStarts then
                Map.remove size freeMap
            else
                Map.add size newStarts freeMap


// Helper: check if an address is inside a free chunk
    let private isFree (ptr : int) (mem : memory) =
        mem.freeMap
        |> Map.exists (fun size starts ->
            starts
            |> List.exists (fun start ->
                ptr >= start && ptr < start + size))


// Helper: check if an address is allocated
    let private isAllocated (ptr : int) (mem : memory) =
        ptr >= 0
        && ptr < mem.arr.Length
        && not (isFree ptr mem)


// Helper: convert freeMap to a list of chunks: (start, size)
    let private chunksFromFreeMap (freeMap : Map<int, int list>) =
        freeMap
        |> Map.toList
        |> List.collect (fun (size, starts) ->
            starts |> List.map (fun start -> (start, size)))
        |> List.sortBy fst


// Exercise Red: defrag
// Merge adjacent free chunks
    let defrag (mem : memory) =
        let chunks = chunksFromFreeMap mem.freeMap

        let rec merge chunks acc =
            match chunks, acc with
            | [], _ ->
                List.rev acc

            | (start, size) :: rest, [] ->
                merge rest [(start, size)]

            | (start, size) :: rest, (prevStart, prevSize) :: accRest ->
                let prevEnd = prevStart + prevSize

                if prevEnd = start then
                    let mergedChunk = (prevStart, prevSize + size)
                    merge rest (mergedChunk :: accRest)
                else
                    merge rest ((start, size) :: (prevStart, prevSize) :: accRest)

        let mergedChunks = merge chunks []

        let newFreeMap =
            mergedChunks
            |> List.fold (fun map (start, size) ->
                addFreeChunk start size map) Map.empty

        {mem with freeMap = newFreeMap}


// Exercise 6.3 / Red
// empty : int -> memory
    let empty (memSize : int) =
        {
            arr = Array.zeroCreate memSize
            freeMap = Map.ofList [(memSize, [0])]
        }


// Helper: try to allocate without defragmenting
    let private allocWithoutDefrag (size : int) (mem : memory) =
        let possibleChunk =
            mem.freeMap
            |> Map.toSeq
            |> Seq.tryFind (fun (freeSize, starts) ->
                freeSize >= size && not (List.isEmpty starts))

        match possibleChunk with
        | None ->
            None

        | Some (freeSize, starts) ->
            let startAddress = List.head starts

            let freeMapWithoutChunk =
                removeFreeChunk startAddress freeSize mem.freeMap

            let remainingSize = freeSize - size
            let remainingStart = startAddress + size

            let newFreeMap =
                addFreeChunk remainingStart remainingSize freeMapWithoutChunk

            let newMemory =
                {mem with freeMap = newFreeMap}

            Some (newMemory, startAddress)


// Exercise 6.4 / Red
// alloc : int -> memory -> Result<memory * int, error>
    let alloc (size : int) (mem : memory) =
        if size <= 0 then
            Error (NegativeMemoryAllocated size)
        else
            match allocWithoutDefrag size mem with
            | Some result ->
                Ok result

            | None ->
                let defraggedMem = defrag mem

                match allocWithoutDefrag size defraggedMem with
                | Some result ->
                    Ok result

                | None ->
                    Error OutOfMemory


// Exercise 6.5 / Red
// free : int -> int -> memory -> Result<memory, error>
    let free (ptr : int) (size : int) (mem : memory) =
        if size <= 0 then
            Error (NegativeMemoryAllocated size)
        else
            let addresses = [ptr .. ptr + size - 1]

            let firstNotAllocated =
                addresses
                |> List.tryFind (fun address -> not (isAllocated address mem))

            match firstNotAllocated with
            | Some ptr' ->
                Error (MemoryNotAllocated ptr')

            | None ->
                let newFreeMap =
                    addFreeChunk ptr size mem.freeMap

                Ok {mem with freeMap = newFreeMap}


// Exercise 6.6 / Red
// setMem : int -> int -> memory -> Result<memory, error>
    let setMem (ptr : int) (v : int) (mem : memory) =
        if isAllocated ptr mem then
            let newArray = Array.copy mem.arr
            newArray.[ptr] <- v

            Ok {mem with arr = newArray}
        else
            Error (MemoryNotAllocated ptr)


// Exercise 6.7 / Red
// getMem : int -> memory -> Result<int, error>
    let getMem (ptr : int) (mem : memory) =
        if isAllocated ptr mem then
            Ok mem.arr.[ptr]
        else
            Error (MemoryNotAllocated ptr)