module threadSimple
open System.Threading

let slowAdd a b = async { 
        do! Async.Sleep 1000 
        return (a + b) }


let seqCalc = async {
    let! r1 = slowAdd 1 2
    let! r2 = slowAdd 3 4
    return (r1,r2)
}

let parCalc = [slowAdd 1 2; slowAdd 3 4] |> Async.Parallel

let logger = MailboxProcessor.Start(fun inbox ->
    let mutable counter = 1
    
    let rec loop () = async {
        let! message = inbox.Receive()
        printf "%d %s" counter message 
        counter <- counter + 1
        return! loop()
    }
    loop ()
)

type CounterMsg =
    | Increment
    | GetCount of AsyncReplyChannel<int>

let logger2 = MailboxProcessor.Start(fun inbox ->
    let mutable counter = 0
    
    let rec loop () = async {
        let! message = inbox.Receive()

        match message with
        |Increment -> counter <- counter + 1
        |GetCount x -> x.Reply counter

        return! loop()
    }
    loop ()
)


let countForever (name : string) : Async<unit> =
    let rec loop n = async { 
        printfn "%s: %d" name n
        do! Async.Sleep 1000
        return! loop (n+1)
    }
    loop 1
     
let cts1 = new CancellationTokenSource()
Async.Start(countForever "first", cts1.Token)

let cts2 = new CancellationTokenSource()
Async.Start(countForever "second", cts2.Token)

Thread.Sleep(3000)
cts2.Cancel()




let safeDivideAsync a b : Async<int> =
    async {
        match a,b with
        | x,y when y = 0 -> return failwith "div by 0"
        | x, y -> return x/y
    }

Async.StartWithContinuations(
    safeDivideAsync 10 2,
    (fun result -> printfn "OK: %A" result),
    (fun exn -> printfn "%A" exn),
    (fun cancel -> printfn "Cancelled")
)


let processUrls (urls : string list) : int[] =
    urls
    |> List.map (fun url -> async { 
        do! Async.Sleep 1000
        return url.Length })
    |> Async.Parallel
    |> Async.RunSynchronously


type StoreMsg =
    | Set of string * int
    | Get of string * AsyncReplyChannel<int option>
    | GetAll of AsyncReplyChannel<(string * int) list>

let msgProcessor = MailboxProcessor.Start(fun inbox ->    

    let rec loop (messages : Map<string, int>) = async {
        let! message = inbox.Receive()

        match message with
        |Set (s, x) -> return! loop (Map.add s x messages)
        |Get (s, ch) -> ch.Reply (Map.tryFind s messages)
                        return! loop messages
        |GetAll ch -> ch.Reply (Map.toList messages)
                      return! loop messages

        return! loop messages
    }
    loop Map.empty
)
