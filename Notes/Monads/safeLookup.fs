module safeLookup

(* Without >>= and ret
let safeLookup (key : string) (m : Map<string, int>) : int option =
    Map.tryFind key m


let lookupAndDivide (x : string) (y : string) (m : Map<string, int>) : int option =

    match safeLookup x m, safeLookup y m with
    | Some k1, Some k2 when k2 <> 0 -> Some (k1 / k2)
    | _ -> None
*)

let ret x = Some x

let (>>=) x f =
    match x with
    | Some a -> f a
    | None -> None


let safeLookup (key : string) (m : Map<string, int>) : int option =
    Map.tryFind key m

let lookupAndDivide (x : string) (y : string) (m : Map<string, int>) : int option =
    safeLookup x m >>= fun r1 -> 
    safeLookup y m >>= fun r2 -> 
    if r2 <> 0 then ret (r1/r2) else None




type MyResultBuilder() =
    member bld.Bind(a, f) = a >>= f
    member bld.Return e   = ret e
    member bld.ReturnFrom e = e
let ev = MyResultBuilder()


let lookupAndDivideEv(x : string) (y : string) (m : Map<string, int>) : int option =
    ev { let! r1 = safeLookup x m 
         let! r2 = safeLookup y m
         if r2 <> 0 then return (r1/r2) else return! None }


