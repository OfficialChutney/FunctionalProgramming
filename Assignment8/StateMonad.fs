module Interpreter.StateMonad

    // Use one of these state monads depending on which combination of green, yellow, and red exercises you are doing.
    // Feel free to just delete the rest to unclutter the code.
    
    open State
    open Language
    
    
    // Yellow exercises
    
    
    type 'a stateMonad = SM of (state -> Result<'a * state, error>)
        
    let ret x      = SM (fun st -> Ok(x, st))
    let fail err = SM (fun _ -> Error err)
    
    let bind (SM f) g =
        SM (fun st ->
            match f st with
            | Ok (x, st') -> let (SM h) = g x in h st'
            | Error err   -> Error err) 

    let declare (str : string) =
        SM (fun (st : state) ->
            match State.declare str st with
            | Ok st' -> Ok ((), st')
            | Error err -> Error err)

    let setVar (str : string) (v : int) =
        SM (fun (st : state) -> 
            match State.setVar str v st with
            | Ok st' -> Ok ((), st')
            | Error err -> Error err
        )

    let getVar (str : string) =
        SM (fun (st : state) ->
            match State.getVar str st with
            | Ok v -> Ok(v, st)
            | Error err -> Error err
        )

    let alloc (str : string) (size : int) =
        SM (fun (st : state) -> 
            match State.alloc str size st with
            | Ok st' -> Ok (((), st'))
            | Error err -> Error err
        )

    let free (ptr : int) (size : int) =
        SM (fun (st : state) -> 
            match State.free ptr size st with
            | Ok st' -> Ok ((), st')
            | Error err -> Error err
        )

    let setMem (ptr : int) (v : int) =
        SM (fun (st : state) -> 
            match State.setMem ptr v st with
            | Ok st' -> Ok ((), st')
            | Error err -> Error err
        )

    let getMem (ptr : int) =
        SM (fun (st : state) -> 
            match State.getMem ptr st with
            | Ok v -> Ok (v , st)
            | Error err -> Error err
        )

    let random =
        SM (fun (st : state) ->
            match State.random st with
            | v -> Ok (v, st)
        )
    
    let evalState (st : state) (SM f) =
        match f st with
        | Ok (v, _) -> Ok v
        | Error err -> Error err

    // Red Green exercises
    (*
    type stateContMonad<'a, 'r> =
        SCM of (state ->
               (int -> stateContMonad<unit, 'r>) list ->
               (('a * state) option -> 'r) ->
               'r)
        
    let ret x= SCM (fun st _ f -> f (Some (x, st)))
    let fail     = SCM (fun _  _ f -> f None)
    
    let bind (SCM f : stateContMonad<'a, 'b> ) (g : 'a -> stateContMonad<'c, 'b>)  =
        SCM(fun st rs h ->
            f st rs (function
                     | Some(x, st') ->
                       let (SCM i) = g x
                       i st' rs h
                     | None -> h None))

    
    let callCC f =
        SCM (fun st rs cont ->
                let (SCM g) = f (fun x -> SCM (fun st _ _ -> cont (Some(x, st))))
                g st rs cont)
        
    
    let fret x : stateContMonad<unit, 'a> =
        SCM (fun st rs cont ->
            match rs with
            | c :: rs -> let (SCM f) = c x in f (popFrame st) rs cont
            | []      -> failwith "Should never happen")
        
    let fcall (fname : string) (args : int list) (runBody: stmnt -> stateContMonad<unit, 'a>) : stateContMonad<int, 'a>=
        callCC (fun c -> SCM (fun st rs f -> 
                                  match pushFrame fname args st with
                                  | None -> f None
                                  | Some (st', body) ->
                                       let (SCM g) = runBody body
                                       g st' (c :: rs) (function
                                                        | Some((), st'') -> f (Some(0, st''))
                                                        | None           -> f None)))
                                                        
    *)                                                       
    // Red Yellow exercises
    (*
    type stateContMonad<'a, 'r> =
        SCM of (state ->
               (int -> stateContMonad<unit, 'r>) list ->
               (Result<'a * state, error> -> 'r) ->
               'r)
        
    let ret x       = SCM (fun st _ f -> f (Ok (x, st)))
    let fail err = SCM (fun _  _ f -> f (Error err))
    
    let bind (SCM f : stateContMonad<'a, 'b> ) (g : 'a -> stateContMonad<'c, 'b>)  =
        SCM(fun st rs h ->
            f st rs (function
                     | Ok(x, st') ->
                       let (SCM i) = g x
                       i st' rs h
                     | Error err -> h (Error err)))
        
        
    let callCC f =
        SCM (fun st rs cont ->
                let (SCM g) = f (fun x -> SCM (fun st _ _ -> cont (Ok(x, st))))
                g st rs cont)
        
    
    let fret x : stateContMonad<unit, 'a> =
        SCM (fun st rs cont ->
            match rs with
            | c :: rs -> let (SCM f) = c x in f (popFrame st) rs cont
            | []      -> failwith "Should never happen")

        
    let fcall (fname : string) (args : int list) (runBody: stmnt -> stateContMonad<unit, 'a>) : stateContMonad<int, 'a>=
        callCC (fun c -> SCM (fun st rs f -> 
                                  match pushFrame fname args st with
                                  | Error err -> f (Error err)
                                  | Ok (st', body) ->
                                       let (SCM g) = runBody body
                                       g st' (c :: rs) (function
                                                        | Ok((), st'') -> f (Ok(0, st''))
                                                        | Error err    -> f (Error err))))

    
    
    
    *)
    
    
    let (>>=) a f = bind a f
    let (>>>=) a b = a >>= (fun _ -> b)
    
    let push _ = failwith "not implemented"
    let pop _ = failwith "not implemented"
