module Interpreter.State

    open Result
    open Language
    
    let validVariableName _ = failwith "not implemented"
    let reservedVariableName _ = failwith "not implemented"
    
    type state = unit // your type goes here
    
    let mkState _ = failwith "not implemented"
    let random _ = failwith "not implemented"
    
    let declare _ = failwith "not implemented"
    
    let getVar _ = failwith "not implemented"
    let setVar _ = failwith "not implemented"
    
    let alloc _ = failwith "not implemented"
    let free _ = failwith "not implemented"
    let getMem _ = failwith "not implemented"
    let setMem _ = failwith "not implemented"
    
    let push _ = failwith "not implemented"
    let pop _ = failwith "not implemented"
    
    let pushFrame _ = failwith "not implementd"
    let popFrame _ = failwith "not implemented"