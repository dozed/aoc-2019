module Foo.Bar

open Exercise1
//open Exercise2
open Exercise3
open Exercise4
open Exercise5
open System


[<EntryPoint>]
let main argv =

    // exercise1
    // exercise2
    // exercise3
    // exercise4

    printfn "%A" (parseParameterMode  1002)    
    printfn "%A" (parseParameterMode 11002)    
    printfn "%A" (parseParameterMode 11102)
    
    let program1 = Array.copy initProgram
    run program1 0
    
    0
