module Foo.Bar

open Exercise1
open Exercise2
open Exercise3
open Exercise4
open Exercise5
open Exercise6

open System

let intProgram7 = [|3;8;1001;8;10;8;105;1;0;0;21;42;67;88;101;114;195;276;357;438;99999;3;9;101;3;9;9;1002;9;4;9;1001;9;5;9;102;4;9;9;4;9;99;3;9;1001;9;3;9;1002;9;2;9;101;2;9;9;102;2;9;9;1001;9;5;9;4;9;99;3;9;102;4;9;9;1001;9;3;9;102;4;9;9;101;4;9;9;4;9;99;3;9;101;2;9;9;1002;9;3;9;4;9;99;3;9;101;4;9;9;1002;9;5;9;4;9;99;3;9;102;2;9;9;4;9;3;9;1001;9;1;9;4;9;3;9;101;1;9;9;4;9;3;9;1001;9;1;9;4;9;3;9;101;1;9;9;4;9;3;9;1002;9;2;9;4;9;3;9;101;1;9;9;4;9;3;9;1002;9;2;9;4;9;3;9;102;2;9;9;4;9;3;9;1002;9;2;9;4;9;99;3;9;102;2;9;9;4;9;3;9;1002;9;2;9;4;9;3;9;1001;9;1;9;4;9;3;9;1002;9;2;9;4;9;3;9;1002;9;2;9;4;9;3;9;1001;9;2;9;4;9;3;9;1001;9;2;9;4;9;3;9;1001;9;2;9;4;9;3;9;1002;9;2;9;4;9;3;9;101;1;9;9;4;9;99;3;9;102;2;9;9;4;9;3;9;1002;9;2;9;4;9;3;9;1001;9;2;9;4;9;3;9;102;2;9;9;4;9;3;9;1001;9;2;9;4;9;3;9;101;2;9;9;4;9;3;9;1001;9;1;9;4;9;3;9;101;1;9;9;4;9;3;9;101;2;9;9;4;9;3;9;1001;9;1;9;4;9;99;3;9;102;2;9;9;4;9;3;9;101;1;9;9;4;9;3;9;1001;9;1;9;4;9;3;9;101;1;9;9;4;9;3;9;101;1;9;9;4;9;3;9;101;1;9;9;4;9;3;9;1001;9;2;9;4;9;3;9;101;2;9;9;4;9;3;9;1002;9;2;9;4;9;3;9;1001;9;1;9;4;9;99;3;9;1001;9;2;9;4;9;3;9;102;2;9;9;4;9;3;9;1002;9;2;9;4;9;3;9;1002;9;2;9;4;9;3;9;1002;9;2;9;4;9;3;9;1002;9;2;9;4;9;3;9;1002;9;2;9;4;9;3;9;1002;9;2;9;4;9;3;9;101;2;9;9;4;9;3;9;101;2;9;9;4;9;99|]    

let example1 = [|3;15;3;16;1002;16;10;16;1;16;15;15;4;15;99;0;0|]    
let example2 = [|3;23;3;24;1002;24;10;24;1002;23;-1;23;101;5;23;23;1;24;23;23;4;23;99;0;0|]    
let example3 = [|3;31;3;32;1002;32;10;32;1001;31;-2;31;1007;31;0;33;1002;33;7;33;1;33;31;31;1;32;31;31;4;31;99;0;0;0|]

let mutable input: int list = []
let mutable output: int list = []

let parseParameterMode i  =
    let j = i % 10000
    let k = j % 1000
    let a = if i / 10000 = 1 then Immediate else Position 
    let b = if j / 1000 = 1 then Immediate else Position
    let c = if k / 100 = 1 then Immediate else Position
    let d = k % 100
    (d, c, b, a)

let applyOp (program: int []) eip =
    let rawOpcode = program.[eip]
    let (opcode, c,b,a) = parseParameterMode rawOpcode
    match opcode with
        | 1 ->
            // add
            let p1 = program.[eip+1]
            let p2 = program.[eip+2]
            let idx3 = program.[eip+3]
            let p1' = if c = Immediate then p1 else program.[p1]
            let p2' = if b = Immediate then p2 else program.[p2]
            program.[idx3] <- p1' + p2'
            eip+4
        | 2 ->
            // multiply
            let p1 = program.[eip+1]
            let p2 = program.[eip+2]
            let idx3 = program.[eip+3]
            let p1' = if c = Immediate then p1 else program.[p1]
            let p2' = if b = Immediate then p2 else program.[p2]
            program.[idx3] <- p1' * p2'
            eip+4
        | 3 ->
            // input
            let idx1 = program.[eip+1]
            // printfn "Enter int: "
            // let i = int (Console.ReadLine())
            let i = List.head input
            input <- List.tail input
            program.[idx1] <- i
            eip+2
        | 4 ->
            // output
            let p1 = program.[eip+1]
            let o = if c = Immediate then p1 else program.[p1]
            // printfn "output: %A" o
            output <- o :: output
            eip + 2
        | 5 ->
            // jump-if-true
            let p1 = program.[eip+1]
            let p2 = program.[eip+2]
            let p1' = if c = Immediate then p1 else program.[p1]
            let p2' = if b = Immediate then p2 else program.[p2]
            if p1' <> 0 then p2' else eip+3
        | 6 ->
            // jump-if-true
            let p1 = program.[eip+1]
            let p2 = program.[eip+2]
            let p1' = if c = Immediate then p1 else program.[p1]
            let p2' = if b = Immediate then p2 else program.[p2]
            if p1' = 0 then p2' else eip+3
        | 7 ->
            // less-than
            let p1 = program.[eip+1]
            let p2 = program.[eip+2]
            let idx3 = program.[eip+3]
            let p1' = if c = Immediate then p1 else program.[p1]
            let p2' = if b = Immediate then p2 else program.[p2]
            if p1' < p2' then
                program.[idx3] <- 1
            else
                program.[idx3] <- 0
            eip+4
        | 8 ->
            // equals
            let p1 = program.[eip+1]
            let p2 = program.[eip+2]
            let idx3 = program.[eip+3]
            let p1' = if c = Immediate then p1 else program.[p1]
            let p2' = if b = Immediate then p2 else program.[p2]
            if p1' = p2' then
                program.[idx3] <- 1
            else
                program.[idx3] <- 0
            eip+4
        | 99 -> eip
        | _ -> eip

let rec run (program: int []) eip =
    let opcode = program.[eip]
    if opcode = 99 then
        ()
    else
        let eip' = applyOp program eip
        run program eip'

let permutations xs = 
    let rec insert x = function
        | [] -> [[x]]
        | head :: tail -> (x :: (head :: tail)) :: (List.map (fun l -> head :: l) (insert x tail))
    List.fold (fun s e -> List.collect (insert e) s) [[]] xs

let computeOutputSignal amplifierProgram phaseSettings = 
    let mutable currentOut = 0
    
    for (phaseSetting,i) in List.zip phaseSettings [1..5] do
        input <- [phaseSetting; currentOut]
        output <- []

        // printfn " in[%A]: %A" i input
        let module1 = Array.copy amplifierProgram
        run module1 0

        currentOut <- List.head output
        input <- []
        output <- []
        // printfn "out[%A]: %A" i currentOut
        
    currentOut


[<EntryPoint>]
let main argv =

    // exercise1
    // exercise2
    // exercise3
    // exercise4
    // exercise5
    // exercise6
    
    
    let xs = permutations [0;1;2;3;4]
    printfn "%A" (List.length xs)
    
    //    let phaseSettings = [4;3;2;1;0]
    //    let phaseSettings = [0;1;2;3;4]
    //    let phaseSettings = [1;0;4;3;2]

    let ys = List.map (fun phaseSetting -> (phaseSetting, computeOutputSignal intProgram7 phaseSetting)) xs |> List.sortBy snd
    
    for y in ys do
        printfn "%A" y
    
    
    0
