module Exercise7

open System

let intProgram7 = [|3;8;1001;8;10;8;105;1;0;0;21;42;67;88;101;114;195;276;357;438;99999;3;9;101;3;9;9;1002;9;4;9;1001;9;5;9;102;4;9;9;4;9;99;3;9;1001;9;3;9;1002;9;2;9;101;2;9;9;102;2;9;9;1001;9;5;9;4;9;99;3;9;102;4;9;9;1001;9;3;9;102;4;9;9;101;4;9;9;4;9;99;3;9;101;2;9;9;1002;9;3;9;4;9;99;3;9;101;4;9;9;1002;9;5;9;4;9;99;3;9;102;2;9;9;4;9;3;9;1001;9;1;9;4;9;3;9;101;1;9;9;4;9;3;9;1001;9;1;9;4;9;3;9;101;1;9;9;4;9;3;9;1002;9;2;9;4;9;3;9;101;1;9;9;4;9;3;9;1002;9;2;9;4;9;3;9;102;2;9;9;4;9;3;9;1002;9;2;9;4;9;99;3;9;102;2;9;9;4;9;3;9;1002;9;2;9;4;9;3;9;1001;9;1;9;4;9;3;9;1002;9;2;9;4;9;3;9;1002;9;2;9;4;9;3;9;1001;9;2;9;4;9;3;9;1001;9;2;9;4;9;3;9;1001;9;2;9;4;9;3;9;1002;9;2;9;4;9;3;9;101;1;9;9;4;9;99;3;9;102;2;9;9;4;9;3;9;1002;9;2;9;4;9;3;9;1001;9;2;9;4;9;3;9;102;2;9;9;4;9;3;9;1001;9;2;9;4;9;3;9;101;2;9;9;4;9;3;9;1001;9;1;9;4;9;3;9;101;1;9;9;4;9;3;9;101;2;9;9;4;9;3;9;1001;9;1;9;4;9;99;3;9;102;2;9;9;4;9;3;9;101;1;9;9;4;9;3;9;1001;9;1;9;4;9;3;9;101;1;9;9;4;9;3;9;101;1;9;9;4;9;3;9;101;1;9;9;4;9;3;9;1001;9;2;9;4;9;3;9;101;2;9;9;4;9;3;9;1002;9;2;9;4;9;3;9;1001;9;1;9;4;9;99;3;9;1001;9;2;9;4;9;3;9;102;2;9;9;4;9;3;9;1002;9;2;9;4;9;3;9;1002;9;2;9;4;9;3;9;1002;9;2;9;4;9;3;9;1002;9;2;9;4;9;3;9;1002;9;2;9;4;9;3;9;1002;9;2;9;4;9;3;9;101;2;9;9;4;9;3;9;101;2;9;9;4;9;99|]    

let example1 = [|3;15;3;16;1002;16;10;16;1;16;15;15;4;15;99;0;0|]    
let example2 = [|3;23;3;24;1002;24;10;24;1002;23;-1;23;101;5;23;23;1;24;23;23;4;23;99;0;0|]    
let example3 = [|3;31;3;32;1002;32;10;32;1001;31;-2;31;1007;31;0;33;1002;33;7;33;1;33;31;31;1;32;31;31;4;31;99;0;0;0|]

let mutable input: int list = []
let mutable output: int list = []


type StatusCode = Continue of int | Pause of int | Halt | Error

type ParameterMode = Position | Immediate

type OpCode = Add of ParameterMode * ParameterMode | Multiply of ParameterMode * ParameterMode | Input | Output | JumpIfTrue | JumpIfFalse | LessThan | Equals | DoHalt

let parseOpCode i: OpCode option =
    match i with
    | 1 -> Some (Add(Position, Position))
    | 2 -> Some (Multiply(Position, Position))
    | 3 -> Some Input
    | 4 -> Some Output
    | 5 -> Some JumpIfTrue
    | 6 -> Some JumpIfFalse
    | 7 -> Some LessThan
    | 8 -> Some Equals
    | 9 -> Some DoHalt
    | _ -> None

let parseParameterMode i  =
    let j = i % 10000
    let k = j % 1000
    let a = if i / 10000 = 1 then Immediate else Position 
    let b = if j / 1000 = 1 then Immediate else Position
    let c = if k / 100 = 1 then Immediate else Position
    let d = k % 100
    (d, c, b, a)


let applyOp (program: int []) eip: StatusCode =
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
            Continue (eip+4)
        | 2 ->
            // multiply
            let p1 = program.[eip+1]
            let p2 = program.[eip+2]
            let idx3 = program.[eip+3]
            let p1' = if c = Immediate then p1 else program.[p1]
            let p2' = if b = Immediate then p2 else program.[p2]
            program.[idx3] <- p1' * p2'
            Continue (eip+4)
        | 3 ->
            // input
            let idx1 = program.[eip+1]
            // printfn "Enter int: "
            // let i = int (Console.ReadLine())
            // printfn "in' : %A" input
            if List.length input > 0 then
                let i = List.head input
                input <- List.tail input
                program.[idx1] <- i
                Continue (eip+2)
            else
                // pause program if there is no input available
                Pause eip
        | 4 ->
            // output
            let p1 = program.[eip+1]
            let o = if c = Immediate then p1 else program.[p1]
            // printfn "output: %A" o
            output <- o :: output
            Continue (eip + 2)
        | 5 ->
            // jump-if-true
            let p1 = program.[eip+1]
            let p2 = program.[eip+2]
            let p1' = if c = Immediate then p1 else program.[p1]
            let p2' = if b = Immediate then p2 else program.[p2]
            if p1' <> 0 then Continue p2'
            else Continue (eip+3)
        | 6 ->
            // jump-if-false
            let p1 = program.[eip+1]
            let p2 = program.[eip+2]
            let p1' = if c = Immediate then p1 else program.[p1]
            let p2' = if b = Immediate then p2 else program.[p2]
            if p1' = 0 then Continue p2'
            else Continue (eip+3)
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
            Continue (eip+4)
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
            Continue (eip+4)
        | 99 -> Halt
        | _ -> Error

let rec run program eip: StatusCode =
    match applyOp program eip with
    | Continue eip' -> run program eip'
    | Pause eip' -> Pause eip'
    | Halt -> Halt
    | Error -> raise (InvalidOperationException("invalid operation"))

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

let exercise7 =
    // let xs = permutations [0;1;2;3;4]
    let xs = permutations [5;6;7;8;9]
    printfn "%A" (List.length xs)
    
    //    let phaseSettings = [4;3;2;1;0]
    //    let phaseSettings = [0;1;2;3;4]
    //    let phaseSettings = [1;0;4;3;2]

    //    let ys = List.map (fun phaseSetting -> (phaseSetting, computeOutputSignal intProgram7 phaseSetting)) xs |> List.sortBy snd
    //    
    //    for y in ys do
    //        printfn "%A" y

    let example4 = [|3;26;1001;26;-4;26;3;27;1002;27;2;27;1;27;26;27;4;27;1001;28;-1;28;1005;28;6;99;0;0;5|]
    let phaseSettings = [9;8;7;6;5] 

    let mutable amp = 0
    let amplifierProgram = intProgram7
    
    for phaseSettings in xs do
        let module1 = Array.copy amplifierProgram
        let module2 = Array.copy amplifierProgram
        let module3 = Array.copy amplifierProgram
        let module4 = Array.copy amplifierProgram
        let module5 = Array.copy amplifierProgram
            
        let modules = [module1;module2;module3;module4;module5]
        
        let mutable currentOut = 0
        
        let statuses = [|Pause 0;Pause 0;Pause 0;Pause 0;Pause 0|]
        
        let isHaltStatus status =
            match status with
            | Halt -> true
            | _ -> false
        
        let checkAllModulesHalted (statuses: StatusCode []) = Array.forall isHaltStatus statuses

        let mutable it = 0
            
        while not (checkAllModulesHalted statuses) do
            for (phaseSetting,m,i) in List.zip3 phaseSettings modules [0..4] do
                if it = 0 then
                    input <- [phaseSetting; currentOut]
                else
                    input <- [currentOut]
                output <- []
                printfn "in [%A]: %A" i input

                let eip =
                    match statuses.[i] with
                    | Pause eip -> eip
                    | Halt -> raise (InvalidProgramException("halt not "))
                
                let ret = run m eip
                statuses.[i] <- ret
                printfn "ret[%A]: %A" i ret

                currentOut <- List.head output
                input <- []
                output <- []
                printfn "out[%A]: %A" i currentOut
                
            it <- it + 1

        if currentOut > amp then
            amp <- currentOut
            printfn "%A" currentOut


    printf "%A" amp


