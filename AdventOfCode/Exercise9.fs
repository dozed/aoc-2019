module Exercise9

open System
open System.Collections.Generic

let mutable input: int64 list = []

let mutable output: int64 list = []

let mutable relativeBase: int64 = int64 0

let extendedMemory: Dictionary<int64, int64> = Dictionary<int64, int64>()

type StatusCode = Continue of int | Pause of int | Halt | Error of string

type ParameterMode = Position | Immediate | Relative

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

let parameterModeFromInt i =
    match i with
    | 0 -> Position
    | 1 -> Immediate
    | 2 -> Relative

let parseParameterMode i  =
    let j = i % 10000
    let k = j % 1000
    let a = parameterModeFromInt (i / 10000) 
    let b = parameterModeFromInt (j / 1000)
    let c = parameterModeFromInt (k / 100)
    let d = k % 100
    (d, c, b, a)

let readFromMemory (program: int64 []) (extendedMemory: Dictionary<int64, int64>) (address:int64) =
    let programSize = int64 (Array.length program)
    if address < programSize then program.[int address]
    else
        let newAddress = int64 (address - programSize)
        extendedMemory.GetValueOrDefault(newAddress, int64 0)

let writeToMemory (program: int64 []) (extendedMemory: Dictionary<int64, int64>) (address:int64) (value:int64) =
    let programSize = int64 (Array.length program)
    if address < programSize then program.[int address] <- value
    else
        let newAddress = int64 (address - programSize)
        if extendedMemory.ContainsKey newAddress then extendedMemory.Remove(newAddress) |> ignore
        extendedMemory.Add(newAddress, value)

let readParameterValue (program: int64 []) (extendedMemory: Dictionary<int64, int64>)(p:int64) (pm:ParameterMode) (relativeBase:int64):int64 =
    match pm with
    | Immediate -> p
    | Position -> readFromMemory program extendedMemory p
    | Relative -> readFromMemory program extendedMemory (relativeBase+p) 

let readParameterAddress (p:int64) (pm:ParameterMode) (relativeBase:int64):int64 =
    match pm with
    | Immediate -> raise (InvalidProgramException("address cannot be read from immediate"))
    | Position -> p
    | Relative -> relativeBase + p 


let applyOp (program: int64 []) eip: StatusCode =
    let rawOpcode = program.[eip]
    let (opcode, c,b,a) = parseParameterMode (int rawOpcode)
    match opcode with
        | 1 ->
            // add
            let p1 = readParameterValue program extendedMemory program.[eip+1] c relativeBase
            let p2 = readParameterValue program extendedMemory program.[eip+2] b relativeBase
            let idx3 = readParameterAddress program.[eip+3] a relativeBase
            writeToMemory program extendedMemory idx3 (p1 + p2)
            Continue (eip+4)
        | 2 ->
            // multiply
            let p1 = readParameterValue program extendedMemory program.[eip+1] c relativeBase
            let p2 = readParameterValue program extendedMemory program.[eip+2] b relativeBase
            let idx3 = readParameterAddress program.[eip+3] a relativeBase
            writeToMemory program extendedMemory idx3 (p1 * p2)
            Continue (eip+4)
        | 3 ->
            // input
            let idx1 = readParameterAddress program.[eip+1] c relativeBase
            // let idx1 = program.[eip+1]
            // printfn "Enter int: "
            // let i = int (Console.ReadLine())
            // printfn "in' : %A" input
            if List.length input > 0 then
                let i = List.head input
                input <- List.tail input
                writeToMemory program extendedMemory idx1 i
                Continue (eip+2)
            else
                // pause program if there is no input available
                Pause eip
        | 4 ->
            // output
            let p1 = readParameterValue program extendedMemory program.[eip+1] c relativeBase
            // printfn "output: %A" o
            output <- p1 :: output
            Continue (eip + 2)
        | 5 ->
            // jump-if-true
            let p1 = readParameterValue program extendedMemory program.[eip+1] c relativeBase
            let p2 = readParameterValue program extendedMemory program.[eip+2] b relativeBase
            if p1 <> 0L then Continue (int p2)
            else Continue (eip+3)
        | 6 ->
            // jump-if-false
            let p1 = readParameterValue program extendedMemory program.[eip+1] c relativeBase
            let p2 = readParameterValue program extendedMemory program.[eip+2] b relativeBase
            if p1 = 0L then Continue (int p2)
            else Continue (eip+3)
        | 7 ->
            // less-than
            let p1 = readParameterValue program extendedMemory program.[eip+1] c relativeBase
            let p2 = readParameterValue program extendedMemory program.[eip+2] b relativeBase
            let idx3 = readParameterAddress program.[eip+3] a relativeBase
            let v = if p1 < p2 then 1L else 0L
            writeToMemory program extendedMemory idx3 v
            Continue (eip+4)
        | 8 ->
            // equals
            let p1 = readParameterValue program extendedMemory program.[eip+1] c relativeBase
            let p2 = readParameterValue program extendedMemory program.[eip+2] b relativeBase
            let idx3 = readParameterAddress program.[eip+3] a relativeBase
            let v = if p1 = p2 then 1L else 0L
            writeToMemory program extendedMemory idx3 v
            Continue (eip+4)
        | 9 ->
            let p1 = readParameterValue program extendedMemory program.[eip+1] c relativeBase
            relativeBase <- relativeBase + p1
            Continue (eip+2)
        | 99 -> Halt
        | x -> Error (sprintf "invalid operation %A" x)

let rec run program eip: StatusCode =
    match applyOp program eip with
    | Continue eip' -> run program eip'
    | Pause eip' -> Pause eip'
    | Halt -> Halt
    | Error msg -> Error msg

let permutations xs = 
    let rec insert x = function
        | [] -> [[x]]
        | head :: tail -> (x :: (head :: tail)) :: (List.map (fun l -> head :: l) (insert x tail))
    List.fold (fun s e -> List.collect (insert e) s) [[]] xs



let readLines filePath = List.ofSeq(System.IO.File.ReadLines(filePath))

let readIntProgram filePath =
    let line::_ = readLines filePath
    let xs = line.Split(',') |> Array.map int64
    xs



let exercise9 =
    let x: int64 = 1125899906842624L
    
    printfn "%A" x

    input <- [2L]
    output <- []
    printfn "in : %A" input
    printfn "out: %A" output

    let m = [|109;1;204;-1;1001;100;1;100;1008;100;16;101;1006;101;0;99|] |> Array.map int64
    let m2 = [|1102;34915192;34915192;7;4;7;99;0|] |> Array.map int64
    let m3 = [|104L;1125899906842624L;99L|]
    let m4 = readIntProgram "input/input9.txt"
    
    let eip = 0    
    let ret = run m4 eip
    printfn "ret: %A" ret
    printfn "in : %A" input
    printfn "out: %A" output

//    let m = [|109;1;204;-1;1001;100;1;100;1008;100;16;101;1006;101;0;99|]
//    let s = Array.length m
//    writeToMemory m extendedMemory (s-1) 42
//    writeToMemory m extendedMemory s 42
//    printfn "%A" m
//    printfn "%A" extendedMemory
