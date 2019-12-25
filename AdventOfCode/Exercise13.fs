module Exercise13

open System
open System.Threading
open System.Collections.Generic
open Dict

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
    if address < programSize then
        program.[int address]
    else
        extendedMemory.GetValueOrDefault(address, int64 0)

let writeToMemory (program: int64 []) (extendedMemory: Dictionary<int64, int64>) (address:int64) (value:int64) =
    let programSize = int64 (Array.length program)
    if address < programSize then
        program.[int address] <- value
    else
        if extendedMemory.ContainsKey address then extendedMemory.Remove(address) |> ignore
        extendedMemory.Add(address, value)

let readParameterValue (program: int64 []) (extendedMemory: Dictionary<int64, int64>) (p:int64) (pm:ParameterMode) (relativeBase:int64):int64 =
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
    let (opcode,c,b,a) = parseParameterMode (int rawOpcode)
    // printfn "%A - %A" (opcode,c,b,a) rawOpcode
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
            let idx1 = readParameterAddress program.[eip+1] a relativeBase
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
            output <- List.append output [p1]
            Continue (eip+2)
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


let parseOutput (output:int64 list): Dictionary<int * int, int> =
    let tiles = Dictionary<int * int, int>()
    let mutable i = 0

    while i < List.length output do
        let x = int output.[i]
        let y = int output.[i+1]
        let t = int output.[i+2]
        
        tiles.Remove((x, y))
        tiles.Add((x, y), t)
        
        i <- i + 3
    
    tiles

let countTilesOfType (tiles:Dictionary<int * int, int>) tpe : int = 
    List.filter (fun x -> x = tpe) (List.ofSeq tiles.Values) |> List.length

let drawField (tiles:Dictionary<int * int, int>) frame (ret:StatusCode) = 
    let minX = List.ofSeq tiles.Keys |> List.map fst |> List.min        
    let maxX = List.ofSeq tiles.Keys |> List.map fst |> List.max        
    let minY = List.ofSeq tiles.Keys |> List.map snd |> List.min        
    let maxY = List.ofSeq tiles.Keys |> List.map snd |> List.max        

    for y in 0..maxY do
        for x in 0..maxX do
            Console.SetCursorPosition(x, y)
            if tiles.ContainsKey((x,y)) then
                let t = tiles.GetValueOrDefault((x,y), 0)
                match t with
                | 0 -> printf " " 
                | 1 -> printf "█"
                | 2 -> printf "▒"
                | 3 -> printf "_"
                | 4 -> printf "o"
                ()
            else
                printf " "
        printfn ""

    if tiles.ContainsKey((-1,0)) then
        Console.SetCursorPosition(0, 22)
        let score = tiles.GetValueOrDefault((-1,0), 0)
        printf "score: %A" score
        printfn ""
        
    Console.SetCursorPosition(0, 23)
    printf "frame: %A" frame

    Console.SetCursorPosition(0, 24)
    printf "ret: %A" ret

    Console.SetCursorPosition(0, 25)

let exercise13 =
//    let program = readIntProgram "input/input13.txt"
//    
//    let mutable status = Pause 0
//
//    input <- []
//    output <- []
//    
//    printfn "in : %A" input
//    printfn "out: %A" output
//
//    let ret = run program 0
//    
//    printfn "ret: %A" ret
//    printfn "in : %A" input
//    printfn "out: %A" output
//
//    
//    let tiles = parseOutput output
//    let numBlocks = countTilesOfType tiles 2
//    printfn "num blocks: %A" numBlocks

        
    // ████████████████████████████████████
    // █                                  █
    // █ ▒▒ ▒    ▒▒ ▒▒▒ ▒▒▒▒  ▒ ▒   ▒▒▒▒▒ █
    // █ ▒  ▒▒▒  ▒  ▒ ▒  ▒▒  ▒  ▒▒▒ ▒ ▒▒▒ █
    // █ ▒▒ ▒▒ ▒   ▒ ▒  ▒ ▒  ▒▒▒▒ ▒▒▒   ▒ █
    // █    ▒  ▒▒  ▒▒ ▒ ▒▒ ▒▒▒   ▒▒▒▒ ▒▒▒ █
    // █ ▒       ▒▒▒▒ ▒▒▒▒▒ ▒▒ ▒   ▒▒▒ ▒  █
    // █ ▒ ▒▒▒▒▒  ▒   ▒ ▒▒  ▒ ▒▒▒▒ ▒      █
    // █ ▒▒   ▒   ▒▒ ▒▒▒ ▒▒▒ ▒▒▒▒▒▒▒ ▒▒▒▒ █
    // █  ▒ ▒▒▒▒  ▒▒  ▒  ▒  ▒▒  ▒ ▒▒  ▒ ▒ █
    // █ ▒▒▒▒ ▒ ▒▒ ▒▒▒▒       ▒▒▒▒▒ ▒   ▒ █
    // █ ▒   ▒  ▒  ▒   ▒▒▒▒▒ ▒ ▒ ▒ ▒  ▒▒  █
    // █  ▒ ▒  ▒ ▒ ▒▒▒ ▒ ▒▒▒▒▒  ▒▒▒▒ ▒▒▒  █
    // █ ▒▒▒▒  ▒▒  ▒   ▒  ▒▒  ▒▒ ▒▒ ▒▒▒▒▒ █
    // █ ▒▒▒  ▒▒ ▒▒  ▒▒   ▒▒ ▒▒▒ ▒▒▒▒▒▒▒▒ █
    // █                                  █
    // █               o                  █
    // █                                  █
    // █                                  █
    // █                 _                █
    // █                                  █


    let program = readIntProgram "input/input13.txt"
    
    program.[0] <- 2L
    
    let mutable i = 0
    let mutable padControl = 0L
    let mutable status = Pause 0
    let mutable halted = false

    while true do
        i <- i + 1
        
        input <- [padControl]
        output <- []
        
        let eip =
            match status with
            | Pause eip -> eip
            | Halt -> 0
            
        // board is missing when starting from paused eip
        let eip = 0
        
        status <- run program eip
        let tiles = parseOutput output
        drawField tiles i status

        let ((bx,by),_) = List.find (fun ((x,y),t) -> t = 4) (toList tiles)
        let ((px,py),_) = List.find (fun ((x,y),t) -> t = 3) (toList tiles)
        
        padControl <-
            if bx < px then -1L
            else if bx > px then 1L
            else 0L
        
        Thread.Sleep 5
