module Exercise11

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

type Turn = TurnRight | TurnLeft

type Direction = Up | Down | Right | Left

type Color = White | Black

let mkTurn x : Turn =
    match x with
    | 0L -> TurnLeft
    | 1L -> TurnRight

let mkColor x : Color =
    match x with
    | 0L -> Black
    | 1L -> White

let colorToInt x =
    match x with
    | Black -> 0L
    | White -> 1L

let setPanel (panels:Dictionary<int * int, Color>) p c : unit =
    panels.Remove p
    panels.Add(p, c)
 
let getPanel (panels:Dictionary<int * int, Color>) p : Color =
    panels.GetValueOrDefault(p, Black)
   
let getNewPos ((x,y): int*int) (dir:Direction) : int*int =
    match dir with
    | Up -> (x, y+1)
    | Down -> (x, y-1)
    | Right -> (x+1, y)
    | Left -> (x-1, y)

let getNewDirection (d:Direction) (t:Turn) : Direction =
    match d with
    | Up ->
        match t with
        | TurnLeft -> Left
        | TurnRight -> Right
    | Down ->
        match t with
        | TurnLeft -> Right
        | TurnRight -> Left
    | Right ->
        match t with
        | TurnLeft -> Up
        | TurnRight -> Down
    | Left ->
        match t with
        | TurnLeft -> Down
        | TurnRight -> Up


let exercise11 =
    
    let program = readIntProgram "input/input11.txt"
    
    let mutable status = Pause 0

    input <- [1L]
    output <- []
    
    let mutable pos = (0,5)
    let mutable direction = Up
    let panels = Dictionary<int * int, Color>()
    
    while status <> Halt do
        printfn "in : %A" input
        printfn "out: %A" output

        let eip =
            match status with
            | Pause eip -> eip

        let ret = run program eip
        
        printfn "ret: %A" ret
        printfn "in : %A" input
        printfn "out: %A" output
        
        status <- ret
        
        if status <> Halt then
            let color = mkColor output.[0]
            let turn = mkTurn output.[1]
            
            printfn "%A - %A" color turn
            
            setPanel panels pos color
            direction <- getNewDirection direction turn
            pos <- getNewPos pos direction
            let c = getPanel panels pos
            
            output <- []
            input <- [colorToInt c]
        
        printfn "---"
    
    printfn "num panels: %A" panels.Keys.Count
    
    let coords = List.ofSeq panels.Keys
    
    let minX = coords |> List.minBy fst
    let minY = coords |> List.minBy snd
    let maxX = coords |> List.maxBy fst
    let maxY = coords |> List.maxBy snd
    printfn "%A - %A / %A - %A" minX minY maxX maxY

    for y in 0..5 do
        for x in 0..42 do
            let c = panels.GetValueOrDefault((x,5-y), Black)
            if c = Black then printf(" ")
            else printf("█")
        printfn("")

    printfn "%A" panels
            


