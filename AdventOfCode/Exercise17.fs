module Exercise17

open System
open System.Threading
open System.Collections.Generic
open FsAlg.Generic
open Dict
open ListE
open QuickGraph
open QuickGraph.Algorithms.TopologicalSort

open Exercise1
open Exercise2
open Exercise3
open Exercise4
open Exercise5
open Exercise6
open Exercise7
open Exercise8
open Exercise9
open Exercise10
open Exercise11
open Exercise12
open Exercise13
open Exercise14
open Exercise15

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

type Pos = int*int

type Dir = Up | Down | Left | Right

type Tile = Scaffold | Empty | Bot of Dir

type Move = StaySame | TurnLeft | TurnRight

let printOutput (output:int64 list) =     
    for i in output do
        match i with
        | 46L -> printf "."
        | 35L -> printf "#"
        | 10L -> printfn ""
        | 94L -> printf "^"
        | 60L -> printf "<"
        | 62L -> printf ">"
        | 118L -> printf "v"
        //| x -> printf "%A" x


let mkMap (output:int64 list): Dictionary<Pos, Tile> = 
    let map = Dictionary<Pos, Tile>()

    let mutable x = 0
    let mutable y = 0
    for i in output do
        let tile =
            match i with
            | 46L -> Some(Empty)
            | 35L -> Some(Scaffold)
            | 94L -> Some(Bot Up)
            | 60L -> Some(Bot Left)
            | 62L -> Some(Bot Right)
            | 118L -> Some(Bot Down)
            | 10L -> 
                y <- y + 1
                x <- 0
                None
            //| x -> printf "%A" x
    
        match tile with
        | Some t ->
            map.Add((x,y), t)            
            x <- x + 1
        | None -> ()

    map
let isBot t =
    match t with
    | Bot x -> true
    | _ -> false

let isEmpty t =
    match t with
    | Empty -> true
    | _ -> false

let isNonEmpty t =
    match t with
    | Empty -> false
    | _ -> true

let addDirToPos ((x,y):Pos) (d:Dir): Pos =
    match d with
    | Up -> (x, (y-1))
    | Down -> (x, (y+1))
    | Left -> ((x-1), y)
    | Right -> ((x+1), y) 

let getTile (map:Dictionary<int*int, Tile>) (p:Pos): Tile =
    map.GetValueOrDefault(p, Empty)

let mutable crossPoints = []

let isCrossPoint (map:Dictionary<Pos, Tile>) (pos:Pos): bool =
    let up = getTile map (addDirToPos pos Up)
    let down = getTile map (addDirToPos pos Down)
    let left = getTile map (addDirToPos pos Left)
    let right = getTile map (addDirToPos pos Right)
    let center = getTile map pos
    
    if up = Scaffold && down = Scaffold && left = Scaffold && right = Scaffold && center = Scaffold then true
    else false

let findNext' (map:Dictionary<Pos, Tile>) (botPos:Pos) (botDir:Dir) =
    let nextPos = addDirToPos botPos botDir
    let nextTile = getTile map nextPos
    
    if isEmpty nextTile then None
    else Some nextPos

let findTurn (map:Dictionary<Pos, Tile>) (botPos:Pos) (botDir:Dir): (Pos*Dir*Move) Option =
    match botDir with
    | Up ->
        let leftPos = addDirToPos botPos Left
        let rightPos = addDirToPos botPos Right
        let leftTile = getTile map leftPos
        let rightTile = getTile map rightPos
        
        if isNonEmpty leftTile then Some (leftPos, Left, TurnLeft)
        else if isNonEmpty rightTile then Some (rightPos, Right, TurnRight)
        else None
        
    | Down ->
        let leftPos = addDirToPos botPos Left
        let rightPos = addDirToPos botPos Right
        let leftTile = getTile map leftPos
        let rightTile = getTile map rightPos
        
        if isNonEmpty leftTile then Some (leftPos, Left, TurnRight)
        else if isNonEmpty rightTile then Some (rightPos, Right, TurnLeft)
        else None
        
    | Right ->
        let upPos = addDirToPos botPos Up
        let downPos = addDirToPos botPos Down
        let upTile = getTile map upPos
        let downTile = getTile map downPos
        
        if isNonEmpty upTile then Some (upPos, Up, TurnLeft)
        else if isNonEmpty downTile then Some (downPos, Down, TurnRight)
        else None
        
    | Left ->
        let upPos = addDirToPos botPos Up
        let downPos = addDirToPos botPos Down
        let upTile = getTile map upPos
        let downTile = getTile map downPos
        
        if isNonEmpty upTile then Some (upPos, Up, TurnRight)
        else if isNonEmpty downTile then Some (downPos, Down, TurnLeft)
        else None


let exercise17 =
    
    let program = readIntProgram "input/input17.txt"
    
    let mutable status = Pause 0

    input <- []
    output <- []
    
//    printfn "in : %A" input
//    printfn "out: %A" output

    let ret = run program 0
    
//    printfn "ret: %A" ret
//    printfn "in : %A" input
//    printfn "out: %A" output





    let map = mkMap output

    let width = Seq.map fst map.Keys |> Seq.max           
    let height = Seq.map snd map.Keys |> Seq.max
    
    printfn "width: %A height: %A" width height
        
    for y in 0..height do
        for x in 0..width do
            let pos = (x, y)
                        
            if isCrossPoint map pos then
                printf "O"
                crossPoints <- pos :: crossPoints
            else
                let center = getTile map pos
                match center with
                | Empty -> printf "."
                | Scaffold -> printf "#"
                | Bot Up -> printf "^"
                | Bot Left -> printf "<"
                | Bot Right -> printf ">"
                | Bot Down -> printf "v"
        
        printfn ""
                
    printfn "%A" crossPoints
    
    
    let x = List.map (fun (x, y) -> x*y) crossPoints |> List.sum
    printfn "%A" x
    

    let mutable (botPos, Bot botDir) = List.find (fun (_, t) -> isBot t) (toList map)
    
    printfn "%A - %A" botPos botDir

    let mutable c = true
    let mutable i = 0

    while c do
        match findNext' map botPos botDir with
        | Some nextPos ->
            botPos <- nextPos
            i <- i + 1

        | None ->
            match findTurn map botPos botDir with
            | Some (nextPos,nextDir,move) ->
                printf ",%A" i
                i <- 0

                match move with
                | TurnLeft -> printf ",L"
                | TurnRight -> printf ",R"

                botDir <- nextDir

            | None ->
                printfn ",%A" i
                c <- false
        
                
    // complete program
    // L,6,R,12,L,6,L,8,L,8,
    // L,6,R,12,L,6,L,8,L,8,
    // L,6,R,12,R,8,L,8,
    // L,4,L,4,L,6,
    // L,6,R,12,R,8,L,8,
    // L,6,R,12,L,6,L,8,L,8,
    // L,4,L,4,L,6,
    // L,6,R,12,R,8,L,8,
    // L,4,L,4,L,6,
    // L,6,R,12,L,6,L,8,L,8

    // split into sub-programs
    // A = L,6,R,12,L,6,L,8,L,8
    // B = L,6,R,12,R,8,L,8
    // C = L,4,L,4,L,6
    
    // A,A,B,C,B,A,C,B,C,A

    let program = readIntProgram "input/input17.txt"

    // wake up bot
    program.[0] <- 2L
    
    let toAscii s =
        Seq.toList s |> List.map int64
    
    let xs = toAscii "A,A,B,C,B,A,C,B,C,A" @ [10L]
    let xsA = toAscii "L,6,R,12,L,6,L,8,L,8" @ [10L]
    let xsB = toAscii "L,6,R,12,R,8,L,8" @ [10L]
    let xsC = toAscii "L,4,L,4,L,6" @ [10L]
    let camera = [int64 'n'] @ [10L]
    
    printfn "%A" xs
    printfn "%A" xsA
    printfn "%A" xsB
    printfn "%A" xsC
    
    input <- xs @ xsA @ xsB @ xsC @ camera 
    output <- []
    
    printfn "in : %A" input
    printfn "out: %A" output

    let ret = run program 0
    
    printfn "ret: %A" ret
    printfn "in : %A" input
    printfn "out: %A" output

    printfn "%A" (List.last output)
    
    0