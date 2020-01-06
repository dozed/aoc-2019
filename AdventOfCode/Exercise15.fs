module Exercise15

open System
open System.Threading
open System.Collections.Generic
open Dict
open ListE

type Program = int64[]

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

type Dir = North | South | West | East

let directions = [North; South; East; West]

let dirToInt d =
    match d with
    | North -> 1L 
    | South -> 2L
    | West -> 3L
    | East -> 4L
    
let intToDir i =
    match i with
    | 1L -> North
    | 2L -> South
    | 3L -> West
    | 4L -> East

type Tile = Wall | Empty | Oxygen

let intToTile i =
    match i with
    | 0L -> Wall
    | 1L -> Empty
    | 2L -> Oxygen

let tileIsOk i =
    match i with
    | Wall -> false
    | Empty -> true
    | Oxygen -> true

let tileIsOxygen i =
    match i with
    | Wall -> false
    | Empty -> false
    | Oxygen -> true

type Path = Dir list

type Pos = int*int

let runPath (program:Program) (path: Path): Tile =
    input <- List.map dirToInt path
    output <- []
    
    let eip = 0
    let status = run program eip
    
    intToTile (List.last output)

let runPath' (p: Program) (d: Dir): Tile =
    input <- [dirToInt d]
    output <- []
    
    let eip = 0
    let status = run p eip
    
    intToTile (List.last output)

let addDirToPosition ((x,y):Pos) (d:Dir): Pos =
    match d with
    | North -> (x,y+1)
    | South -> (x,y-1)
    | East -> (x+1,y)
    | West -> (x-1,y)

let rec pathToPosition ((x,y):Pos) (p:Path): Pos =
    match p with
    | [] -> (x,y)
    | d::ps -> pathToPosition (addDirToPosition (x,y) d) ps

let rec stepBfs (program:Program) (paths: Path list) =
    let possibleFurtherSteps = [North; South; East; West]
    let positions = List.map (pathToPosition (0,0)) paths |> Set.ofList

    let newPaths: Path list = List.map (fun p -> List.map (fun d -> d :: p) possibleFurtherSteps) paths |> List.concat
    let nonWallPaths: Path list = List.filter (fun p -> runPath program p |> tileIsOk) newPaths
    let nonCycleNonWallPaths = List.filter (fun p -> not(Set.contains (pathToPosition (0,0) p) positions)) nonWallPaths

    let oxygenPaths: Path list = List.filter (fun p -> runPath program p |> tileIsOxygen) nonCycleNonWallPaths

    if nonEmpty oxygenPaths then
        printfn "found oxygen path: %A" oxygenPaths
    else
        printfn "new paths: %A" (List.length newPaths)
        printfn "non wall paths: %A" (List.length nonWallPaths)
        printfn "check paths: %A" (List.length nonCycleNonWallPaths)
        stepBfs program nonCycleNonWallPaths

let rec stepBfs' (paths: (Path*Program*Tile) list) =
    let possibleFurtherSteps = [North; South; East; West]
    let positions = List.map (fun (p,_,_) -> pathToPosition (0,0) p) paths |> Set.ofList

    let runStep (d:Dir) (p:Path) (pr:Program): (Path*Program*Tile) =
        let pr' = Array.copy(pr)
        let ot = runPath' pr' d
        (d::p, pr', ot)
    
    let newPaths: (Path*Program*Tile) list =
        List.map (fun (p,pr,_) ->
            List.map (fun d -> runStep d p pr) possibleFurtherSteps) paths |> List.concat
        
    let nonWallPaths: (Path*Program*Tile) list = List.filter (fun (_,_,t) -> tileIsOk t) newPaths

    let nonCycleNonWallPaths: (Path*Program*Tile) list =
        List.filter (fun (p,_,_) -> not(Set.contains (pathToPosition (0,0) p) positions)) nonWallPaths

    let oxygenPaths: (Path*Program*Tile) list = List.filter (fun (_,_,t) -> tileIsOxygen t) nonCycleNonWallPaths

    if nonEmpty oxygenPaths then
        printfn "found oxygen path: %A" oxygenPaths
    else
        printfn "new paths: %A" (List.length newPaths)
        printfn "non wall paths: %A" (List.length nonWallPaths)
        printfn "check paths: %A" (List.length nonCycleNonWallPaths)
        stepBfs' nonCycleNonWallPaths

let rec stepDfs (map:Dictionary<Pos, Tile>) (program: int64[]) (path:Path) (pos:Pos) =
    for d in directions do
        let pos' = addDirToPosition pos d
        
        if map.ContainsKey(pos') then
            // printfn "cycle %A" path
            ()
        else
            let program' = Array.copy program
            let t = runPath' program' d
            
            map.Add(pos', t)

            match t with
            | Empty -> stepDfs map program' (d::path) pos'
            | Wall -> ()
            | Oxygen ->
                printfn "oxygen %A %A %A" (List.length (d::path)) d pos'
                stepDfs map program' (d::path) pos'
    
    ()

let getBoundaries (map:Dictionary<Pos, Tile>): (int*int*int*int) =
    let minX = List.ofSeq map.Keys |> List.map fst |> List.min
    let maxX = List.ofSeq map.Keys |> List.map fst |> List.max
    let minY = List.ofSeq map.Keys |> List.map snd |> List.min
    let maxY = List.ofSeq map.Keys |> List.map snd |> List.max

    (minX, maxX, minY, maxY)

let normalizeMap (map:Dictionary<Pos, Tile>):Dictionary<Pos, Tile> = 
    let (minX, maxX, minY, maxY) = getBoundaries map
    // printfn "%A - %A - %A - %A" minX maxX minY maxY
    let map = toList map |> List.map (fun ((x,y),t) -> ((x-minX, y-minY), t)) |> ofList
    map
        
let drawMap (map:Dictionary<Pos, Tile>): unit = 
    let (minX, maxX, minY, maxY) = getBoundaries map        

    for y in minY..maxY do
        for x in minX..maxX do
            let t = map.GetValueOrDefault((x,y), Wall)
            Console.SetCursorPosition(x, maxY-y)
            
            match t with
            | Empty -> printf " "
            | Wall -> printf "█"
            | Oxygen -> printf "O"
        printfn ""

let isOxygen t =
    match t with
    | Oxygen -> true
    | _ -> false

let adjacents (map:Dictionary<Pos, Tile>) (p:Pos): Pos Set =
    let mutable xs = Set.empty
    
    for d in directions do
        let p' = addDirToPosition p d
        let t' = map.GetValueOrDefault(p', Wall)
        if tileIsOk t' then xs <- Set.add p' xs
    
    xs

let exercise15 =
    let program = readIntProgram "input/input15.txt"

    //    input <- List.map dirToInt [West; West; North; North; North; North]
    //    output <- []
    //    
    //    let eip = 0
    //    let status = run program eip
    //    
    //    printfn "%A" status
    //    printfn "%A" output
    //
    //    printfn "%A" extendedMemory

    // stepBfs' [([],program,Empty)]
    // stepBfs [[]]
    
    let map = Dictionary<Pos, Tile>()
    map.Add((0,0), Empty)


    // part 1: find oxygen
    stepDfs map program [] (0,0) 


    // part 2: fill oxygen
    let (minX', maxX', minY', maxY') = getBoundaries map
    let map = normalizeMap map
    let (minX, maxX, minY, maxY) = getBoundaries map
    drawMap map
    Console.SetCursorPosition(0-minX', maxY-(0-minY'))
    printf "X"

    let p = List.find (fun (_,t) -> isOxygen t) (toList map) |> fst
    let mutable oxygenFilled: Pos Set = Set.ofList [p]
    // printfn "%A" oxygenFilled
    // printfn "%A" (adjacents map p)
    
    let mutable c = true
    let mutable i = 0

    while c do
        i <- i + 1
        let xs = Set.map (fun f -> adjacents map f) oxygenFilled |> Set.unionMany
        
        if Set.count xs = 0 then c <- false
        else
            let newPoints = Set.difference xs oxygenFilled
            for (x,y) in newPoints do
                Console.SetCursorPosition(x,maxY-y)
                printf "O"
                ()
            oxygenFilled <- Set.union oxygenFilled xs
            Console.SetCursorPosition(0,0)
            printf "it %A: %A" i (Set.count oxygenFilled)
            Thread.Sleep 200
            ()


