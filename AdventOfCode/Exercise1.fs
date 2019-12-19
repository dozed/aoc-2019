module Exercise1



let readLines filePath = List.ofSeq(System.IO.File.ReadLines(filePath))

let parseLine line = line |> int

let parseFile filePath =
    let xs = readLines filePath
    let ys = List.map parseLine xs
    ys
    
let massToFuel mass =
    int(System.Math.Floor(float(mass) / 3.0) - 2.0)

let rec fuelRequired mass =
    let fuel = massToFuel mass
    if fuel <= 0 then 0
    else fuel + (fuelRequired fuel)

let exercise1 =
    let masses = parseFile "/home/stefan/Code/data/aoc2019/input1.txt"
    let fuels = List.map fuelRequired masses
    let sum = List.sum(fuels)
    printfn "%A" sum
