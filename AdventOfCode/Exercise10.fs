module Exercise10

open System
open FsAlg.Generic

let ex1 = [
    ".#..#";
    ".....";
    "#####";
    "....#";
    "...##"
]
let ex2 = [
    "......#.#.";
    "#..#.#....";
    "..#######.";
    ".#.#.###..";
    ".#..#.....";
    "..#....#.#";
    "#..#....#.";
    ".##.#..###";
    "##...#..#.";
    ".#....####"
]

let ex = [
    ".#....#####...#..";
    "##...##.#####..##";
    "##...#...#.#####.";
    "..#.....#...###..";
    "..#.#.....#....##"
]

let ex3 = [
    ".#..#..###";
    "####.###.#";
    "....###.#.";
    "..###.##.#";
    "##.##.#.#.";
    "....###..#";
    "..#.#..#.#";
    "#..#.#.###";
    ".##...##.#";
    ".....#.#.."
]

let ex4 = [
    ".#..##.###...#######";
    "##.############..##.";
    ".#.######.########.#";
    ".###.#######.####.#.";
    "#####.##.#.##.###.##";
    "..#####..#.#########";
    "####################";
    "#.####....###.#.#.##";
    "##.#################";
    "#####.##.###..####..";
    "..######..##.#######";
    "####.##.####...##..#";
    ".#####..#.######.###";
    "##...#.##########...";
    "#.##########.#######";
    ".####.#.###.###.#.##";
    "....##.##.###..#####";
    ".#.#.###########.###";
    "#.#.#.#####.####.###";
    "###.##.####.##.#..##"
]

let ex5 = [
    ".###..#......###..#...#";
    "#.#..#.##..###..#...#.#";
    "#.#.#.##.#..##.#.###.##";
    ".#..#...####.#.##..##..";
    "#.###.#.####.##.#######";
    "..#######..##..##.#.###";
    ".##.#...##.##.####..###";
    "....####.####.#########";
    "#.########.#...##.####.";
    ".#.#..#.#.#.#.##.###.##";
    "#..#.#..##...#..#.####.";
    ".###.#.#...###....###..";
    "###..#.###..###.#.###.#";
    "...###.##.#.##.#...#..#";
    "#......#.#.##..#...#.#.";
    "###.##.#..##...#..#.#.#";
    "###..###..##.##..##.###";
    "###.###.####....######.";
    ".###.#####.#.#.#.#####.";
    "##.#.###.###.##.##..##.";
    "##.#..#..#..#.####.#.#.";
    ".#.#.#.##.##########..#";
    "#####.##......#.#.####."
]

let parseMap (xs:string list): Vector<double> list =
    let rows = List.length xs
    let cols = xs.[0].Length
    let mutable vectors = [] 
    
    for y in 0..rows-1 do
        for x in 0..cols-1 do
            if xs.[y].[x] = '#' then
                let v = vector [double x; double y]
                vectors <- v :: vectors

    List.rev vectors

let isOnLine (a:Vector<double>) (b:Vector<double>) (x:Vector<double>) =
    let p = b-a
    let n = vector [-p.[1]; p.[0]]
    if (x-a)*n = 0.0 then true else false

let scale (a:Vector<double>) (b:Vector<double>) (x:Vector<double>) =
    if b.[0] <> a.[0] then
        let s = (x.[0] - a.[0]) / (b.[0] - a.[0])
        s
    else
        let s = (x.[1] - a.[1]) / (b.[1] - a.[1])
        s

let isBlockedBy (a:Vector<double>) (b:Vector<double>) (x:Vector<double>) =
    if isOnLine a b x then
        let s = scale a b x
        if s >= 0.0 && s <= 1.0 then true
        else false
    else false

let isBlockedBy' (a:Vector<double>) (b:Vector<double>) (xs:Vector<double> list) =
    List.exists (fun x -> isBlockedBy a b x) xs

let isEqual (a:Vector<double>) (b:Vector<double>) =
    (a.[0] = b.[0]) && (a.[1] = b.[1])
    
let getViewablePoints (a:Vector<double>) (points:Vector<double> list) = 
    let mutable viewable = []

    let others = List.filter (fun v -> not(isEqual v a)) points
    
    for b in others do
        let possibleBlocks = List.filter (fun v -> not(isEqual v a || isEqual v b)) points
        if not(isBlockedBy' a b possibleBlocks) then
            viewable <- b :: viewable
        else
            //printfn "blocked: %A" b
            ()

    viewable

let cosine (a:Vector<double>) (b:Vector<double>) = a * b / (a.GetL2Norm() * b.GetL2Norm())
let angle (a:Vector<double>) (b:Vector<double>) = 180.0 / Math.PI * Math.Acos (cosine a b)

// https://stackoverflow.com/a/21484228
// https://stackoverflow.com/a/1707251
// https://stackoverflow.com/a/21486462    
let angle' (a:Vector<double>) (b:Vector<double>) =
    let a = Math.Atan2(b.[1], b.[0]) - Math.Atan2(a.[1], a.[0])
    let a = if a < 0.0 then a + 2.0 * Math.PI else a
    let a = 180.0 / Math.PI * a
    (a + 90.0) % 360.0

let contains' (x:Vector<double>) (xs:Vector<double> list) =
    List.exists (fun v -> isEqual v x) xs
   
let rec computeList i (x:Vector<double>) (xs:Vector<double> list) =
    match xs with
    | [] -> ()
    | _ ->
        let viewable = getViewablePoints x xs
        let nonViewable = List.filter (fun v -> not(contains' v viewable)) xs 

        printfn "---"
        let mutable j = i
        let zs = List.map (fun y -> (y, (angle' (vector [0.0;0.0]) (y-x)))) viewable
        for x in List.sortBy snd zs do
            j <- j + 1
            printfn "%A: %A" j x
        computeList j x nonViewable


let exercise10 =
    //    let a = vector [2.0;4.0]
    //    let b = vector [6.0;2.0]
    //    let c = vector [8.0;1.0]
    //    let d = vector [0.0;5.0]
    //
    //    printfn "%A" (isOnLine a c b)    
    //    printfn "%A" (isOnLine a c d)    
    //    printfn "%A" (isOnLine a c (vector [2.0;1.0]))    
    //    
    //    printfn "%A" (scale a c b)    
    //    printfn "%A" (scale a c d)    
    //    printfn "%A" (scale a c (vector [10.0;0.0]))    
    //
    //    printfn "%A" vectors    
    //    let a = vectors.[3]
    //    printfn "%A" (numViewable a vectors)
    //        
    //    let b = vectors.[2]
    //    let x = vectors.[4]
    //    printfn "---"
    //    printfn "%A" (isOnLine a b x)
    //    printfn "%A" (scale a b x)
    //
    //    let a = vector [3.0;6.0]
    //    let b = vector [7.0;5.0]
    //    let c = vector [6.0;3.0]
    //    let d = vector [3.0;1.0]
    //    let e = vector [2.0;3.0]
    //    let x = vector [3.0;3.0]
    //    let u = vector [0.0;1.0]
    //    
    //    let a' = a - x
    //    let b' = b - x
    //    let c' = c - x
    //    let d' = d - x
    //    let e' = e - x
    //
    //    printfn "%A" e'
    //    printfn "%A" (cosine e' u)
    //    printfn "%A" (angle e' u)
    //    printfn "%A" (angle' e' u)



    let points = parseMap ex5

    let xs = List.map (fun x -> (x, List.length (getViewablePoints x points))) points |> List.sortByDescending snd
    for x in xs do
        printfn "%A" x
    printfn "---"

    let x = vector [19.0;11.0]
    printfn "x: %A" x

    let others = List.filter (fun v -> not(isEqual v x)) points
    computeList 0 x others
