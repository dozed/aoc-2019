module Foo.Bar

open System
open System.Collections.Generic
open FsAlg.Generic

open Exercise1
open Exercise2
open Exercise3
open Exercise4
open Exercise5
open Exercise6
open Exercise7
open Exercise8
open Exercise9

[<EntryPoint>]
let main argv =

    // exercise1
    // exercise2
    // exercise3
    // exercise4
    // exercise5
    // exercise6
    // exercise7    
    // exercise8
    // exercise9
    

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
    
    let xs = [
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
        
    let numViewable (a:Vector<double>) (points:Vector<double> list) = 
        let mutable viewable = 0
    
        let others = List.filter (fun v -> not(isEqual v a)) points
        
        for b in others do
            let possibleBlocks = List.filter (fun v -> not(isEqual v a || isEqual v b)) points
            if not(isBlockedBy' a b possibleBlocks) then
                viewable <- viewable + 1
            else
                //printfn "blocked: %A" b
                ()

        viewable


    let vectors = parseMap ex5
    
    // printfn "%A" vectors    
    //    let a = vectors.[3]
    //    printfn "%A" (numViewable a vectors)
    //        
    //    let b = vectors.[2]
    //    let x = vectors.[4]
    //    printfn "---"
    //    printfn "%A" (isOnLine a b x)
    //    printfn "%A" (scale a b x)

    printfn "---"
    let xs = List.map (fun v -> (v, numViewable v vectors)) vectors |> List.sortByDescending snd
    for x in xs do
        printfn "%A" x
    
    0
