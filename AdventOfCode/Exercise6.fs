module Exercise6

open System.Collections
open QuickGraph
open QuickGraph.Algorithms
open QuickGraph.Algorithms.Observers
open QuickGraph.Algorithms.ShortestPath
open System

// https://github.com/FubarDevelopment/QuickGraph/wiki/Shortest-Path

let exampleGraph: AdjacencyGraph<string, IEdge<string>> =
    let g = new AdjacencyGraph<string, IEdge<string>>()
    let nodes = ["COM"; "B"; "C"; "D"; "E"; "F"; "G"; "H"; "I"; "J"; "K"; "L"]
    let edges = [("B", "COM"); ("C", "B"); ("D", "C"); ("E", "D"); ("F", "E"); ("G", "B"); ("H", "G"); ("I", "D"); ("J", "E"); ("K", "J"); ("L", "K")]
    for n in nodes do g.AddVertex(n)
    for (u,v) in edges do
        g.AddEdge(new Edge<string>(u, v))
    g

let exampleGraph': UndirectedGraph<string, IEdge<string>> =
    let g = new UndirectedGraph<string, IEdge<string>>()
    let nodes = ["COM"; "B"; "C"; "D"; "E"; "F"; "G"; "H"; "I"; "J"; "K"; "L"]
    let edges = [("B", "COM"); ("C", "B"); ("D", "C"); ("E", "D"); ("F", "E"); ("G", "B"); ("H", "G"); ("I", "D"); ("J", "E"); ("K", "J"); ("L", "K")]
    for n in nodes do g.AddVertex(n)
    for (u,v) in edges do
        g.AddEdge(new Edge<string>(u, v))
    g


let readLines filePath = List.ofSeq(System.IO.File.ReadLines(filePath))

let parseLine (line: string): string * string =
    let xs = line.Split(')')
    (xs.[0], xs.[1])

let parseFile filePath =
    let xs = readLines filePath
    let ys = List.map parseLine xs
    ys

let readGraph filePath = 
    let edges = parseFile filePath
    printfn "%A" edges
    
    let nodes1 = List.map fst edges |> Set.ofList
    let nodes2 = List.map snd edges |> Set.ofList
    let nodes = Set.union nodes1 nodes2
    
    let g = UndirectedGraph<string, IEdge<string>>()
    for n in nodes do g.AddVertex(n)
    for (v,u) in edges do g.AddEdge(new Edge<string>(u, v))
    
    g


let exercise6 =
    let edges = parseFile "input/input6.txt"
    printfn "%A" edges
    
    let nodes1 = List.map fst edges |> Set.ofList
    let nodes2 = List.map snd edges |> Set.ofList
    let nodes = Set.union nodes1 nodes2
    
    let g = new AdjacencyGraph<string, IEdge<string>>()
    for n in nodes do g.AddVertex(n)
    for (v,u) in edges do g.AddEdge(new Edge<string>(u, v))

    // let g = exampleGraph
    
    let rec pathToRoot (g:AdjacencyGraph<string, IEdge<string>>) (n:string) (xs:string list): string list =
        let out = List.ofSeq(g.OutEdges n) |> List.map (fun e -> e.Target)
        let len = List.length out
        if len = 0 then List.rev xs
        else pathToRoot g (List.head out) (List.head out :: xs)
    
    let sum = List.ofSeq(g.Vertices) |> List.map (fun n -> pathToRoot g n []) |> List.map List.length |> List.sum

    printfn "sum of orbits = %A" sum
    
    // let g' = exampleGraph'
    let g' = readGraph "input/input6.txt"
    let dist = fun (d:IEdge<string>) -> 1.0

    // let res = g'.ShortestPathsDijkstra((Func<_, _> dist), "C")
    // res.
    
    let sp = UndirectedDijkstraShortestPathAlgorithm<string, IEdge<string>>(g', (Func<_,_> dist))        

    let predecessorRecorder = UndirectedVertexPredecessorRecorderObserver<string, IEdge<string>>()
    predecessorRecorder.Attach(sp)
    sp.Compute("YOU")
    let exists, path = predecessorRecorder.TryGetPath "SAN"
    
    if exists then    
        printfn "%A" path
        printfn "%A" (Seq.length path - 2)


