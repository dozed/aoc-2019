module Exercise14


open System
open System.Threading
open System.Collections.Generic
open FsAlg.Generic
open Dict
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

type AmountAndElement = { amount: int64; element: string }

type Reaction =  { target: AmountAndElement; source: AmountAndElement list }

let parseAmountAndElement (s:string): AmountAndElement =
    let n::el::_ = List.ofArray (s.Trim().Split(" "))
    { amount=int64 n; element=el }

let parseReaction (line:string): Reaction =
    let a::b::_ = List.ofArray (line.Split([|" => "|], StringSplitOptions.None))
    let cs = List.ofArray (a.Split(",")) |> List.map parseAmountAndElement
    let c = parseAmountAndElement b
    { target=c; source=cs }

let readLines filePath: string list = List.ofSeq(System.IO.File.ReadLines(filePath))

let parseReactionFile filePath: Reaction list =
    readLines filePath |> List.filter (fun x -> not (String.IsNullOrEmpty(x))) |> List.map parseReaction

let findReactionTo (reactions: Reaction list) (targetElement:string): Reaction =
    List.find (fun {target={element=x}} -> x = targetElement) reactions

let findRequired (reactions: AmountAndElement list) (targetElement:string): AmountAndElement =
    List.find (fun {element=x} -> x = targetElement) reactions

let addToRequired (required: AmountAndElement list) ({amount=a; element=x}: AmountAndElement): AmountAndElement list =
    if List.exists (fun {element=y} -> x = y) required then
        List.map (fun {element=y;amount=b} -> if x=y then {element=x;amount=a+b} else {element=y;amount=b}) required
    else
        List.append required [{amount=a; element=x}]
    
let mkTopologicalSort (xs:Reaction list): string list =
    let g = AdjacencyGraph<string, Edge<string>>()

    for {target={element=u;amount=a}; source=xs} in xs do
        g.AddVertex(u)
        
        for {element=v;amount=b} in xs do
            g.AddVertex(v)
            g.AddEdge(Edge<string>(u, v))
            // printfn "%A -> %A" u v

    let t = TopologicalSortAlgorithm<string, Edge<string>>(g)
    
    let vertices = List<string>(g.VertexCount)
    t.Compute(vertices)

    let vertices = List.ofSeq vertices
    vertices

let findAmountOfOreForNFuel (vertices:string list) (reactions:Reaction list) (n:int64): int64 =
    let mutable required: AmountAndElement list = [{amount=n; element="FUEL"}]

    for targetElement in vertices do
        let {source=sources; target={amount=targetAmount}} = findReactionTo reactions targetElement
        let {amount=requiredAmount} = findRequired required targetElement

        let numReactions = int64 (Math.Ceiling (float requiredAmount / float targetAmount))
        
        for {amount=sourceAmount; element=sourceElement} in sources do
            required <- addToRequired required {amount=sourceAmount*numReactions; element=sourceElement}
    
    let {amount=x} = findRequired required "ORE"
    
    x


let exercise14 =

    let reactions: Reaction list = parseReactionFile "input/input14.txt"
    let vertices = mkTopologicalSort reactions |> List.filter (fun x -> x <> "ORE")    

    // part1
    let amountOfOre = findAmountOfOreForNFuel vertices reactions 1L
    printfn "%A" amountOfOre
    
    // part2
    let cargoAmountOfOre = 1000000000000L
    let mutable c = true
    let mutable low = 1L
    let mutable high = 1000000000000L

    while c do
        let amountOfFuel = low + ((high - low) / 2L) 
        let requiredAmountOfOre = findAmountOfOreForNFuel vertices reactions amountOfFuel

        if requiredAmountOfOre > cargoAmountOfOre then
            printfn "amount of ore is too high for amount of fuel: %A FUEL %A ORE (lo: %A, hi: %A)" amountOfFuel requiredAmountOfOre low high
            high <- amountOfFuel
        else if requiredAmountOfOre < cargoAmountOfOre then
            printfn "amount of ore is too low for amount of fuel: %A FUEL %A ORE (lo: %A, hi: %A)" amountOfFuel requiredAmountOfOre low high
            low <- amountOfFuel
        else
            printfn "found amount of fuel for 1T ore: %A" amountOfFuel
            c <- false
        
        ()
    

