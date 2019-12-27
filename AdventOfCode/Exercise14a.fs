module Exercise14a

open System
open System.Threading
open System.Collections.Generic
open FsAlg.Generic
open Dict

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

type AmountAndElement = { amount: int; element: string }

type Reaction =  { target: AmountAndElement; source: AmountAndElement list }

let parseAmountAndElement (s:string): AmountAndElement =
    let n::el::_ = List.ofArray (s.Trim().Split(" "))
    { amount=int n; element=el }

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

let addToRequired (required: AmountAndElement list) ({amount=a; element=x}: AmountAndElement): AmountAndElement list =
    if List.exists (fun {element=y} -> x = y) required then
        List.map (fun {element=y;amount=b} -> if x=y then {element=x;amount=a+b} else {element=y;amount=b}) required
    else
        List.append required [{amount=a; element=x}]
    
let containsWaste (waste: AmountAndElement list) (targetElement:string): bool =
    List.exists (fun {element=x} -> x = targetElement) waste

let findWaste (waste: AmountAndElement list) (targetElement:string): int =
    match List.tryFind (fun {element=x} -> x = targetElement) waste with
    | Some {amount=a} -> a
    | None -> 0

let addToWaste (waste: AmountAndElement list) {amount=a; element=x}: AmountAndElement list =
    if containsWaste waste x then
        List.map (fun {amount=b; element=y} -> if x = y then {amount=a+b; element=x} else {amount=b; element=y}) waste
    else
        {amount=a; element=x} :: waste
    
let replaceWaste (waste: AmountAndElement list) {amount=a; element=x}: AmountAndElement list =
    if containsWaste waste x then
        List.map (fun {amount=b; element=y} -> if x = y then {amount=a; element=x} else {amount=b; element=y}) waste
    else
        {amount=a; element=x} :: waste
    

let exercise14a =

    let reactions: Reaction list = parseReactionFile "input/input14a.txt"

    let mutable numOre = 0
    let mutable waste: AmountAndElement list = []
    let mutable required: AmountAndElement list = []

    required <- [{amount=1; element="FUEL"}]

    while List.length required > 0 do
    // for i in 1..2 do
        let {amount=requiredAmount; element=requiredElement}::otherRequired = required
        printfn "required element: %A with amount: %A" requiredElement requiredAmount
        
        if requiredElement = "ORE" then
            numOre <- numOre + requiredAmount
            required <- otherRequired
            
            printfn "increased amount of ORE by %A" requiredAmount

        else
            let wasteOfElement = findWaste waste requiredElement        
//            printfn "waste of element: %A is: %A" requiredElement wasteOfElement
//            printfn "%A" waste
            
            if wasteOfElement >= requiredAmount then
                let newWaste = wasteOfElement - requiredAmount
                waste <- replaceWaste waste {amount=newWaste; element=requiredElement}    

                printfn "taken from waste: %A" requiredElement
                                
                required <- otherRequired

            else
                let {source=sources; target={amount=targetAmount}} as r = findReactionTo reactions requiredElement
                
                printfn "using reaction: %A" r
                
                let numReactions = int (Math.Ceiling (float requiredAmount / float targetAmount))

                let sourceRequired = List.map
                                         (fun {amount=sourceAmount; element=sourceElement} -> {amount=sourceAmount*numReactions; element=sourceElement})
                                         sources

                let newWaste = targetAmount - requiredAmount
                
                if newWaste > 0 then
                    waste <- addToWaste waste {amount=newWaste; element=requiredElement}    
                
                    printfn "added to waste: %A with amount: %A" requiredElement newWaste

                // printfn "%A" sourceRequired
                
                // required <- List.append sourceRequired rs
                // required <- List.append rs sourceRequired
                required <- List.fold (fun xs s -> addToRequired xs s) otherRequired sourceRequired
                
                ()
            
    printfn "amount of ORE: %A" numOre
    
    0
