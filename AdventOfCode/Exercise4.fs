module Exercise4

let exercise4 = 
    let checkAdjacentDigitsSame x =
        Seq.toList (string x) |> List.pairwise |> List.exists (fun (a,b) -> a = b)

    let checkDigitsIncreasing x =
        Seq.toList (string x) |> List.map int |> List.pairwise |> List.forall (fun (a,b) -> b - a >= 0)

    let checkPassword x = checkAdjacentDigitsSame x && checkDigitsIncreasing x
    
    let validPasswords = List.where checkPassword [124075..580769] |> List.length
    
    printfn "%A" (checkAdjacentDigitsSame 123123)
    printfn "%A" (checkAdjacentDigitsSame 123323)
        
    printfn "%A" (checkDigitsIncreasing 123123)
    printfn "%A" (checkDigitsIncreasing 123446)
    
    printfn "%A" (validPasswords)    

    let checkAdjacentDigitsSame' (x:int) =
        let y = string x
        let z = Seq.toList y
        let a = List.groupBy id z
        let b = List.map (fun (k,v) -> List.length v) a
        let c = List.exists (fun v -> v = 2) b
        c

    let checkPassword' x = checkAdjacentDigitsSame' x && checkDigitsIncreasing x

    let validPasswords' = List.where checkPassword' [124075..580769] |> List.length

    printfn "%A" (checkAdjacentDigitsSame' 112233)
    printfn "%A" (checkAdjacentDigitsSame' 123444)
    printfn "%A" (checkAdjacentDigitsSame' 111122)
    
    printfn "%A" (validPasswords')    

