module Module1

let lift2 f a b =
    match a, b with
    | Some x, Some y -> Some (f x y)
    | _ -> None

let apply fOpt xOpt =
    lift2 (fun f x -> f x) fOpt xOpt

let squareInt x = x * x

let app1 = 
    let x = lift2 (fun x -> fun y -> x * y) (Some 2) (Some 3)
    printfn "x: %A" x
    
    let x2 = lift2 (fun x -> fun y -> x * y) (Some 2) None
    printfn "x2: %A" x2
    printfn "x2: %A" None
    
    let r = apply (Some squareInt)
    let r2 = r (Some 2)
    printfn "r: %A" r2
