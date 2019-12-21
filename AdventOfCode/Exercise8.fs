module Exercise8

let exercise8 =
        
    let readLines filePath = List.ofSeq(System.IO.File.ReadLines(filePath))

    let line::_ = readLines "input/input8.txt"
    let w = 25
    let h = 6
    
    let xs = [for c in line do c] |> List.map string |> List.map int
    printfn "%A" (List.length xs)
    printfn "%A" ((List.length xs) / (25*6))
        
    let n = (List.length xs) / (25*6)
        
    let x: int[,,] = Array3D.zeroCreate n h w

    for i in 0..n-1 do
        for j in 0..h-1 do
            for k in 0..w-1 do
                x.[i,j,k] <- xs.[i*(h*w) + j*w + k]
                
    printfn "%A" x

    let zeroes = Array.zeroCreate n       
    let ones = Array.zeroCreate n       
    let twos = Array.zeroCreate n
    for i in 0..n-1 do
        for j in 0..h-1 do
            for k in 0..w-1 do
                match x.[i,j,k] with
                | 0 -> zeroes.[i] <- zeroes.[i] + 1
                | 1 -> ones.[i] <- ones.[i] + 1
                | 2 -> twos.[i] <- twos.[i] + 1
                | _ -> ()

    let i, _ = Array.mapi (fun i c -> (i,c)) zeroes |> Array.minBy snd
    
    printfn "%A" zeroes

    let a = ones.[i] * twos.[i]
    printfn "%A" a

    let image = Array2D.zeroCreate h w
    
    for j in 0..h-1 do
        for k in 0..w-1 do
            let xs = [for i in 0..n-1 do x.[i,j,k]]
            let x = List.find (fun x -> x <> 2) xs
            image.[j,k] <- x
            if x = 1 then
                printf "█"
            else
                printf " "
        printfn ""

    printfn "%A" image


