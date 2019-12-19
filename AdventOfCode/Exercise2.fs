module Exercise2


let initProgram: int[] =
    [| 1; 0; 0; 3; 1; 1; 2; 3; 1; 3; 4; 3; 1; 5; 0; 3; 2; 13; 1; 19; 1; 9; 19; 23; 2; 23; 13; 27; 1; 27; 9; 31; 2; 31; 6;
       35; 1; 5; 35; 39; 1; 10; 39; 43; 2; 43; 6; 47; 1; 10; 47; 51; 2; 6; 51; 55; 1; 5; 55; 59; 1; 59; 9; 63; 1; 13; 63;
       67; 2; 6; 67; 71; 1; 5; 71; 75; 2; 6; 75; 79; 2; 79; 6; 83; 1; 13; 83; 87; 1; 9; 87; 91; 1; 9; 91; 95; 1; 5; 95;
       99; 1; 5; 99; 103; 2; 13; 103; 107; 1; 6; 107; 111; 1; 9; 111; 115; 2; 6; 115; 119; 1; 13; 119; 123; 1; 123; 6;
       127; 1; 127; 5; 131; 2; 10; 131; 135; 2; 135; 10; 139; 1; 13; 139; 143; 1; 10; 143; 147; 1; 2; 147; 151; 1; 6; 151;
       0; 99; 2; 14; 0; 0 |]

let applyOp (program: int []) opcode idx1 idx2 idx3 =
    match opcode with
        | 1 ->
            program.[idx3] <- program.[idx1] + program.[idx2]
            ()
        | 2 ->
            program.[idx3] <- program.[idx1] * program.[idx2]
            ()
        | 99 -> ()
        | _ -> ()

let rec run (program: int []) eip =
    let opcode = program.[eip]
    if opcode = 99 then
        ()
    else
        let idx1 = program.[eip+1]
        let idx2 = program.[eip+2]
        let idx3 = program.[eip+3]
        applyOp program opcode idx1 idx2 idx3
        run program (eip+4)

let exercise2 =
    for i in 0..99 do
        for j in 0..99 do
            let program1 = Array.copy initProgram
            program1.[1] <- i 
            program1.[2] <- j 

            run program1 0
            
            if program1.[0] = 19690720 then
                printfn "%A%A" program1.[1] program1.[2]


