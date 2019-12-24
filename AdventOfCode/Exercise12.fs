module Exercise12

open System

type Moon = {
    id: int;
    position: int*int*int;
    velocity: int*int*int
}

let applyGravity (xs:Moon list) (m:Moon): Moon =
    let mutable dx = 0
    let mutable dy = 0
    let mutable dz = 0
    let others = List.filter (fun x -> m <> x) xs
    let { position = (ax,ay,az); velocity = (vx,vy,vz) } = m
    
    for { position = (xx,xy,xz) } in others do
        if xx > ax then dx <- dx + 1
        else if xx < ax then dx <- dx - 1
        else ()

        if xy > ay then dy <- dy + 1
        else if xy < ay then dy <- dy - 1
        else ()

        if xz > az then dz <- dz + 1
        else if xz < az then dz <- dz - 1
        else ()

    { m with velocity = (vx+dx, vy+dy, vz+dz) }

let applyVelocity (m:Moon): Moon =
    let { position = (ax,ay,az); velocity = (vx,vy,vz) } = m
    { m with position = (ax+vx, ay+vy, az+vz) }
    
let potentialEnergy (m:Moon): int =
    let { position = (ax,ay,az); velocity = (vx,vy,vz) } = m
    Math.Abs(ax) + Math.Abs(ay) + Math.Abs(az)        

let kineticEnergy (m:Moon): int =
    let { position = (ax,ay,az); velocity = (vx,vy,vz) } = m
    Math.Abs(vx) + Math.Abs(vy) + Math.Abs(vz)        

let totalEnergy (m:Moon): int =
    (potentialEnergy m) * (kineticEnergy m)        

let exercise12 =

    let mutable moons = [
        { id=0; position=(-1, 0, 2); velocity=(0,0,0) };
        { id=1; position=(2, -10, -7); velocity=(0,0,0) };
        { id=2; position=(4, -8, 8); velocity=(0,0,0) };
        { id=3; position=(3, 5, -1); velocity=(0,0,0) }
    ]

    let mutable moons = [
        { id=0; position=(-8, -10, 0); velocity=(0,0,0) };
        { id=1; position=(5, 5, 10); velocity=(0,0,0) };
        { id=2; position=(2, -7, 3); velocity=(0,0,0) };
        { id=3; position=(9, -8, -3); velocity=(0,0,0) }
    ]

    //    let mutable moons = [
    //        { id=0; position=(-13, -13, -13); velocity=(0,0,0) };
    //        { id=1; position=(5, -8, 3); velocity=(0,0,0) };
    //        { id=2; position=(-6, -10, -3); velocity=(0,0,0) };
    //        { id=3; position=(0, 5, -5); velocity=(0,0,0) }
    //    ]

    let moonStart = moons

    //    let mutable moonSet = Set.empty
    //    moonSet <- moonSet.Add(moons)

    let mutable i = 0L
    let mutable notContains = true
        
    while notContains do
        moons <- List.map (applyGravity moons) moons |> List.map applyVelocity
        i <- i + 1L
        
        let { velocity = (vx1,vy1,vz1) } = moonStart.[0]
        let { velocity = (vx2,vy2,vz2) } = moons.[0]
        
        if vx1 = vx2 && vy1 = vy2 && vz1 = vz2 then
            if moonStart = moons then
            // if moonSet.Contains(moons) then
                printfn "contains: %A" i
                notContains <- false

        if i % 100000L = 0L then
            printfn "%A" i
        
        // moonSet <- moonSet.Add(moons)
        
        
    //        if i % 10 = 0 then
    //            printfn "step: %A" i
    //            printfn "%A" moons
    //            printfn "%A %A %A" (potentialEnergy moons.[0]) (kineticEnergy moons.[0]) (totalEnergy moons.[0])
    //            printfn "%A %A %A" (potentialEnergy moons.[1]) (kineticEnergy moons.[1]) (totalEnergy moons.[1])
    //            printfn "%A %A %A" (potentialEnergy moons.[2]) (kineticEnergy moons.[2]) (totalEnergy moons.[2])
    //            printfn "%A %A %A" (potentialEnergy moons.[3]) (kineticEnergy moons.[3]) (totalEnergy moons.[3])
    //            printfn "total: %A" ((totalEnergy moons.[0]) + (totalEnergy moons.[1]) + (totalEnergy moons.[2]) + (totalEnergy moons.[3]))



