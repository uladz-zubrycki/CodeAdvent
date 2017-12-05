open System

(* Part I *)
let getBounds num = 
    Seq.initInfinite (fun n -> n * n)
    |> Seq.filter (fun n -> n % 2 <> 0)
    |> Seq.windowed 2
    |> Seq.find (function 
        | [|start; finish|] -> start < num && num <= finish
        | _ -> failwith "unreachable")
    |> (fun pair -> pair.[0], pair.[1])

let getMidpoints start edgeDistance =
     [1..3] 
     |> Seq.fold (fun midpoints _ -> 
         match midpoints with
         | prev :: _ ->
             (prev + 2 * edgeDistance) :: midpoints 
         | _ -> failwith "unreachable")
         [start + edgeDistance]

let calculateStepsCount num = 
    if num = 1 then 0
    else 
        let (start, finish) = getBounds num    
        let edgeLength = int (Math.Sqrt <| float finish) 
        let edgeDistance = (edgeLength - 1) / 2
        let midpoints = getMidpoints start edgeDistance
        
        let midpoint = 
            midpoints
            |> Seq.find (fun midpoint -> 
                midpoint - edgeDistance < num && num <= midpoint + edgeDistance)
        
        edgeDistance + Math.Abs(midpoint - num)

calculateStepsCount 1 // 0
calculateStepsCount 12 // 3
calculateStepsCount 23 // 2
calculateStepsCount 1024 // 31