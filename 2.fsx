open System

(* Part I*)
let calculateChecksum1 (spreadsheet: int list list) = 
    spreadsheet
    |> List.sumBy (fun row -> 
         let (min, max) = 
             row 
             |> List.fold (fun (min, max) item -> 
                 let min = if item < min then item else min 
                 let max = if item > max then item else max
                 (min, max)) (row.[0], row.[0])
         max - min)

calculateChecksum1 
    [[5; 1; 9; 5]
     [7; 5; 3]
     [2; 4; 6; 8]] // 18

(* Part II *)
let calculateChecksum2 (spreadsheet: int list list) = 
    spreadsheet
    |> List.sumBy (fun row -> 
        let pairs = seq {
            for i in 0..row.Length - 1 do
                for j in i + 1..row.Length - 1 do
                    yield row.[i], row.[j] }

        pairs 
        |> Seq.map (fun (fst, snd) -> 
            match Math.DivRem(fst, snd) with
            | div, 0 -> Some(div)
            | _, _ ->
                match Math.DivRem(snd, fst) with 
                | div, 0 -> Some(div)
                | _, _ -> None )
        |> Seq.find Option.isSome 
        |> Option.get)

calculateChecksum2
    [[5; 9; 2; 8]
     [9; 4; 7; 3]
     [3; 8; 6; 5]] // 9