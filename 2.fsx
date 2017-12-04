open System
open System.IO

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