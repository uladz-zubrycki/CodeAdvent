open System

(* Part I *)
let calculateCaptcha1 (puzzle: string) = 
    puzzle + string puzzle.[0]
    |> Seq.windowed 2
    |> Seq.sumBy (fun pair ->
        if pair.[0] = pair.[1] then Int32.Parse(string pair.[0])
        else 0)

calculateCaptcha1 "1122" // 3
calculateCaptcha1 "1111" // 4
calculateCaptcha1 "1234" // 0
calculateCaptcha1 "91212129" // 9

(* Part II *)
let calculateCaptcha2 (puzzle: string) =
    let halfwayStep = puzzle.Length / 2

    puzzle
    |> Seq.mapi (fun i x -> 
        let next = (i + halfwayStep) % puzzle.Length
        (x, puzzle.[next]))
    |> Seq.sumBy (fun (fst, snd) ->
        if fst = snd then Int32.Parse(string fst)
        else 0)

calculateCaptcha2 "1212" // 6
calculateCaptcha2 "1221" // 0
calculateCaptcha2 "123425" // 4
calculateCaptcha2 "123123" // 12
calculateCaptcha2 "12131415" // 4