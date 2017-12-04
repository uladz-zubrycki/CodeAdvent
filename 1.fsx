open System
let calculateCaptcha (puzzle: string) = 
    puzzle + string puzzle.[0]
    |> Seq.windowed 2
    |> Seq.sumBy (fun pair ->
        if pair.[0] = pair.[1] then Int32.Parse(string pair.[0])
        else 0)

calculateCaptcha "1122"
calculateCaptcha "1111"
calculateCaptcha "1234"
calculateCaptcha "91212129"
