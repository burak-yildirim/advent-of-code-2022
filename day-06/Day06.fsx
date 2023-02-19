open System.IO

let getChars () =
    seq {
        use reader = new StreamReader("input.txt")

        while not reader.EndOfStream do
            yield char (reader.Read())
    }

let getAnswer isPart1 =
    let markerSize = if isPart1 then 4 else 14
    getChars ()
    |> Seq.indexed
    |> Seq.windowed markerSize
    |> Seq.takeWhile (
        (Array.map snd)
        >> (Array.distinct)
        >> (Array.length)
        >> ((<>) markerSize)
    )
    |> Seq.last
    |> ((Array.map fst) >> (Array.last) >> ((+) 2))

printfn "part 1: %d" (getAnswer true)
printfn "part 2: %d" (getAnswer false)
