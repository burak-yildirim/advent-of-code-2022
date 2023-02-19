open System.IO

let getStartsEnds () =
    File.ReadLines("input.txt")
    |> Seq.map (fun line ->
        let [| e1; e2; |] = line.Split(',')
        let [| start1; end1 |] = e1.Split('-')
        let [| start2; end2 |] = e2.Split('-')
        (int start1, int end1), (int start2, int end2))

let getPart1 () =
    getStartsEnds ()
    |> Seq.filter (fun ((s1, e1), (s2, e2)) ->
        (s1 <= s2 && e1 >= e2 ) || (s1 >= s2 && e1 <= e2))
    |> Seq.length

printfn "part 1: %d" (getPart1 ())

let getPart2 () =
    getStartsEnds ()
    |> Seq.filter (fun ((s1, e1), (s2, e2)) ->
        (s1 <= s2 && e1 >= s2)
        || (s1 <= e2 && e1 >= e2)
        || (s2 <= s1 && e2 >= s1)
        || (s2 <= e1 && e2 >= e1))
    |> Seq.length

printfn "part 2: %d" (getPart2 ())
