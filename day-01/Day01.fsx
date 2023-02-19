open System
open System.IO

let getNumberGroups () =
    File.ReadLines("input.txt")
    |> Seq.fold
        (fun result line ->
            if String.IsNullOrWhiteSpace(line) then
                List.empty<int> :: result
            else
                match result with
                | [only] -> [(int line) :: only]
                | head :: tail -> ((int line) :: head) :: tail)
        [List.empty<int>]


let totalCalOfMax =
    getNumberGroups ()
    |> Seq.map (Seq.reduce (+))
    |> Seq.max

printfn "%d cals" totalCalOfMax

let top3Total =
    getNumberGroups ()
    |> Seq.map (Seq.reduce (+))
    |> Seq.sortDescending
    |> Seq.take 3
    |> Seq.reduce (+)

printfn "%d cals" top3Total
