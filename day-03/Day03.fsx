// open System
open System.IO

let letterToPriority (chr: char) =
    let rawInt = int chr
    if rawInt <= 90 then
        rawInt - 38
    else
        rawInt - 96

let arrayToMap (chars: char[]) =
    chars
    |> Seq.ofArray
    |> Seq.map (fun c -> (letterToPriority c), c)
    |> Map.ofSeq

let testLines = [
    "vJrwpWtwJgWrhcsFMMfFFhFp"
    "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
    "PmmdzqPrVvPwwTWBwg"
    "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn"
    "ttgJtRGJQctTZtZT"
    "CrZsJsPPZsGzwwsLwLmpwMDw"
]

let getCompartments () =
    File.ReadLines("input.txt")
    // testLines
    |> Seq.map (fun line ->
        line.ToCharArray() |> Array.splitAt (line.Length / 2))

let getPart1 () =
    getCompartments ()
    |> Seq.map (fun (c1, c2) ->
        let m1 = arrayToMap c1
        let m2 = arrayToMap c2
        
        Map.keys m1
        |> Seq.filter (fun k1 -> Map.containsKey k1 m2)
        |> Seq.sum)
    |> Seq.sum

printfn "part 1: %d" (getPart1 ())

let getElfGroups () =
    File.ReadLines("input.txt")
    |> Seq.chunkBySize 3

let getPart2 () =
    getElfGroups ()
    |> Seq.map (fun [| e1; e2; e3; |] ->
        let m1 = arrayToMap (e1.ToCharArray())
        let m2 = arrayToMap (e2.ToCharArray())
        let m3 = arrayToMap (e3.ToCharArray())
        
        Map.keys m1
        |> Seq.find (fun k1 ->
            (Map.containsKey k1 m2) && (Map.containsKey k1 m3)))
    |> Seq.sum

printfn "part 2: %d" (getPart2 ())
