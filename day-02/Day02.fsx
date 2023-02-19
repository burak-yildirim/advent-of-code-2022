open System.IO
type HandShape = Rock | Paper | Scissors

type MatchResult = Defeat | Draw | Victory

let (|ValidChar|_|) (chr: char) =
    let validChars = ['A'; 'B'; 'C'; 'X'; 'Y'; 'Z']
    if List.contains chr validChars then Some chr
    else None

let getMatchResult (opponent, you) =
    match opponent, you with
    | Rock, Paper
    | Paper, Scissors
    | Scissors, Rock -> Victory
    | o, y when o = y -> Draw
    | _, _ -> Defeat

let scoreByShape = function
    | Rock -> 1
    | Paper -> 2
    | Scissors -> 3

let scoreByResult = function
    | Defeat -> 0
    | Draw -> 3
    | Victory -> 6

let getMatchScore (opponent, you) =
    (opponent, you)
    |> getMatchResult
    |> scoreByResult
    |> (+) (scoreByShape you)

let getMatches () =
    File.ReadAllText("input.txt")
    // "A Y B X C Z"
    |> Seq.filter (function | ValidChar _ -> true | _ -> false)
    |> Seq.chunkBySize 2
    |> Seq.map (fun [| opponent; you; |] -> (opponent, you))

let charToShapePart1 = function
    | 'A' | 'X' -> Rock
    | 'B' | 'Y' -> Paper
    | 'C' | 'Z' -> Scissors

let printRound i =
    printfn "round result: %d" i
    i

let getPartOne () =
    getMatches ()
    |> Seq.map (fun (o, y) -> (charToShapePart1 o, charToShapePart1 y))
    |> Seq.map getMatchScore
    |> Seq.sum

// printfn "part 1: %d" (getPartOne ())

let charToShapePart2 = function
    | 'A' -> Rock
    | 'B' -> Paper
    | 'C' -> Scissors

let charToResult = function
    | 'X' -> Defeat
    | 'Y' -> Draw
    | 'Z' -> Victory

let getYourShape opponentShape result =
    match result with
    | Draw -> opponentShape
    | Victory ->
        match opponentShape with
        | Rock -> Paper
        | Paper -> Scissors
        | Scissors -> Rock
    | Defeat ->
        match opponentShape with
        | Rock -> Scissors
        | Paper -> Rock
        | Scissors -> Paper



let getPartTwo () =
    getMatches ()
    |> Seq.map (fun (o, r) ->
        let opponentShape = charToShapePart2 o
        let result = charToResult r
        let yourShape = getYourShape opponentShape result
        (scoreByShape yourShape) + (scoreByResult result))
    |> Seq.sum

printfn "part 2: %d" (getPartTwo ())
