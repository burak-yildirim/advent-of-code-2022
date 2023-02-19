open System
open System.IO
open System.Text.RegularExpressions

let inputText = File.ReadAllText("input.txt")

let getStacks (input: string) =
    let stackText = (input.Split("\n\n")[0])

    stackText.Split("\n")
    |> Seq.ofArray
    |> Seq.takeWhile (fun line -> not (line.StartsWith(" 1")))
    |> Seq.map (fun line ->
        let chars = line.ToCharArray() |> Seq.ofArray
        
        chars
        |> Seq.indexed
        |> Seq.filter (fun (i, c) -> i % 2 <> 0)
        |> Seq.map (fun (i, c) -> c))
    |> Seq.transpose
    |> Seq.map (Seq.filter (fun c -> c <> ' '))
    |> Seq.filter (fun chars -> Seq.length chars > 0)
    // |> Seq.map (fun chars -> String.Join("", chars))
    // |> (fun lines -> String.Join("\n", lines))
    |> Seq.map List.ofSeq
    |> Seq.indexed
    |> Map.ofSeq

let matchEvaluator (m: Match) =
    let amount = m.Groups["amount"].Value
    let from = m.Groups["from"].Value
    let toVal = m.Groups["to"].Value
    sprintf "%s-%s-%s" amount from toVal

let getCommands (input: string) =
    let commandsText = (input.Split("\n\n")[1])
    let rx = new Regex(@"move (?'amount'\d*) from (?'from'\d*) to (?'to'\d*)",
        RegexOptions.Multiline)
    
    rx.Replace(commandsText, matchEvaluator).Split('\n')
    |> Seq.ofArray
    |> Seq.map (fun line ->
        let [| amount; fromVal; toVal; |] = line.Split('-')
        (int amount, (int fromVal) - 1, (int toVal) - 1))
    |> List.ofSeq

let getTopCrates isPart1 =
    let originalStacks = getStacks inputText
    let commands = getCommands inputText

    List.fold
        (fun (stacks: Map<int, char list>) (amount, fromVal, toVal) ->
            let (taken, remaining) =
                stacks |> Map.find fromVal |> List.splitAt amount
            let takenOrdered = if isPart1 then List.rev taken else taken
            let toList = Map.find toVal stacks

            stacks
            |> Map.add fromVal remaining
            |> Map.add toVal (takenOrdered @ toList))
        originalStacks
        commands
    |> Map.values
    |> Seq.map (List.item 0)
    |> (fun chars -> String.Join("", chars))

printfn "part 1: %s" (getTopCrates true)
printfn "part 2: %s" (getTopCrates false)
