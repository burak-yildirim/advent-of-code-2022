open System.IO

type FileTreeItem =
    | FileNode of FileNode
    | DirNode of DirNode

and FileNode = (string * int)

and DirNode = (string * FileTreeItem seq)

let rec insert (itemToInsert: FileTreeItem) (paths: string list) (mainDir: DirNode) : DirNode =
    match paths with
    | [] ->
        ((fst mainDir),
         seq {
             yield! (snd mainDir)
             yield itemToInsert
         })
    | head :: tail ->
        (snd mainDir)
        |> Seq.map (function
            | DirNode (dirName, subItems) when dirName = head ->
                (dirName, subItems)
                |> insert itemToInsert tail
                |> DirNode
            | x -> x)
        |> (fun treeItems -> ((fst mainDir), treeItems))

let rec cd (destinationPaths: string list) (workingPaths: string list) =
    match destinationPaths with
    | [] -> workingPaths
    | ".." :: tail ->
        workingPaths
        |> List.removeAt (workingPaths.Length - 1)
        |> cd tail
    | "" :: tail -> cd tail [ "" ]
    | head :: tail -> [ head ] |> List.append workingPaths |> cd tail

let rec getSize (mapper: (int * int) -> (int * int)) (mappedSize: int) =
    function
    | FileNode (_, size) ->
        // do not include file sizes into collectedSize
        // because only directory sizes are asked
        // (size, (size |> mapper |> (+) collectedSize))
        (size, mappedSize)
    | DirNode (_, itemSeq) ->
        itemSeq
        |> Seq.fold
            (fun (dirSize, accSize) item ->
                let (itemSize, mappedAccSize) = item |> getSize mapper accSize

                ((dirSize + itemSize), mappedAccSize))
            (0, mappedSize)
        |> mapper

let linesToDirNode (lines: string seq) : DirNode =
    let theFileSystem: DirNode =
        ("FILE_SYSTEM", seq { yield DirNode("", Seq.empty<FileTreeItem>) })

    lines
    |> Seq.fold
        (fun (fileSystem, workingPaths) line ->
            if line.StartsWith(@"$ cd ") then
                let newWorkingPaths =
                    line.Substring(5).Split(@"/")
                    |> List.ofArray
                    |> cd
                    <| workingPaths

                (fileSystem, newWorkingPaths)
            else if line.StartsWith(@"dir ") then
                let newDir = DirNode(line.Substring(4), Seq.empty<FileTreeItem>)

                let newSystem = insert newDir workingPaths fileSystem

                (newSystem, workingPaths)
            else if line.StartsWith(@"$ ls") then
                (fileSystem, workingPaths)
            // case for FileNode which starts with file size
            else
                let [| sizeStr; fileName |] = line.Split(" ")
                let newFile = FileNode(fileName, (int sizeStr))
                let newRoot = insert newFile workingPaths fileSystem
                (newRoot, workingPaths))
        (theFileSystem, [ "" ])
    |> fst

let mapper1 (dirSize, foldedSize) =
    let addition = if dirSize <= 100000 then dirSize else 0
    (dirSize, (addition + foldedSize))

let getPart1 () =
    File.ReadLines("input.txt")
    |> linesToDirNode
    |> DirNode
    |> getSize mapper1 0
    |> snd

printfn "part 1: %d" (getPart1 ())

let mapper2 (requiredSpaceToErase: int) (dirSize, smallestBig) =
    let newSmallestBig =
        if smallestBig < requiredSpaceToErase then
            max dirSize smallestBig
        else if dirSize >= requiredSpaceToErase then
            min dirSize smallestBig
        else
            smallestBig

    (dirSize, newSmallestBig)

let getPart2 () =
    File.ReadLines("input.txt")
    |> linesToDirNode
    |> DirNode
    |> (fun dirNode ->
        let totalUsedSize = dirNode |> getSize id 0 |> fst
        let totalFreeSpace = 70000000 - totalUsedSize

        let requiredSpaceToErase =
            if totalFreeSpace < 30000000 then
                30000000 - totalFreeSpace
            else
                30000000

        (requiredSpaceToErase, dirNode))
    |> (fun (requiredSpaceToErase, dirNode) -> getSize (mapper2 requiredSpaceToErase) 0 dirNode)
    |> snd

printfn "part 2: %d" (getPart2 ())
