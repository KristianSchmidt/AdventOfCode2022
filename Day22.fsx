#load "Helpers.fsx"

open System
open Helpers

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let test = IO.File.ReadAllLines(@"C:\code\AdventOfCode2022\22test.txt")

type Instruction = | Turn of string | Walk of int

type Direction = | Up | Down | Right | Left

let doTurn dir c =
    match c with
    | "R" ->
        match dir with
        | Up -> Right
        | Down -> Left
        | Right -> Down
        | Left -> Up
    | "L" ->
        match dir with
        | Up -> Left
        | Down -> Right
        | Right -> Up
        | Left -> Down

let doOneMove (x,y) dir =
    match dir with
    | Up -> (x,y-1)
    | Down -> (x,y+1)
    | Right -> (x+1,y)
    | Left -> (x-1,y)

let (map, code) =
    let input = Helpers.Web.getInput 22
    
    let rec parseCode res (s : string) =
        match s with
        | _ when s.Length = 0 -> List.rev res
        | Regex "^(\d+)(.*)" [d; rest] -> parseCode (Walk (int d) :: res) rest
        | Regex "^(\w)(.+)" [s; rest] -> parseCode (Turn s :: res) rest

    let code = parseCode [] input[input.Length-1]
    
    let map = 
        input[..input.Length-3]
        |> Array.mapi (fun y s -> s.ToCharArray() |> Array.mapi (fun x c -> ((x+1,y+1),c)))
        |> Array.collect id
        |> Array.filter (function | _,' ' -> false | _ -> true)
        |> Map.ofArray
    
    map, code

let startPos = map |> Map.toArray |> Array.filter (fun ((x,y),_) -> y = 1) |> Array.minBy (fst>>fst) |> fst

let rec doMove (currPos, currDir) instr =
    match instr with
    | Turn d -> (currPos, doTurn currDir d)
    | Walk 0 -> (currPos, currDir)
    | Walk i ->
        let nextTile = doOneMove currPos currDir
        match Map.tryFind nextTile map with
        | Some '#' -> // Wall here so we stop moving
                      (currPos, currDir)
        | Some '.' -> doMove (nextTile, currDir) (Walk (i-1))
        | None -> // We've walked off the map
            let realNextTile =
                match currDir with
                | Up -> map
                        |> Map.toArray
                        |> Array.filter (fun ((x,y),_) -> x = (fst currPos))
                        |> Array.maxBy (fst >> snd) |> fst
                | Down -> map
                          |> Map.toArray
                          |> Array.filter (fun ((x,y),_) -> x = (fst currPos))
                          |> Array.minBy (fst >> snd) |> fst
                | Left -> map
                            |> Map.toArray
                            |> Array.filter (fun ((x,y),_) -> y = (snd currPos))
                            |> Array.maxBy (fst >> fst) |> fst
                | Right -> map
                           |> Map.toArray
                           |> Array.filter (fun ((x,y),_) -> y = (snd currPos))
                           |> Array.minBy (fst >> fst) |> fst
            printfn "RealNextTile: %A" realNextTile
            match map[realNextTile] with
            | '#' -> // Wall here so stop moving
                     (currPos, currDir)
            | '.' -> doMove (realNextTile, currDir) (Walk (i-1))

let (finX, finY),finDir =
    code
    |> List.fold (fun state instr -> doMove state instr |> (fun x -> printfn "%A, %A" x instr; x)) (startPos, Right)

let ans1 = finY * 1000 + finX * 4 + (match finDir with | Right -> 0 | Down -> 1 | Left -> 2 | Up -> 3)

ans1

/// Part 2

let rec doMove2 (currPos, currDir) instr =
    match instr with
    | Turn d -> (currPos, doTurn currDir d)
    | Walk 0 -> (currPos, currDir)
    | Walk i ->
        let nextTile = doOneMove currPos currDir
        match Map.tryFind nextTile map with
        | Some '#' -> // Wall here so we stop moving
                      (currPos, currDir)
        | Some '.' -> doMove (nextTile, currDir) (Walk (i-1))
        | None -> // We've walked off the map
            let realNextTile =
                match currDir with
                | Up -> map
                        |> Map.toArray
                        |> Array.filter (fun ((x,y),_) -> x = (fst currPos))
                        |> Array.maxBy (fst >> snd) |> fst
                | Down -> map
                          |> Map.toArray
                          |> Array.filter (fun ((x,y),_) -> x = (fst currPos))
                          |> Array.minBy (fst >> snd) |> fst
                | Left -> map
                            |> Map.toArray
                            |> Array.filter (fun ((x,y),_) -> y = (snd currPos))
                            |> Array.maxBy (fst >> fst) |> fst
                | Right -> map
                           |> Map.toArray
                           |> Array.filter (fun ((x,y),_) -> y = (snd currPos))
                           |> Array.minBy (fst >> fst) |> fst
            printfn "RealNextTile: %A" realNextTile
            match map[realNextTile] with
            | '#' -> // Wall here so stop moving
                     (currPos, currDir)
            | '.' -> doMove (realNextTile, currDir) (Walk (i-1))

let (finX2, finY2),finDir2 =
    code
    |> List.fold (fun state instr -> doMove2 state instr |> (fun x -> printfn "%A, %A" x instr; x)) (startPos, Right)

let ans2 = finY2 * 1000 + finX2 * 4 + (match finDir2 with | Right -> 0 | Down -> 1 | Left -> 2 | Up -> 3)


ans2