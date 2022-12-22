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
            //printfn "RealNextTile: %A" realNextTile
            match map[realNextTile] with
            | '#' -> // Wall here so stop moving
                     (currPos, currDir)
            | '.' -> doMove (realNextTile, currDir) (Walk (i-1))

let (finX, finY),finDir =
    code
    |> List.fold (fun state instr -> doMove state instr |> (fun x -> x)) (startPos, Right)

let ans1 = finY * 1000 + finX * 4 + (match finDir with | Right -> 0 | Down -> 1 | Left -> 2 | Up -> 3)

ans1

/// Part 2

open Helpers.Vector

let sideCoords = [
    ((51,1),  (100,50)), 1
    ((101,1), (150,50)), 2
    ((51,51), (100,100)), 3
    ((51,101),(100,150)), 4
    ((1,101), (50,150)), 5
    ((1,151), (50,200)), 6
    ]

let coordMap =
    [
    (1, {| Top = V(51,1);   Bottom = V(100,50) |})
    (2, {| Top = V(101,1);  Bottom = V(150,50) |})
    (3, {| Top = V(51,51);  Bottom = V(100,100) |})
    (4, {| Top = V(51,101); Bottom = V(100,150) |})
    (5, {| Top = V(1,101);  Bottom = V(50,150) |})
    (6, {| Top = V(1,151);  Bottom = V(50,200) |})
    ] |> Map.ofList

let determineSide (x,y) =
    printfn "Eval %A" (x,y)
    sideCoords
    |> List.find (fun (((x1,y1),(x2,y2)),_) -> x1 <= x && x <= x2 && y1 <= y && y <= y2)
    |> snd

//map |> Map.map (fun k v -> determineSide k)

let rec doMove2 (currPos, currDir) instr =
    match instr with
    | Turn d -> (currPos, doTurn currDir d)
    | Walk 0 -> (currPos, currDir)
    | Walk i ->
        let nextTile = doOneMove currPos currDir
        match Map.tryFind nextTile map with
        | Some '#' -> // Wall here so we stop moving
                      (currPos, currDir)
        | Some '.' -> doMove2 (nextTile, currDir) (Walk (i-1))
        | None -> // We've walked off the map
            let (realNextTile, nextDir) =
                let currentSide = determineSide currPos
                let (x,y) = currPos
                let xOffset = x - coordMap[currentSide].Top.x
                let yOffset = y - coordMap[currentSide].Top.y
                let nextSide =
                    match currentSide, currDir with
                    | 1, Up -> 6
                    | 1, Left -> 5
                    | 2, Up -> 6
                    | 2, Right -> 4
                    | 2, Down -> 3
                    | 3, Left -> 5
                    | 3, Right -> 2
                    | 4, Right -> 2
                    | 4, Down -> 6
                    | 5, Up -> 3
                    | 5, Left -> 1
                    | 6, Left -> 1
                    | 6, Right -> 4
                    | 6, Down -> 2
                let nextCoords = coordMap[nextSide]
                printfn "%A -> %A" currentSide nextSide
                printfn "xOffset = %i, yOffset = %i" xOffset yOffset
                match currentSide, currDir with
                | 1, Up -> // Get transformed to the left side of 6, facing right
                    ((nextCoords.Top.x, nextCoords.Top.y+xOffset), Right)
                | 1, Left -> // Get transformed to the left side of 5, facing right
                    ((nextCoords.Top.x, nextCoords.Bottom.y-yOffset), Right)
                | 2, Up -> // Get transformed to the bottom of 6, facing up
                    ((nextCoords.Top.x+xOffset, nextCoords.Bottom.y), Up)
                | 2, Right -> // Get transformed to right side of 4, facing left
                    ((nextCoords.Bottom.x, nextCoords.Bottom.y-yOffset), Left)
                | 2, Down -> // Right side of 3, facing Left
                    ((nextCoords.Bottom.x, nextCoords.Top.y+xOffset), Left)
                | 3, Left -> // Top of 5, facing Down
                    ((nextCoords.Top.x+yOffset, nextCoords.Top.y), Down)
                | 3, Right -> // Bottom of 2, facing Up
                    ((nextCoords.Top.x+yOffset, nextCoords.Bottom.y), Up)
                | 4, Right -> // Right side of 2, facing Left
                    ((nextCoords.Bottom.x, nextCoords.Bottom.y-yOffset), Left)
                | 4, Down -> // Right side of 6, facing Left
                    ((nextCoords.Bottom.x, nextCoords.Top.y+xOffset), Left)
                | 5, Up -> // Left side of 3, facing Right
                    ((nextCoords.Top.x, nextCoords.Top.y+xOffset), Right)
                | 5, Left -> // Left side of 1, facing Right
                    ((nextCoords.Top.x, nextCoords.Bottom.y - yOffset), Right)
                | 6, Left -> // Top of 1, facing down
                    ((nextCoords.Top.x+yOffset, nextCoords.Top.y), Down)
                | 6, Right -> // Bottom of 4, facing up
                    ((nextCoords.Top.x+yOffset, nextCoords.Bottom.y), Up)
                | 6, Down -> // Top of 2, facing Down
                    ((nextCoords.Top.x+xOffset, nextCoords.Top.y), Down)

            printfn "RealNextTile: %A" realNextTile
            //printfn "%i -> %i" currentSide
            match map[realNextTile] with
            | '#' -> // Wall here so stop moving
                     (currPos, currDir)
            | '.' -> doMove2 (realNextTile, nextDir) (Walk (i-1))

let (finX2, finY2),finDir2 =
    code
    |> List.fold (fun state instr -> doMove2 state instr |> (fun x -> x)) (startPos, Right)

let ans2 = finY2 * 1000 + finX2 * 4 + (match finDir2 with | Right -> 0 | Down -> 1 | Left -> 2 | Up -> 3)

// 143386 too low
// 52339 too low
// 48338 too low

ans2