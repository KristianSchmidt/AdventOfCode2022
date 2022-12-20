#load "Helpers.fsx"

open System
open Helpers

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

type PushDirection = | PushLeft | PushRight | PushDown

let data =
    Helpers.Web.getInput 17
    //[|">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"|]
    |> Array.map (fun s -> s.ToCharArray())
    |> Array.head
    |> Array.map (function | '<' -> PushLeft | '>' -> PushRight)
    
type Shape =
    | Line | Plus | Lshape | Pipe | Square

let getShape i = [|Line;Plus;Lshape;Pipe;Square|][i % 5]

let getTopRock (map : Set<int*int>) =
    if (Set.isEmpty map) then
        None
    else
        map |> Set.toArray |> Array.map snd |> Array.max |> Some

let summonShape (shape : Shape) (map : Set<int*int>) =
    let lowY = match getTopRock map with | Some x -> x+4 | None -> 4
    let leftX = -1

    match shape with
    | Line -> [(leftX, lowY); (leftX+1, lowY); (leftX+2, lowY); (leftX+3, lowY)]
    | Plus -> [(leftX, lowY+1); (leftX+1, lowY); (leftX+1, lowY+1); (leftX+1,lowY+2); (leftX+2, lowY+1)]
    | Lshape -> [(leftX, lowY); (leftX+1, lowY); (leftX+2, lowY); (leftX+2,lowY+1); (leftX+2, lowY+2)]
    | Pipe -> [(leftX, lowY); (leftX, lowY+1); (leftX, lowY+2); (leftX, lowY+3)]
    | Square -> [(leftX,lowY); (leftX,lowY+1); (leftX+1, lowY); (leftX+1, lowY+1)]

let canBePushed pushDirection (shape : (int*int) list) (map : Set<int*int>) =
    let newCoords =
        match pushDirection with
        | PushLeft -> shape |> List.map (fun (x,y) -> (x-1,y))
        | PushRight -> shape |> List.map (fun (x,y) -> (x+1,y))
        | PushDown -> shape |> List.map (fun (x,y) -> (x,y-1))

    // Can't be pushed if it would go into the wall
    let wouldHitWall = newCoords |> List.exists (fun (x,y) -> abs x > 3 || y <= 0)
    // Can't be pushed if it would go into an occupied space
    let wouldHitOccupied = newCoords |> List.exists (fun c -> Set.contains c map)
    if (wouldHitWall || wouldHitOccupied) then
        None
    else
        Some newCoords

let addShapeToMap (shape : Shape) ((map : Set<int*int>), windIdx) =
    let shapeSpawn = summonShape shape map
    //printfn "Shape started at: %A" shapeSpawn
    let rec f shapeCoords windIdx = 
        // First we get the jet push
        let wind = data[windIdx % data.Length]
        //printfn "Wind push: %A" wind
        let pushedByWind =
            canBePushed wind shapeCoords map
            |> Option.defaultValue shapeCoords
        match canBePushed PushDown pushedByWind map with
        | Some newCoords -> f newCoords (windIdx + 1)
        | None -> pushedByWind, windIdx + 1

    let (shapeEnded, newWindIdx) = f shapeSpawn windIdx
    //printfn "Shape ended at: %A" shapeEnded
    let mapBeforePrune = (shapeEnded |> List.fold (fun s t -> Set.add t s) map)
    let mapAfterPrune =
        let topRock = getTopRock mapBeforePrune |> Option.get
        mapBeforePrune
        |> Set.filter (fun v -> topRock < (snd v) + 50)
    mapAfterPrune, newWindIdx

let addNextShape ((map : Set<int*int>), windIdx) i =
    addShapeToMap (getShape i) (map,windIdx)

let ans1 =
    [0..2021]
    |> List.fold addNextShape (Set.empty, 0)
    |> (fst >> Set.toArray)
    |> Array.maxBy snd
    |> snd

ans1

/// Part 2

let solve () =
    let rec f i windIdx map cache =
        let (next,widx) = addNextShape (map, windIdx) i
        // todo: make string representation and add it to cache
        ()

    f 0 0 Set.empty Map.empty

let ans2 = data

ans2