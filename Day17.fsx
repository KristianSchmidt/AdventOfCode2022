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
        | Some newCoords -> f newCoords ((windIdx + 1)%data.Length)
        | None -> pushedByWind, (windIdx + 1)%data.Length

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

let mapToString (map : Set<int*int>) =
    let topRock = getTopRock map |> Option.get
    let sb = new System.Text.StringBuilder(7*50)
    //let ba = new System.Collections.BitArray(7*50)
    for y in topRock - 49 .. topRock do
        for x in -3 .. 3 do
            if (Set.contains (x,y) map)
            then sb.Append("#") |> ignore 
            else sb.Append(".") |> ignore

    sb.ToString()

let solve () =
    let rec f i windIdx map (cache : Map<string, int*int>) =
        //if (i % 1000 = 0) then printfn "Iter: %i" i
        let (next,widx) = addNextShape (map, windIdx) i
        // todo: make string representation and add it to cache
        let str = mapToString next
        let topRock = getTopRock next |> Option.get
        match Map.tryFind (str) cache with
        | Some (i',tp) ->
            printfn "Section (%i,%i) matches section (%i,%i)" i' tp (i+1) topRock
            if (i > 10_000) then () else
            f (i+1) widx next (cache |> Map.add (str) (i+1,topRock))
        | None -> f (i+1) widx next (cache |> Map.add (str) (i+1,topRock))

    f 0 0 Set.empty Map.empty

solve ()

// Above code gives Section (1875,2972) matches section (3610,5753)
// Sample carefully chosen so there would be no remainer to handle
let (startTime, startHeight) = 1875L, 2972L
let (endTime, endHeight) = 3610L, 5753L

let period = endTime - startTime
let height = endHeight - startHeight

let timeToSimulate = 1_000_000_000_000L - startTime
let extraHeight = (timeToSimulate / period) * height

let ans2 = startHeight + extraHeight

ans2