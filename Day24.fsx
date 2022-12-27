#load "Helpers.fsx"

open System
open System.Collections.Generic
open Helpers

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

type Direction = | Up | Down | Left | Right

let test = """#.######
#>>.<^<#
#.<..<<#
#>v.><>#
#<^v^^>#
######.#""" |> split "\n"

let data =
    //test
    Helpers.Web.getInput 24
    |> Array.mapi (fun y s -> s.ToCharArray() |> Array.mapi (fun x c -> ((x,y),c)))
    |> Array.collect id

let blizzards =
    data
    |> Array.choose (fun (pos,c) -> 
        match c with
        | 'v' -> Some (pos,Down)
        | '<' -> Some (pos,Left)
        | '^' -> Some (pos,Up)
        | '>' -> Some (pos,Right)
        | _ -> None
        )

let xMax = data |> Array.maxBy (fst >> fst) |> (fst >> fst)
let yMax = data |> Array.maxBy (fst >> snd) |> (fst >> snd)

let isWall (x,y) =
    match x,y with
    | (1,0) -> false
    | x,y when x = (xMax - 1) && y = yMax -> false
    | x,y when x <= 0 || y < 0 -> true
    | _ when y >= yMax -> true
    | _,_ when x >= xMax -> true
    | 0,_ -> true
    | _,0 -> true
    | _ -> false

let moveBlizzard ((x,y),(dir : Direction)) =
    let nextTile =
        match dir with
        | Up -> (x,y-1)
        | Down -> (x,y+1)
        | Right -> (x+1,y)
        | Left -> (x-1,y)

    match isWall nextTile with
    | false -> nextTile, dir
    | true ->
        match dir with
        | Up -> let y' = if ((fst nextTile) = xMax-1) then yMax else yMax-1
                (fst nextTile, y'), dir
        | Down -> let y' = if ((fst nextTile) = 1) then 0 else 1
                  (fst nextTile, y'), dir
        | Right -> (1,snd nextTile), dir
        | Left -> ((xMax-1), snd nextTile), dir
        
let blizzPerms =
    Helpers.lcm ((int64 yMax)-1L) ((int64 xMax)-1L) |> int

#time "on"
let allBlizzards =
    [|1..blizzPerms|]
    |> Array.scan (fun b _ -> b |> Array.map moveBlizzard) blizzards

let allBsets =
    allBlizzards
    |> Array.map (fun b -> b |> Seq.map fst |> Set.ofSeq)

let solve startPos (endX,endY) startTime =
    let calcPotential ((x',y'),time) =
        let res = time + abs (endY-y') + abs (endX-x')
        res

    let mutable currMin = Int32.MaxValue
    let s = new System.Diagnostics.Stopwatch()
    s.Start()
    
    let q = PriorityQueue<(int*int)*int,int>()
    q.Enqueue((startPos,startTime), abs (fst startPos - endX) + abs (snd startPos - endY))

    let mutable stateMap : Map<(int*int)*int,int> = Map.empty
    let mutable i = 0
    while (q.Count > 0) do
        i <- i + 1
        let key = q.Dequeue()
        let ((x,y),currTime) = key
        
        if (x = endX && y = endY) then
            if (currTime < currMin) then
                currMin <- currTime
                printfn "New min: %i. Time: %A" currMin s.Elapsed

        let potential = currTime + abs (endY-y) + abs (endX-x)
        if (potential > currMin) then
            ()
        else if (stateMap.ContainsKey(key) && stateMap[key] <= currTime) then
            ()
        else
        let bSet = allBsets[(currTime+1) % blizzPerms]
        

        [| (x,y); (x,y-1); (x,y+1); (x-1,y); (x+1,y)|]
        |> Array.iter (fun c ->
            if ((isWall c |> not) && not (Set.contains c bSet)) then
                let key = (c, currTime + 1)
                q.Enqueue(key, calcPotential key)
        )

        stateMap <- Map.add key currTime stateMap
           
    printfn "%A -> %A examined %i nodes" startPos (endX, endY) i
    currMin


let startPos = (1,0)
let endPos = (xMax-1,yMax)

// 279
let ans1 = solve startPos endPos 0

ans1

/// Part 2

// 529
let ans2 = (solve endPos startPos ans1)

// 762
let ans3 = solve startPos endPos ans2

ans2