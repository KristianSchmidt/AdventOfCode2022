#load "Helpers.fsx"

open System
open Helpers
open System.Collections.Generic

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

type Direction = | Up | Down | Left | Right

let memorec f =
   let cache = Dictionary<int,((int*int)*Direction) array>()
   let rec frec n = 
       let value = ref Array.empty
       let exist = cache.TryGetValue(n, value)
       match exist with
       | true -> 
           ()//printfn "%O -> In cache" n
       | false ->
           //printfn "%O -> Not in cache, calling function..." n
           value := f frec n
           cache.Add(n, !value)
       !value
   in frec

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

let someIfTrue b v = if (b) then Some v else None

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

for y in -1 .. yMax do
    printf "%i: " y
    for x in -1 .. xMax do
        if (isWall (x,y)) then printf "#" else printf "."
    printfn ""

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
        
//let allBlizzards =
//    [|1..2000|]
//    |> Array.scan (fun b _ -> b |> Array.map moveBlizzard) blizzards
//
//let allBsets = allBlizzards |> Array.map (fun b -> b |> Array.map fst |> Set.ofArray)

let blizzardsPos f i =
    if (i = 0) then blizzards
    else
        f (i-1) |> Array.map moveBlizzard

let memBlizzardPos = memorec blizzardsPos

let blizzSet i =
    memBlizzardPos i |> Array.map fst |> Set.ofArray

let memBlizzSet = memoize blizzSet

let solve startPos (endX,endY) startTime =
    let mutable currMin = 1600
    let s = new System.Diagnostics.Stopwatch()
    s.Start()
    
    let q = System.Collections.Generic.PriorityQueue<int,(int*int)*int>()

    let mutable stateMap : Map<(int*int)*int,int> = Map.empty    

    let rec f (x,y) currTime () =
        //if (x = 6 && y = 4) then
        //    printfn "got to end at %i" currTime
        
        if (x = endX && y = endY) then
            if (currTime < currMin) then
                currMin <- currTime
                printfn "New min: %i" currMin
            currTime
        else if (currTime > currMin) then
            Int32.MaxValue
        else if (stateMap.ContainsKey((x,y),currTime)) then
            stateMap[((x,y),currTime)]
        else
            let potential = currTime + abs (endY-y) + abs (endX-x)
            if (potential > currMin) then
                Int32.MaxValue
            else
            //printfn "(x,y) = (%i,%i) Currtime: %i" x y (currTime+1)
            let bSet = memBlizzSet (currTime+1)
            // Moves:
            // 1. do nothing
            // 2. move up, down, left or right
            let doNothing =
                f (x,y) (currTime + 1)
                |> someIfTrue (not (Set.contains (x,y) bSet))

            let moveUp =
                let nextPos = (x,y-1)
                f nextPos (currTime + 1)
                |> someIfTrue ((isWall nextPos |> not)
                               && not (Set.contains nextPos bSet))

            let moveDown =
                let nextPos = (x,y+1)
                f nextPos (currTime + 1)
                |> someIfTrue ((isWall nextPos |> not)
                               && not (Set.contains nextPos bSet))

            let moveLeft =
                let nextPos = (x-1,y)
                f nextPos (currTime + 1)
                |> someIfTrue ((isWall nextPos |> not)
                               && not (Set.contains nextPos bSet))

            let moveRight =
                let nextPos = (x+1,y)
                f nextPos (currTime + 1)
                |> someIfTrue ((isWall nextPos |> not)
                               && not (Set.contains nextPos bSet))

            let choices =
                if (endY > snd startPos) then
                    Array.choose id [| moveRight; moveDown; doNothing; moveLeft; moveUp|]
                else
                    Array.choose id [| moveLeft; moveUp; doNothing; moveRight; moveDown |]
                    
            let res =
                if (choices.Length = 0) then Int32.MaxValue
                else
                choices
                |> Array.map (fun f -> f ())
                |> Array.min

            stateMap <- Map.add ((x,y),currTime) res stateMap
            res
            
    f startPos startTime ()


let startPos = (1,0)
let endPos = (xMax-1,yMax)

let ans1 = solve startPos endPos 0

ans1

/// Part 2

let ans2 = (solve endPos startPos ans1)

let ans3 = solve startPos endPos ans2

// 812 too high

ans2