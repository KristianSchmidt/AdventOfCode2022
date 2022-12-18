#load "Helpers.fsx"

open System
open Helpers

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data =
    Helpers.Web.getInput 18
    |> Array.map (function | Regex "(\d+),(\d+),(\d+)" [x;y;z] -> (int x,int y,int z))
    |> Set.ofArray

let neighbors (x,y,z) =
    [| (x-1,y,z); (x+1,y,z); (x,y-1,z); (x,y+1,z); (x,y,z-1); (x,y,z+1)|]

let numNeighborsExist c =
    neighbors c |> Array.filter (fun c' -> Set.contains c' data) |> Array.length

let ans1 =
    data
    |> Set.toArray
    |> Array.sumBy (fun c -> 6 - numNeighborsExist c)

ans1

/// Part 2

let maxMin f =
    let arr = data |> Set.toArray |> Array.map f
    (Array.min arr - 1, Array.max arr + 1) // Extending grid by 1 so BFS can flow through edges

let (minX,maxX) = maxMin (fun (x,_,_) -> x)
let (minY,maxY) = maxMin (fun (_,y,_) -> y)
let (minZ,maxZ) = maxMin (fun (_,_,z) -> z)

let withinRange (x,y,z) =
    x >= minX && x <= maxX &&
    y >= minY && y <= maxY &&
    z >= minZ && z <= maxZ

let reachable =
    let start = (1,1,1) // Random point not in lava
    BFS.bfs (fun c ->
        neighbors c
        |> Array.filter withinRange
        |> Array.filter (fun c' -> not (Set.contains c' data))) start
    |> Map.keys
    |> Set.ofSeq
    |> Set.add start

let overCounted2 =
    seq {
        for x in minX .. maxX do
            for y in minY .. maxY do
                for z in minZ .. maxZ do
                    if (not (Set.contains (x,y,z) reachable) && not (Set.contains (x,y,z) data)) then   
                        yield (x,y,z)
    }
    |> Array.ofSeq
    |> Array.sumBy numNeighborsExist

let ans2 = ans1 - overCounted2

ans2