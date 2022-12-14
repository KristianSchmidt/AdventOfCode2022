#load "Helpers.fsx"

open System
open Helpers

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let (startPos, endPos, data) =
    let m =
        Helpers.Web.getInput 12
        |> Array.map (fun s -> s.ToCharArray())
        |> toGridMap

    let start = Map.tryFindKey (fun k v -> v = 'S') m |> Option.get
    let endPos = Map.tryFindKey (fun k v -> v = 'E') m |> Option.get

    let finalM = m |> Map.add start 'a' |> Map.add endPos 'z'

    (start, endPos, finalM)

let makeNeighbors ((x,y),v : char) =
    [|(x,y+1);(x,y-1);(x-1,y);(x+1,y)|]
    |> Array.choose (fun c -> Map.tryFind c data
                              |> Option.bind (fun v' -> if ((int v) - (int v') >= -1) then Some ((x,y),c) else None))

let edges =
    data
    |> Map.toArray
    |> Array.collect makeNeighbors
    |> Array.map (fun x -> (x,true))
    |> Map.ofArray

let res = Helpers.Dijkstra.dijkstra edges startPos

let ans1 = res |> Map.find endPos

ans1

/// Part 2

let makeNeighbors2 ((x,y),v : char) =
    [|(x,y+1);(x,y-1);(x-1,y);(x+1,y)|]
    |> Array.choose (fun c -> Map.tryFind c data
                              |> Option.bind (fun v' -> if ((int v') - (int v) >= -1) then Some ((x,y),c) else None))

let edges2 =
    data
    |> Map.toArray
    |> Array.collect makeNeighbors2
    |> Array.map (fun x -> (x,true))
    |> Map.ofArray

let res2 = Helpers.Dijkstra.dijkstra edges2 endPos

let ans2 =
    data
    |> Map.filter (fun _ v -> v = 'a')
    |> Map.toArray
    |> Array.minBy (fun (k,_) -> Map.find k res2)
    |> (fun (k,_) -> Map.find k res2)

ans2