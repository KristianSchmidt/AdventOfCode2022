#load "Helpers.fsx"

open System
open Helpers

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data =
    Helpers.Web.getInput 15
    |> Array.map (fun s -> s.Replace(": closest beacon is at ", ", "))
    |> Array.map (fun s -> s.Replace("Sensor at ", "").Replace("x=", "").Replace("y=",""))
    |> Array.map ((Helpers.split ", ") >> (Array.map int))
    |> Array.map (fun [|sx;sy;bx;by|] -> ((sx,sy),(bx,by)))

let manhattan ((x1,y1),(x2,y2)) =
    abs (x1-x2) + abs (y1-y2)

let distToY (y : int) (x1,y1) =
    abs (y1-y)

let ans1 = 
    data
    |> Array.map (fun c ->
        let dist = c |> fst |> distToY 2000000
        let man = manhattan c
    
        c, man, dist, man - dist)
    |> Array.filter (fun (_,_,_,d) -> d >= 0)
    |> Array.map (fun (((sx,sy),_),man,_,d) ->
        sx - d, sx + d
        )
    |> Array.map (fun (x,y) -> Set.ofList [x .. y])
    |> Set.unionMany
    |> Set.count
    |> (fun c -> c - 1)

ans1

/// Part 2

let ans2 = data

ans2