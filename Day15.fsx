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

#time "on"

let interval i c =
    let man = manhattan c
    let ((sx,sy),_) = c
    let dist = abs (i - sy)
    //printfn "(%i, %i) Man: %i - Dist: %i" sx sy man dist
    if (man >= dist) then
        Some (max 0 (sx - (man - dist)), min (sx + (man - dist)) 4_000_000)
    else    
        None

for i in 1 .. 4_000_000 do
    data |> Array.choose (interval i) |> Array.sortBy fst |> ignore

data |> Array.length

let fs =
    data
    |> Array.map (fun c ->
        let ((sx,sy),_) = c
        let man = manhattan c
        (fun (x,y) -> manhattan ((x,y),(sx,sy)) > man)    
    )

let ans2 =
    Seq.allPairs [0 .. 4_000_000] [0 .. 4_000_000]
    |> Seq.tryFind (fun c -> fs |> Array.forall (fun f -> f c))


ans2