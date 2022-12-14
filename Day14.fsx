#load "Helpers.fsx"

open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data =
    Helpers.Web.getInput 14
    |> Array.map (Helpers.split " -> ")
    |> Array.map (Array.map ((Helpers.split ",") >> Array.map int) >> (Array.map (fun [|x;y|] -> (x,y))))
    |> Array.collect Array.pairwise

let makeRock ((x1,y1),(x2,y2)) =
    seq {
        for x in (Array.min [|x1;x2|]) .. (Array.max [|x1;x2|]) do
            for y in (Array.min [|y1;y2|]) .. (Array.max [|y1;y2|]) do
                yield (x,y)
    }
    |> Array.ofSeq

let rocks =
    data
    |> Array.collect makeRock
    |> Set.ofArray

let pour = (500,0)

let findRest (rocks : Set<int*int>) (sand : Set<int*int>) =
    let isAvail c = not (Set.contains c rocks || Set.contains c sand)
    let rec f i (x,y) =
        // Check downwards, down left and down right
        if (i = 1000) then
            None
        else if (isAvail (x,y+1)) then
            f (i+1) (x,y+1)
        else if (isAvail (x-1,y+1)) then
            f (i+1) (x-1,y+1)
        else if (isAvail (x+1,y+1)) then
            f (i+1) (x+1,y+1)
        else
            Some (x,y)

    f 0 pour

let solve () =
    let rec f sand =
        match findRest rocks sand with
        | Some c -> f (Set.add c sand)
        | None -> Set.count sand

    f Set.empty

let ans1 = solve ()

ans1

/// Part 2

let findRest2 (rocks : Set<int*int>) (sand : Set<int*int>) yMax =
    let isAvail c = not (Set.contains c rocks || Set.contains c sand || (snd c = yMax))
    let rec f i (x,y) =
        // Check downwards, down left and down right
        if (i = 1000) then
            None
        else if (isAvail (x,y+1)) then
            f (i+1) (x,y+1)
        else if (isAvail (x-1,y+1)) then
            f (i+1) (x-1,y+1)
        else if (isAvail (x+1,y+1)) then
            f (i+1) (x+1,y+1)
        else
            Some (x,y)

    f 0 pour

let solve2 () =
    let yMax = rocks |> Set.toArray |> Array.map snd |> Array.max |> ((+)2)
    let rec f sand =
        match findRest2 rocks sand yMax with
        | Some (500,0) -> 1 + (Set.count sand)
        | Some c -> f (Set.add c sand)
        | None -> failwithf "Not supposed to run out of room before hitting (500,0)"

    f Set.empty

let ans2 = solve2 ()

ans2