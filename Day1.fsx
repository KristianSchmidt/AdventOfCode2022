#load "Helpers.fsx"

open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data = Helpers.Web.getInput 1

let groups =
    data
    |> Array.mapi (fun i x -> (i,x))
    |> Array.filter (snd >> ((=)""))
    |> Array.map fst
    |> Array.append [|-1|]
    |> Array.pairwise
    |> Array.map (fun (i, j) -> Array.sub data (i+1) (j-i-1))
    |> Array.map (Array.map int)

let ans1 =
    groups |> Array.maxBy (Array.sum) |> Array.sum

ans1

/// Part 2

let ans2 =
    groups
    |> Array.sortByDescending (Array.sum) 
    |> Array.take 3
    |> Array.sumBy Array.sum

ans2