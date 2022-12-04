#load "Helpers.fsx"

open System
open Helpers

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data =
    Helpers.Web.getInput 4
    |> Array.map (
        function 
        | Regex "(\d+)\-(\d+),(\d+)\-(\d+)" [d1;d2;d3;d4] ->
            ((int d1,int d2),(int d3,int d4)))

let ans1 =
    data
    |> Array.filter (fun ((d1,d2),(d3,d4)) -> (d3 >= d1 && d4 <= d2) || (d1 >= d3 && d2 <= d4))
    |> Array.length

ans1

/// Part 2

let ans2 =
    data
    |> Array.map (fun ((d1,d2),(d3,d4)) -> Set.intersect (Set.ofList [d1..d2]) (Set.ofList [d3..d4]))
    |> Array.filter (Set.isEmpty >> not)
    |> Array.length


ans2