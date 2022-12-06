#load "Helpers.fsx"

open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data = Helpers.Web.getInput 6

let noDupes [|c1;c2;c3;c4|] =
    c1 <> c2 && c1 <> c3 && c1 <> c4 &&
    c2 <> c3 && c2 <> c4 &&
    c3 <> c4

let startMarker (s : string) =
    s.ToCharArray()
    |> Array.windowed 4
    |> Array.findIndex noDupes
    |> ((+)4)

startMarker "bvwbjplbgvbhsrlpgdmjqwftvncz"

let ans1 =
    data
    |> Array.sumBy startMarker

ans1

/// Part 2

let msgMarker (s : string) =
    s.ToCharArray()
    |> Array.windowed 14
    |> Array.findIndex (fun arr -> let c = Set.ofArray arr |> Set.count
                                   c = 14)
    |> ((+)14)

let ans2 =
    data
    |> Array.sumBy msgMarker

ans2