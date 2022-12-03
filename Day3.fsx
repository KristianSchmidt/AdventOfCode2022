#load "Helpers.fsx"

open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data =
    Helpers.Web.getInput 3
    |> Array.map (fun s -> s.ToCharArray())
    // Could be Array.splitAt instead
    |> Array.map (fun arr -> arr[0..(arr.Length/2 - 1)], arr[(arr.Length/2)..])
    |> Array.map (fun (a1,a2) -> Set.intersect (Set.ofArray a1) (Set.ofArray a2))

let point (c : char) =
    match Char.IsUpper(c) with
    | true -> int c - int 'A' + 27
    | false -> int c - int 'a' + 1

let ans1 =
    data
    |> Array.sumBy (fun s -> s |> Set.toSeq |> Seq.head |> point)

/// Part 2

let ans2 =
    Helpers.Web.getInput 3
    |> Array.map (fun s -> s.ToCharArray())
    |> Array.chunkBySize 3
    |> Array.map (Array.map Set.ofArray >> Set.intersectMany)
    |> Array.sumBy (fun s -> s |> Set.toSeq |> Seq.head |> point)


ans2