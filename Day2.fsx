#load "Helpers.fsx"

open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data = Helpers.Web.getInput 2

type Choice =
    | Rock | Paper | Scissors

let parse =
    function
    | 'X' | 'A' -> Rock
    | 'Y' | 'B' -> Paper
    | 'Z' | 'C' -> Scissors

let shapeScore =
    function
    | Rock -> 1
    | Paper -> 2
    | Scissors -> 3

let winScore (my, their) =
    match (my, their) with
    | Rock, Rock -> 3
    | Rock, Paper -> 0
    | Rock, Scissors -> 6
    | Paper, Rock -> 6
    | Paper, Paper -> 3
    | Paper, Scissors -> 0
    | Scissors, Rock -> 0
    | Scissors, Paper -> 6
    | Scissors, Scissors -> 3

let points (my,their) =
    winScore (my, their) + shapeScore my

let ans1 =
    data
    |> Array.map (fun s -> s.ToCharArray())
    |> Array.sumBy (fun arr -> points (parse arr[2], parse arr[0]))

ans1

/// Part 2

let pointsNeeded =
    function
    | 'X' -> 0
    | 'Y' -> 3
    | 'Z' -> 6

let points2 (outcome, their) =
    let my =
        [|Rock; Paper; Scissors|]
        |> Array.find (fun c -> winScore (c,their) = pointsNeeded outcome)
    points (my, their)

let ans2 = 
    data
    |> Array.map (fun s -> s.ToCharArray())
    |> Array.sumBy (fun arr -> points2 (arr[2], parse arr[0]))

ans2