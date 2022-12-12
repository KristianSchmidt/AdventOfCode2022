#load "Helpers.fsx"

open System
open Helpers

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data = Helpers.Web.getInput 10 |> List.ofArray

let solve instrs =
    let rec f instrs cycle x res =
        match instrs with
        | [] -> List.collect id res |> Map.ofList
        | "noop" :: xs ->
            let newRes = [(cycle,x)] :: res
            f xs (cycle+1) x newRes
        | Regex "addx (\-?\d+)" [d] :: xs  ->
            let newRes = [(cycle,x);(cycle+1,x)] :: res
            f xs (cycle+2) (x+(int d)) newRes

    f instrs 1 1 []

let res1 = solve data

let ans1 =
    [20;60;100;140;180;220]
    |> List.sumBy (fun x -> x * res1[x])

ans1

/// Part 2

for i in [1..6] do
    [1..40]
    |> List.iter (fun j ->
        let x = res1[40*(i-1)+j]
        if (x >= j-2 && x <= j) then
            printf "#"
        else
            printf "."
    )
    printfn ""

let ans2 = data

ans2