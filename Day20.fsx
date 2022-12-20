#load "Helpers.fsx"

open System
open Helpers

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let calcIdx i n =
    let modded = i % n
    if (modded < 0) then n + modded else modded

let data =
    let input = Helpers.Web.getInput 20 //[|"1"; "2"; "-3"; "3"; "-2"; "0"; "4" |]
    
    input
    |> Array.map int
    |> Array.mapi (fun i v -> ((calcIdx (i-1) input.Length, calcIdx (i+1) input.Length),v))
    |> Array.map (fun (c,v) -> c,int64 v)

let rec findNextNeighbor (data : ((int*int)*int64) array) next i =
    if (i = 0L) then next
    else if (i < 0L) then
        findNextNeighbor data (data[next] |> fst |> fst) (i+1L)
    else
        findNextNeighbor data (data[next] |> fst |> snd) (i-1L)

let doUpdate i (data : ((int*int)*int64) array) =
    let getValue i = data[i] |> snd
    let getForward i = data[i] |> fst |> snd
    let getBackward i = data[i] |> fst |> fst
    
    let len = int64 (data.Length) - 1L
    let ((backwardN, forwardN), value) = data[i]
    
    if (value % len = 0L) then 
        ()
    else

    // Who needs to be updated?
    // Both of my neighbors need to be spliced together
    let newForwardN  = ((backwardN, getForward forwardN), getValue forwardN)
    let newBackwardN = ((getBackward backwardN, forwardN), getValue backwardN)
    data[backwardN] <- newBackwardN
    data[forwardN] <- newForwardN

    let (myNextBackwardsN, myNextForwardsN) =
        let n1 = findNextNeighbor data i (value%len)
        
        if (value > 0) then
            (n1, findNextNeighbor data n1 1)
        else
            (findNextNeighbor data n1 -1, n1)
            
    let nextForwards = ((i, getForward myNextForwardsN), getValue myNextForwardsN)
    let nextBackwards = ((getBackward myNextBackwardsN, i), getValue myNextBackwardsN)
    
    data[i] <- ((myNextBackwardsN, myNextForwardsN), value)
    data[myNextForwardsN] <- nextForwards
    data[myNextBackwardsN] <- nextBackwards

let solve iters decryptKey data =
    let data' = Array.copy data |> Array.map (fun (c,v) -> c,decryptKey*v)
    for _ in 1 .. iters do
        [0..data'.Length-1] |> List.iter (fun i -> doUpdate i data')

    let zeroIdx = data' |> Array.findIndex (fun (_,v) -> v = 0)

    [| findNextNeighbor data' zeroIdx 1000
       findNextNeighbor data' zeroIdx 2000
       findNextNeighbor data' zeroIdx 3000 |]
    |> Array.sumBy (fun idx -> snd data'[idx])

let ans1 = solve 1 1L data

ans1

/// Part 2

let decryptKey = 811589153L

let ans2 = solve 10 decryptKey data

ans2