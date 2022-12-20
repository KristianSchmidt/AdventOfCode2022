#load "Helpers.fsx"

open System
open Helpers

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let calcIdx i n =
    let modded = i % n
    if (modded < 0) then n + modded else modded

let data =
    let input = //[|"1"; "2"; "-3"; "3"; "-2"; "0"; "4" |]
                Helpers.Web.getInput 20
    
    input
    |> Array.map int
    |> Array.mapi (fun i v -> ((calcIdx (i-1) input.Length, calcIdx (i+1) input.Length),v))
    |> Array.map (fun (c,v) -> c,int64 v)

let getValue (data : ((int*int)*int64) array) i = data[i] |> snd

let rec findNextNeighbor (data : ((int*int)*int64) array) next i =
    if (i = 0L) then next
    else if (i < 0L) then
        findNextNeighbor data (data[next] |> fst |> fst) (i+1L)
    else
        findNextNeighbor data (data[next] |> fst |> snd) (i-1L)

let doUpdate i (data : ((int*int)*int64) array) =
    let len = int64 (data.Length)
    let ((backwardN, forwardN), value) = data[i]
    
    if (value % (len-1L) = 0L) then 
        ()
    else

    // Who needs to be updated?
    // Both of my neighbors need to be spliced together
    let newForwardN  = ((backwardN, data[forwardN] |> fst |> snd), getValue data forwardN)
    let newBackwardN = ((data[backwardN] |> fst |> fst, forwardN), getValue data backwardN)
    data[backwardN] <- newBackwardN
    data[forwardN] <- newForwardN

    let (myNextBackwardsN, myNextForwardsN) =
        if (value > 0) then
            if (value > len) then
                let backwards = findNextNeighbor data i (value%(len-1L))
                let forwards = findNextNeighbor data backwards 1
                (backwards, forwards)
            else
                let backwards = findNextNeighbor data i value
                let forwards = findNextNeighbor data backwards 1
                (backwards, forwards)
        else
            if (-1L*value > len) then
                let forwards = findNextNeighbor data i (value%(len-1L))
                let backwards = findNextNeighbor data forwards -1
                (backwards, forwards)
            else
                let forwards = findNextNeighbor data i value
                let backwards = findNextNeighbor data forwards -1
                (backwards, forwards)
    
    let nextForwards = ((i, data[myNextForwardsN] |> fst |> snd), getValue data myNextForwardsN)
    let nextBackwards = ((data[myNextBackwardsN] |> fst |> fst, i), getValue data myNextBackwardsN)
    
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