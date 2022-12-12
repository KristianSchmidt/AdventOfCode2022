#load "Helpers.fsx"

open System
open Helpers

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
    
type Monkey = 
    {
        Items     : int64 array
        Operation : int64 -> int64
        Test      : int64 -> bool
        IfTrue    : int
        IfFalse   : int
    }

let parseMonkey (arr : string array) =
    let items =
        match arr[1] with
        | Regex "  Starting items: (.+)" [s] -> s |> Helpers.split ", " |> Array.map (fun d -> int64 (int d))
    
    let op =
        match arr[2] with
        | Regex "new = old \* (\d+)" [d] -> ((*)((int64 d)))
        | Regex "new = old \+ (\d+)" [d] -> ((+)((int64 d)))
        | Regex "new = old \* old" [] -> fun old -> old*old

    let test =
        match arr[3] with
        | Regex " divisible by (\d+)" [d] ->
            let factor = int64 d
            (fun s -> s % factor = 0L)

    let ifTrue =
        match arr[4] with
        | Regex "monkey (\d)" [d] -> int d

    let ifFalse =
        match arr[5] with
        | Regex "monkey (\d)" [d] -> int d

    { Items = items; Operation = op; Test = test; IfTrue = ifTrue; IfFalse = ifFalse}

let data =
    Helpers.Web.getInput 11
    |> Array.chunkBySize 7
    |> Array.map parseMonkey

let divideItems (items : int64 array) (m : Monkey) =
    items
    |> Array.map (fun i -> (m.Operation i) / (3L))
    |> Array.groupBy m.Test

let doRound divideItems (monkies : Monkey array) (counts,items) =
    let rec f mIdx (counts : Map<int,int>) (state : Map<int,int64 array>) =
        let monkey = monkies[mIdx]
        
        let (toTrue,toFalse) =
            let grouped = divideItems state[mIdx] monkey
            Array.tryFind fst grouped |> Option.map snd |> Option.defaultValue [||],
            Array.tryFind (fst >> not) grouped |> Option.map snd |> Option.defaultValue [||]

        let newState =
            state
            |> Map.add monkey.IfTrue (Array.append state[monkey.IfTrue] toTrue)
            |> Map.add monkey.IfFalse (Array.append state[monkey.IfFalse] toFalse)
            |> Map.add mIdx [||]

        let newCounts =
            counts |> Map.add mIdx ((Map.tryFind mIdx counts |> Option.defaultValue 0) + state[mIdx].Length)

        if (mIdx = ((Array.length monkies) - 1)) then
            newCounts, newState
        else
            f (mIdx + 1) newCounts newState

    f 0 counts items
    
let solve (monkies : Monkey array) =
    let (counts,items) = Map.empty, (monkies |> Array.mapi (fun i m -> i,m.Items) |> Map.ofArray)
    [1..20]
    |> List.fold (fun s _ -> doRound divideItems monkies s) (counts,items)

let ans1 =
    solve data
    |> fst
    |> Map.toArray
    |> Array.sortByDescending snd
    |> Array.take 2
    |> Array.map snd
    |> Array.reduce (*)

ans1

/// Part 2

let factor = 5L * 11L * 2L * 13L * 7L  * 3L * 17L * 19L

let divideItems2 (items : int64 array) (m : Monkey) =
    items
    |> Array.map (fun i -> (m.Operation i) % factor)
    |> Array.groupBy m.Test

let solve2 (monkies : Monkey array) =
    let (counts,items) = Map.empty, (monkies |> Array.mapi (fun i m -> i, m.Items) |> Map.ofArray)
    [1..10_000]
    |> List.fold (fun s i -> doRound divideItems2 monkies s) (counts,items)

let ans2 =
    data
    |> solve2
    |> fst
    |> Map.toArray
    |> Array.sortByDescending snd
    |> Array.take 2
    |> Array.map (snd >> bigint)
    |> Array.reduce (*)

ans2

