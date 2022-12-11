#load "Helpers.fsx"

open System
open System.Numerics
open Helpers

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
    
type Monkey = 
    {
        Items     : bigint array
        Operation : bigint -> bigint
        Test      : bigint -> bool
        IfTrue    : int
        IfFalse   : int
    }

let parseMonkey (arr : string array) =
    let items =
        match arr[1] with
        | Regex "  Starting items: (.+)" [s] -> s |> Helpers.split ", " |> Array.map (fun d -> bigint (int d))
    
    let op =
        match arr[2] with
        | Regex "new = old \* (\d+)" [d] -> ((*)(bigint (int64 d)))
        | Regex "new = old \+ (\d+)" [d] -> ((+)(bigint (int64 d)))
        | Regex "new = old \* old" [] -> fun old -> old*old

    let test =
        match arr[3] with
        | Regex " divisible by (\d+)" [d] ->
            let factor = bigint(int64 d)
            (fun s -> s % factor = BigInteger.Zero)

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

let divideItems (items : bigint array) (m : Monkey) =
    items
    |> Array.map (fun i -> (m.Operation i) / (bigint 3L))
    |> Array.groupBy m.Test

let divideItems2 (items : bigint array) (m : Monkey) =
    items
    |> Array.map m.Operation
    |> Array.groupBy m.Test

let doRound (monkies : Monkey array) (counts,items) =
    let rec f mIdx (counts : Map<int,int>) (state : Map<int,bigint array>) =
        let monkey = monkies[mIdx]
        
        let (toTrue,toFalse) =
            let grouped = divideItems2 state[mIdx] monkey
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
    |> List.fold (fun s _ -> doRound monkies s) (counts,items)

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

let solve2 (monkies : Monkey array) =
    let (counts,items) = Map.empty, (monkies |> Array.mapi (fun i m -> i, m.Items) |> Map.ofArray)
    [1..10_000]
    |> List.fold (fun s i ->
        if (i % 100 = 0) then printfn "%i" i
        doRound monkies s) (counts,items)

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

// too high 28497643356

