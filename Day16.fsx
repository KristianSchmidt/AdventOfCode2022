#load "Helpers.fsx"

#time "on"

open System
open Helpers

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let testInput = """Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
Valve BB has flow rate=13; tunnels lead to valves CC, AA
Valve CC has flow rate=2; tunnels lead to valves DD, BB
Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
Valve EE has flow rate=3; tunnels lead to valves FF, DD
Valve FF has flow rate=0; tunnels lead to valves EE, GG
Valve GG has flow rate=0; tunnels lead to valves FF, HH
Valve HH has flow rate=22; tunnel leads to valve GG
Valve II has flow rate=0; tunnels lead to valves AA, JJ
Valve JJ has flow rate=21; tunnel leads to valve II""" |> Helpers.split "\n"

let parse =
    function
    | Regex "Valve ([A-Z]+) has flow rate=(\d+); tunnels? leads? to valves? (.+)" [valve; flow; leadsTo] ->
        (valve, int flow, Helpers.split ", " leadsTo)

let data =
    //testInput
    Helpers.Web.getInput 16
    |> Array.map parse
    |> Array.sortBy (fun (_,x,_) -> x)

let rates =
    data
    |> Array.map (fun (valve, flow, _) -> (valve,flow))
    |> Map.ofArray

let paths =
    data
    |> Array.map (fun (valve, _, leadsTo) -> (valve,leadsTo))
    |> Map.ofArray

let pathsAsTuple = paths |> Map.map (fun k v -> v |> Array.map (fun p -> (p,0)))

let pathsAsTupleWithOpen =
    paths |> Map.map (fun k v ->
        let arr = v |> Array.map (fun p -> (p,0))
        Array.append [|(k, rates[k])|] arr
        )


let sortedRates = rates |> Map.values |> Seq.sortDescending |> Seq.filter (fun i -> i > 0) |> Array.ofSeq

let nodes = rates |> Map.toArray |> Array.map fst |> Set.ofArray

data
|> Array.collect (fun (v,_,es) -> es |> Array.map (fun e -> (v,e)))
|> Array.sort
|> Array.distinct
|> Array.iter (fun (v,e) -> printfn "%s-%s" v e)
(*

Can I beat max?

Have 30 - currentMin to do so
Since every open requires at least a move and a open, max open valves are (30 - currentMin) / 2
They can, in the best case, be run for sum = HIGHEST_VALVE * (30 - (currentMin + 1)) + SECOND_HIGHEST_VALVE * (30 - (currentMin + 3)) .. mins

So check if (nextAlreadyFlown + sum > currMax)

*)

let solve () =
    let mutable currMax = 0

    let rec f currPos minute flowRate (alreadyFlowed : int) openFlows =
        let nextAlreadyFlowed = flowRate + alreadyFlowed
        if (minute = 30) then
            if (nextAlreadyFlowed > currMax) then
                currMax <- nextAlreadyFlowed
                printfn "New max: %i" currMax
            nextAlreadyFlowed
        else
            let potential =
                let numValves = (30 - minute) / 2
                if (numValves > sortedRates.Length) then
                    Int32.MaxValue
                else
                    let sum = [1..numValves] |> Seq.sumBy (fun i -> sortedRates[i-1] * (30 - (minute + i*2-1)))
                    nextAlreadyFlowed + sum + flowRate * (30 - minute)

            if (potential > currMax) then
                let availPaths = paths[currPos]
                let currPosFlowRate = rates[currPos]

                let openHere =
                    if (currPosFlowRate > 0 && not (Set.contains currPos openFlows)) then
                        f currPos (minute + 1) (flowRate + currPosFlowRate) nextAlreadyFlowed (Set.add currPos openFlows)
                    else
                        -1

                let maxOfPaths =
                    let mapper = if flowRate = 0 then Array.Parallel.map else Array.map
                    
                    availPaths
                    |> Array.sortByDescending (fun pos -> rates[pos])
                    |> mapper (fun p -> f p (minute + 1) flowRate nextAlreadyFlowed openFlows)
                    |> Array.max

                max maxOfPaths openHere
            else
                -1

    f "AA" 1 0 0 Set.empty

let ans1 = solve ()

ans1

/// Part 2

let solve2 () =
    //let mutable currMax = 2343
    let mutable currMax = 0
    let maxMinute = 26
    let s = new System.Diagnostics.Stopwatch()
    s.Start()

    let rec f (mePrevPos,currPos) (elePrevPos,elePos) minute flowRate (alreadyFlowed : int) openFlows =
        let nextAlreadyFlowed = flowRate + alreadyFlowed
        if (minute = maxMinute) then
            if (nextAlreadyFlowed > currMax) then
                currMax <- nextAlreadyFlowed
                printfn "New max: %i - Time: %A" currMax s.Elapsed
            nextAlreadyFlowed
        else
            let potential =
                let numValves = (maxMinute - minute) / 2
                //let availNodes = Set.difference nodes openFlows
                //let sortedRates = availNodes |> Set.toSeq |> Seq.map (fun x -> rates[x]) |> Seq.sortDescending |> Seq.toArray
                //if (numValves > sortedRates.Length) then
                //    Int32.MaxValue
                //else
                //let sum =
                //    [1..(min numValves sortedRates.Length)]
                //    |> List.map (fun i -> sortedRates[i-1])
                //    |> List.chunkBySize 2
                //    |> List.mapi (fun i lst -> (List.sum lst) * (26 - (minute + i*2-1)))
                //    |> List.sum
                let mutable sum = 0
                for i in 0 .. (min numValves (sortedRates.Length / 2)) do
                    sum <- sum + sortedRates[2*i] * (maxMinute - (minute + 2*i + 1)) 
                    if (2*i+1 < sortedRates.Length) then
                        sum <- sum + sortedRates[2*i+1] * (maxMinute - (minute + 2*i - 1))


                //let sum = [1..(min numValves sortedRates.Length)] |> Seq.sumBy (fun i -> sortedRates[i-1] * (26 - (minute + i-1)))
                //let sum = [1..(min numValves sortedRates.Length)] |> Seq.sumBy (fun i -> sortedRates[i-1] * (26 - (minute + i*2-1)))

                nextAlreadyFlowed + sum + flowRate * (maxMinute - minute)

            if (potential > currMax) then
                let meChoices =
                    if (rates[currPos] > 0 && not (Set.contains currPos openFlows)) then
                        pathsAsTupleWithOpen[currPos]
                    else
                        pathsAsTuple[currPos]
                
                let eleChoices =
                    if (rates[elePos] > 0 && not (Set.contains elePos openFlows)) then
                        pathsAsTupleWithOpen[elePos]
                    else
                        pathsAsTuple[elePos]

                ///
                //let mutable choiceSum = nextAlreadyFlowed + (26 - minute - 1) * flowRate
                //
                //if (currPos <> elePos && rates[currPos] > 0 && rates[elePos] > 0 && not (Set.contains currPos openFlows) && not (Set.contains elePos openFlows)) then
                //    //printfn "%A %A %A %A %A" currPos elePos minute flowRate openFlows
                //    let res = f (currPos,currPos) (elePos,elePos) (minute + 1) (flowRate + rates[currPos] + rates[elePos]) nextAlreadyFlowed (openFlows |> Set.add currPos |> Set.add elePos)
                //    if (res > choiceSum) then
                //        choiceSum <- res
                //
                //// Open elePos and move with me
                //if (rates[elePos] > 0 && not (Set.contains elePos openFlows)) then
                //    for meNextPos in paths[currPos] do
                //        let res = f (currPos,meNextPos) (elePos,elePos) (minute + 1) (flowRate + rates[elePos]) nextAlreadyFlowed (Set.add elePos openFlows)
                //        if (res > choiceSum) then
                //            choiceSum <- res
                //
                //// Open currPos and move with ele
                //if (rates[currPos] > 0 && not (Set.contains currPos openFlows)) then
                //    for eleNextPos in paths[elePos] do
                //        let res = f (currPos,currPos) (elePos,eleNextPos) (minute + 1) (flowRate + rates[currPos]) nextAlreadyFlowed (Set.add currPos openFlows)
                //        if (res > choiceSum) then
                //            choiceSum <- res
                //
                //for meNextPos in paths[currPos] do
                //    if (meNextPos = mePrevPos) then
                //        ()
                //    else
                //        // Cases where both move to another tile
                //        for eleNextPos in paths[elePos] do
                //            if (eleNextPos = elePrevPos) then
                //                ()
                //            else
                //                let res = f (currPos,meNextPos) (elePos,eleNextPos) (minute + 1) flowRate nextAlreadyFlowed openFlows
                //                if (res > choiceSum) then
                //                    choiceSum <- res
                //
                //choiceSum
                ///
                let mutable choiceSum = nextAlreadyFlowed + (maxMinute - minute - 1) * flowRate
                
                for (meNextPos,meContrib) in meChoices do
                    if (meNextPos = mePrevPos) then
                        ()
                    else
                        for (eleNextPos,eleContrib) in eleChoices do
                            if (meContrib = eleContrib && eleNextPos = meNextPos && meContrib > 0) then
                                // No opening the same faucet
                                ()
                            else if (eleNextPos = elePrevPos) then
                                ()
                            else
                                let newOpens =
                                    let open1 = if (meContrib > 0) then openFlows |> Set.add meNextPos else openFlows
                                    if (eleContrib > 0) then open1 |> Set.add eleNextPos else open1
                                
                                let res = f (currPos,meNextPos) (elePos,eleNextPos) (minute + 1) (flowRate + meContrib + eleContrib) nextAlreadyFlowed newOpens
                                if (res > choiceSum) then
                                    choiceSum <- res
                
                choiceSum
            else
                -1

    f ("AA","AA") ("AA","AA") 1 0 0 Set.empty

solve2 ()

// 2425 correct!
// 2412 not correct
// 2379 not correct
// 2343 too low
// 2289 too low

let ans2 = data

ans2