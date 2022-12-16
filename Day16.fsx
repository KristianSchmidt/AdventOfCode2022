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

let sortedRates = rates |> Map.values |> Seq.sortDescending |> Seq.filter (fun i -> i > 0) |> Array.ofSeq

let nodes = rates |> Map.toArray |> Array.map fst |> Set.ofArray

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

let r = System.Random();
let randomSort (arr : 'a array) =
    arr |> Array.sortBy (fun _ -> r.NextDouble())

let solve2 () =
    let mutable currMax = 0

    let rec f currPos elePos minute flowRate (alreadyFlowed : int) openFlows =
        let nextAlreadyFlowed = flowRate + alreadyFlowed
        if (minute = 26) then
            if (nextAlreadyFlowed > currMax) then
                currMax <- nextAlreadyFlowed
                printfn "New max: %i - Open flows: %i" currMax (Set.count openFlows)
            nextAlreadyFlowed
        else
            let potential =
                let numValves = (26 - minute)
                let availNodes = Set.difference nodes openFlows
                let sortedRates = availNodes |> Set.toSeq |> Seq.map (fun x -> rates[x]) |> Seq.sortDescending |> Seq.toArray
                //if (numValves > sortedRates.Length) then
                //    Int32.MaxValue
                //else
                //let sum =
                //    [1..(min numValves sortedRates.Length)]
                //    |> List.map (fun i -> sortedRates[i-1])
                //    |> List.chunkBySize 2
                //    |> List.mapi (fun i lst -> (List.sum lst) * (26 - (minute + i*2-1)))
                //    |> List.sum
                let sum = [1..(min numValves sortedRates.Length)] |> Seq.sumBy (fun i -> sortedRates[i-1] * (26 - (minute + i-1)))
                nextAlreadyFlowed + sum + flowRate * (26 - minute)

            if (potential > currMax) then
                let meChoices =
                    let c = paths[currPos] |> Array.sortByDescending (fun pos -> rates[pos]) |> Array.map (fun p -> (p,0))
                    if (rates[currPos] > 0 && not (Set.contains currPos openFlows)) then
                        Array.append [|(currPos, rates[currPos])|] c
                    else
                        c

                let eleChoices =
                    let c = paths[elePos] |> Array.sortByDescending (fun pos -> rates[pos]) |> Array.map (fun p -> (p,0))
                    if (rates[elePos] > 0 && not (Set.contains elePos openFlows)) then
                        Array.append [|(elePos, rates[elePos])|] c
                    else
                        c

                let mapper = if (flowRate = 0) then Array.Parallel.map else Array.map
                //let mapper = Array.map
                Array.allPairs meChoices eleChoices
                |> Array.filter (fun ((mePos,meContrib),(elePos,eleContrib)) -> not (meContrib = eleContrib && elePos = mePos && meContrib > 0))
                //|> Array.filter (fun ((mePos,meContrib),(elePos,eleContrib)) -> mePos <> elePos) // no use traveling to the same place?
                |> Array.sortByDescending (fun ((mePos,meContrib),(elePos,eleContrib)) -> meContrib + eleContrib)
                |> mapper (fun ((mePos,meContrib),(elePos,eleContrib)) -> 
                    let newOpens =
                        let open1 = if (meContrib > 0) then openFlows |> Set.add mePos else openFlows
                        if (eleContrib > 0) then open1 |> Set.add elePos else open1

                    f mePos elePos (minute + 1) (flowRate + meContrib + eleContrib) nextAlreadyFlowed newOpens
                )
                |> Array.max
            else
                -1

    f "AA" "AA" 1 0 0 Set.empty

solve2 ()

// 2412 not correct
// 2379 not correct
// 2343 too low
// 2289 too low

let ans2 = data

ans2