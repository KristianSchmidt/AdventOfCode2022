#load "Helpers.fsx"

#time "on"

open System
open Helpers

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

//[<Measure>] type ore
//[<Measure>] type clay
//[<Measure>] type obsidian

type Blueprint =
    {
        Number : int
        OreRobotCost : int
        ClayRobotCost : int
        ObsidianRobotCost : int*int
        GeodeRobotCost : int*int
    }

let test = [|"Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian."
             "Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian." |]

let data =
    //test
    Helpers.Web.getInput 19
    |> Array.take 3
    |> Array.map (
        function
        | Regex "Blueprint (\d+): Each ore robot costs (\d+) ore. Each clay robot costs (\d+) ore. Each obsidian robot costs (\d+) ore and (\d+) clay. Each geode robot costs (\d+) ore and (\d+) obsidian."
                [num;oreCost;clayCost;obsidianOre;obsidianClay;geodeOre; geodeObsidian] ->
                    let toMeasure (x : string) = int x
                    { Number = int num
                      OreRobotCost = toMeasure oreCost
                      ClayRobotCost = toMeasure clayCost
                      ObsidianRobotCost = (toMeasure obsidianOre, toMeasure obsidianClay)
                      GeodeRobotCost = (toMeasure geodeOre, toMeasure geodeObsidian) }
        )

// Sub problem to possibly be solved first - how fast can we make one geode?
// OR - can we make an obsidian robot in time (24-[obsidian cost of geode])?

let someIfTrue b v = if (b) then Some v else None

// Let's try with seeing how fast we can make an obsidian bot
// and then possibly starting the solution of the rest of the tree from there
let solveSubproblem (bp : Blueprint) =
    let mutable currMin = Int32.MaxValue
    let mutable res = Set.empty
    //let maxTime = 24 - (snd bp.GeodeRobotCost) + 1

    let rec f (oreBots : int) (oreCount : int)
              (clayBots : int) (clayCount : int)
              minute () =
        if (minute > 24 || minute > currMin) then
            Int32.MaxValue
        else if (oreCount >= (fst bp.ObsidianRobotCost) && clayCount >= (snd bp.ObsidianRobotCost) && minute <= currMin) then
            if (currMin = minute) then
                res <- Set.add (minute, oreBots, oreCount, clayBots, clayCount) res
            else
                res <- Set.ofList [(minute, oreBots, oreCount, clayBots, clayCount)]

            currMin <- minute
            printfn "New min: %i. OreBots: %i. OreCount: %i. ClayBots: %i. ClayCount: %i" currMin oreBots oreCount clayBots clayCount
            minute
        else
            //printfn "Status min: %i. OreBots: %i. OreCount: %i. ClayBots: %i. ClayCount: %i" minute oreBots oreCount clayBots clayCount
            // 3 choices
            // 1. Do nothing
            // 2. Build ore bot
            // 3. Build clay bot
            let doNothing = f oreBots (oreCount + oreBots)
                              clayBots (clayBots + clayCount)
                              (minute + 1) |> Some

            let buildOreRobot = f (oreBots+1) (oreCount + oreBots - bp.OreRobotCost)
                                  clayBots (clayBots + clayCount)
                                  (minute + 1) |> someIfTrue (oreCount >= bp.OreRobotCost)

            let buildClayRobot = f oreBots (oreCount + oreBots - bp.ClayRobotCost)
                                   (clayBots+1) (clayBots + clayCount)
                                   (minute + 1) |> someIfTrue (oreCount >= bp.ClayRobotCost)
            
            [|buildClayRobot; buildOreRobot; doNothing|]
            |> Array.choose id
            |> Array.map (fun f -> f ())
            |> Array.min
            
    let res1 = f 1 0 0 0 1 ()
    printfn "Minimum is %i reached by %i solutions" res1 (Set.count res)
    res

solveSubproblem data[1]

let solve (bp : Blueprint) (oreBots : int) (oreCount : int) (clayBots : int) (clayCount : int) (startMinute : int) =
    let mutable currMax = 0
    let s = new System.Diagnostics.Stopwatch()
    s.Start()

    let mutable stateMap : Map<(int*int*int*int*int*int*int*int*int),int> = Map.empty
    let mutable cacheHits = 0

    let rec f (oreBots : int) (oreCount : int)
              (clayBots : int) (clayCount : int)
              (obsidianBots : int) (obsidianCount : int) 
              (geodeBots : int) (geodeCount : int)
              (minute : int) () =
        
        let key = (oreBots, oreCount, clayBots, clayCount, obsidianBots, obsidianCount, geodeBots, geodeCount, minute)
        
        if (Map.containsKey key stateMap) then
            cacheHits <- cacheHits + 1
            stateMap[key]
        else
        
            // Max potential would be to get a new geode every minute
            // So the first contribs (24-minute), the next one (24-minute-1), then (24-minute-2) ...
            // I.e. the sum from 0 to (24-minute)
            // (24-minute) * ((24-minute)-1) / 2
            let potential =
                let canAffordGeodeBotNow = 0
                    //if (oreCount >= fst bp.GeodeRobotCost && obsidianCount >= snd bp.GeodeRobotCost) then 0 else 1
                geodeCount
                + geodeBots * (32 - minute + 1) 
                + ((32-minute-canAffordGeodeBotNow) * ((32-minute-canAffordGeodeBotNow)-1)) / 2
            
            let potential = potential + 10
            if (minute = 32) then
                let finalCount = geodeBots + geodeCount
                if (finalCount > currMax) then
                    currMax <- finalCount
                    printfn "BP #%i New max: %i - Time: %A - GeodeBots: %i" bp.Number currMax s.Elapsed geodeBots
                finalCount
            else if (potential <= currMax) then
                -1
            else
        
            // Available decisions:
            //
            // 1. Do nothing - increases count by #robots
            // 2. Build ore orbot - decreases ore and increases count by #robots
            // 3. Build clay robot - decreases ore and increases count by #robots
            // 4. Build obsidian robot - decreases ore and clay and increases count by #robots
            // 5. Build Geode robot - decreases ore and obsidian and increases count by #robots
        
            let doNothing = f oreBots (oreCount + oreBots)
                              clayBots (clayBots + clayCount)
                              obsidianBots (obsidianBots + obsidianCount)
                              geodeBots (geodeBots + geodeCount)
                              (minute + 1) |> Some

            let buildOreRobot = f (oreBots+1) (oreCount + oreBots - bp.OreRobotCost)
                                  clayBots (clayBots + clayCount)
                                  obsidianBots (obsidianBots + obsidianCount)
                                  geodeBots (geodeBots + geodeCount)
                                  (minute + 1) |> someIfTrue (oreCount >= bp.OreRobotCost)

            let buildClayRobot = f oreBots (oreCount + oreBots - bp.ClayRobotCost)
                                   (clayBots+1) (clayBots + clayCount)
                                   obsidianBots (obsidianBots + obsidianCount)
                                   geodeBots (geodeBots + geodeCount)
                                   (minute + 1) |> someIfTrue (oreCount >= bp.ClayRobotCost)

            let buildObsidianRobot = f oreBots (oreCount + oreBots - (fst bp.ObsidianRobotCost))
                                       clayBots (clayBots + clayCount - (snd bp.ObsidianRobotCost))
                                       (obsidianBots+1) (obsidianBots + obsidianCount)
                                       geodeBots (geodeBots + geodeCount)
                                       (minute + 1) |> someIfTrue (oreCount >= fst bp.ObsidianRobotCost && clayCount >= snd bp.ObsidianRobotCost)

            let buildGeodeRobot = f oreBots (oreCount + oreBots - (fst bp.GeodeRobotCost))
                                    clayBots (clayBots + clayCount)
                                    obsidianBots (obsidianBots + obsidianCount - (snd bp.GeodeRobotCost))
                                    (geodeBots+1) (geodeBots + geodeCount)
                                    (minute + 1) |> someIfTrue (oreCount >= fst bp.GeodeRobotCost && obsidianCount >= snd bp.GeodeRobotCost)

            let result =
                //let mapper = if (clayBots = 0) then Array.Parallel.map else Array.map
                [|buildGeodeRobot; buildObsidianRobot; buildClayRobot; buildOreRobot; doNothing|]
                |> Array.choose id
                //|> (fun arr -> printfn "Arr len: %i" arr.Length; arr)
                |> Array.map (fun f -> f ())
                |> Array.max

            stateMap <- Map.add key result stateMap
            result

    //let res = f 1 0 0 0 0 0 0 0 1 ()
    let res = f oreBots oreCount clayBots clayCount 0 0 0 0 startMinute ()
    printfn "Completed BP %i = %i. Cache Size: %i. Cache hits: %i. Time: %A" bp.Number res (Map.count stateMap) (cacheHits) s.Elapsed
    res

//solve data[0] 1 4 3 15 11
//solve data[0] 1 0 0 0 1

let allRes2 =
    data |> Array.Parallel.map (fun bp -> bp.Number, solve bp 1 0 0 0 1)


solveSubproblem data[1]
|> Set.toList
|> List.map (fun (minute,oreBots, oreCount, clayBots, clayCount) -> solve data[1] oreBots oreCount clayBots clayCount minute)

let solve2 (bp : Blueprint) =
    let results = solveSubproblem bp
                  |> Set.toArray
                  |> Array.Parallel.map (fun (minute,oreBots, oreCount, clayBots, clayCount) -> solve bp oreBots oreCount clayBots clayCount minute)

    Array.max results

data
|> Array.map (fun bp -> (solve2 bp) * bp.Number)

let allRes = it

Array.sum allRes

allRes
|> Array.mapi (fun i v -> data[i].Number, v / data[i].Number)
|> Array.iter (printfn "%A")

solve2 data[9]
40*58*38

let ans1 =
    data
    |> Array.Parallel.map (fun bp -> (solve bp) * bp.Number)
    |> Array.sum


ans1

// 1912 too low
// 1871 too low
// 1864 too low


/// Part 2

let ans2 = data

ans2