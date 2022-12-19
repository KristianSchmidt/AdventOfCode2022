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
    //|> Array.take 3
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

let someIfTrue b v = if (b) then Some v else None

let solve (bp : Blueprint) maxMinute =
    let mutable currMax = 0
    let s = new System.Diagnostics.Stopwatch()
    s.Start()

    let mutable stateMap : Map<((int*int)*(int*int)*(int*int)*int*int*int),int> = Map.empty
    let mutable cacheHits = 0

    let maxOreSpend = Array.max [| bp.OreRobotCost; bp.ClayRobotCost; fst bp.ObsidianRobotCost; fst bp.GeodeRobotCost|]
    let maxClaySpend = snd bp.ObsidianRobotCost
    let maxObsidianSpend = snd bp.GeodeRobotCost

    let rec f (oreBots : int) (oreCount : int)
              (clayBots : int) (clayCount : int)
              (obsidianBots : int) (obsidianCount : int) 
              (geodeBots : int) (geodeCount : int)
              (minute : int) () =
        
        let key =
            let oreCountKey =
                if (oreCount >= maxOreSpend * (maxMinute - minute + 1)) then
                    (Int32.MaxValue, Int32.MaxValue)
                else
                    oreBots, oreCount
            let clayCountKey =
                if (clayCount >= maxClaySpend * (maxMinute - minute + 1)) then
                    Int32.MaxValue, Int32.MaxValue
                else
                    clayBots, clayCount
            let obsidianCountKey =
                if (obsidianCount >= maxObsidianSpend * (maxMinute - minute + 1)) then
                    Int32.MaxValue, Int32.MaxValue
                else
                    obsidianBots, obsidianCount
            (oreCountKey, clayCountKey, obsidianCountKey, geodeBots, geodeCount, minute)
        
        if (Map.containsKey key stateMap) then
            cacheHits <- cacheHits + 1
            stateMap[key]
        else
        
            // Max potential would be to get a new geode every minute
            // So the first contribs (24-minute), the next one (24-minute-1), then (24-minute-2) ...
            // I.e. the sum from 0 to (24-minute)
            // (24-minute) * ((24-minute)-1) / 2
            let potential =
                let minLeft = maxMinute - minute + 1
                let canAffordGeodeBotNow =
                    oreCount >= fst bp.GeodeRobotCost && obsidianCount >= snd bp.GeodeRobotCost
                if (canAffordGeodeBotNow) then
                    geodeCount +
                    geodeBots * minLeft +
                    (minLeft * (minLeft + 1)) / 2 
                else
                    geodeCount +
                    geodeBots * minLeft +
                    ((minLeft - 1) * minLeft) / 2 
                            
            let potential = potential + 0
            if (minute = maxMinute) then
                let finalCount = geodeBots + geodeCount
                if (finalCount > currMax) then
                    currMax <- finalCount
                    printfn "BP #%i New max: %i - Time: %A - GeodeBots: %i" bp.Number currMax s.Elapsed geodeBots
                finalCount
            //else if (potential <= currMax) then
            //    -1
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
                                  (minute + 1) |> someIfTrue (oreCount >= bp.OreRobotCost && oreBots < maxOreSpend)

            let buildClayRobot = f oreBots (oreCount + oreBots - bp.ClayRobotCost)
                                   (clayBots+1) (clayBots + clayCount)
                                   obsidianBots (obsidianBots + obsidianCount)
                                   geodeBots (geodeBots + geodeCount)
                                   (minute + 1) |> someIfTrue (oreCount >= bp.ClayRobotCost && clayBots < maxClaySpend)

            let buildObsidianRobot = f oreBots (oreCount + oreBots - (fst bp.ObsidianRobotCost))
                                       clayBots (clayBots + clayCount - (snd bp.ObsidianRobotCost))
                                       (obsidianBots+1) (obsidianBots + obsidianCount)
                                       geodeBots (geodeBots + geodeCount)
                                       (minute + 1) |> someIfTrue (oreCount >= fst bp.ObsidianRobotCost && clayCount >= snd bp.ObsidianRobotCost && obsidianBots < maxObsidianSpend)

            let buildGeodeRobot = f oreBots (oreCount + oreBots - (fst bp.GeodeRobotCost))
                                    clayBots (clayBots + clayCount)
                                    obsidianBots (obsidianBots + obsidianCount - (snd bp.GeodeRobotCost))
                                    (geodeBots+1) (geodeBots + geodeCount)
                                    (minute + 1) |> someIfTrue (oreCount >= fst bp.GeodeRobotCost && obsidianCount >= snd bp.GeodeRobotCost)

            let result =
                if (buildGeodeRobot.IsSome) then
                    // If we can afford it, always build a geode bot
                    buildGeodeRobot.Value ()
                else if (buildObsidianRobot.IsSome) then
                    // If we can't build a geode, but we can build an obsidian, then either do that or do nothing
                    max (doNothing.Value ()) (buildObsidianRobot.Value ())
                else
                    [|buildGeodeRobot; buildObsidianRobot; buildClayRobot; buildOreRobot; doNothing|]
                    |> Array.choose id
                    |> Array.map (fun f -> f ())
                    |> Array.max

            stateMap <- Map.add key result stateMap
            result

    let res = f 1 0 0 0 0 0 0 0 1 ()
    printfn "Completed BP %i = %i. Cache Size: %i. Cache hits: %i. Time: %A" bp.Number res (Map.count stateMap) (cacheHits) s.Elapsed
    res

//solve data[0] 1 4 3 15 11
//solve data[20] 1 0 0 0 1

let ans1 =
    data
    |> Array.Parallel.map (fun bp -> bp.Number, solve bp 24)
    |> Array.sumBy (fun (num,res) -> num * res)

ans1

/// Part 2

let ans2 =
    data
    |> Array.take 3
    |> Array.Parallel.map (fun bp -> solve bp 32)
    |> Array.reduce (*)


ans2