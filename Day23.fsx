#load "Helpers.fsx"

open System
open Helpers

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

type Direction = | N | S | E | W | NE | SE | NW | SW

let draw (set : Set<int*int>) =
    let minX = set |> Set.toArray |> Array.minBy fst |> fst
    let maxX = set |> Set.toArray |> Array.maxBy fst |> fst
    let minY = set |> Set.toArray |> Array.minBy snd |> snd
    let maxY = set |> Set.toArray |> Array.maxBy snd |> snd
    for y in minY .. maxY do
        printf "%2i: " y
        for x in minX .. maxX do
            if (Set.contains (x,y) set) then
                printf "#"
            else
                printf "."
        printfn ""

let neighbor (x,y) =
    function
    | N -> (x,y-1)
    | S -> (x,y+1)
    | E -> (x+1,y)
    | W -> (x-1,y)
    | NE -> (x+1,y-1)
    | SE -> (x+1,y+1)
    | NW -> (x-1,y-1)
    | SW -> (x-1,y+1)

let allNeighbors (x,y) = [|N;S;E;W;NE;SE;NW;SW|] |> Array.map (neighbor (x,y))

let test = """....#..
..###.#
#...#.#
.#...##
#.###..
##.#.##
.#..#..""" |> split "\n"

let data =
    Helpers.Web.getInput 23
    //test
    |> Array.mapi (fun y s -> s.ToCharArray() |> Array.mapi (fun x c -> ((x,y),c)))
    |> Array.collect id
    |> Array.filter (snd >> ((=)'#'))
    |> Array.map fst
    |> Set.ofArray

let directionOrder = [| [|N;NE;NW|]; [|S;SE;SW|]; [|W; NW; SW|]; [|E; NE; SE|] |]

let doRound set i =
    let noElf c = Set.contains c set |> not
    let noElves arr = arr |> Array.forall noElf
    let propose c =
        let noNeighbors = allNeighbors c |> noElves
        if (noNeighbors) then c
        else
            [|i .. i+3|]
            |> Array.tryPick (fun i ->
                let n = directionOrder[i % 4] |> Array.map (neighbor c)
                //printfn "%A - %A" c n
                if (noElves n) then
                    Some (neighbor c (directionOrder[i % 4][0]))
                else
                    None)
            |> Option.defaultValue c // If all neighbors are occupied we stay
    
    let proposals =
        set
        |> Set.toArray
        |> Array.map (fun c -> c, propose c)

    //proposals |> Array.iter (fun (c1,c2) -> printfn "%A -> %A" c1 c2)

    let banned =
        proposals
        |> Array.map snd
        |> Array.countBy id
        |> Array.filter (fun (c,count) -> count > 1)
        |> Array.map fst
        |> Set.ofArray
    
    let newSet =
        proposals
        |> Array.map (fun (c1,c2) -> if (Set.contains c2 banned) then c1 else c2)
        |> Set.ofArray

    if (newSet = set) then
        printfn "No elves moved in round %i" (i+1)

    newSet

let res =
    [0..9]
    |> List.fold doRound data

let xWidth,yWidth =
    let xmin = res |> Set.toArray |> Array.minBy fst |> fst
    let ymin = res |> Set.toArray |> Array.minBy snd |> snd
    let xmax = res |> Set.toArray |> Array.maxBy fst |> fst
    let ymax = res |> Set.toArray |> Array.maxBy snd |> snd
    xmax - xmin + 1, ymax - ymin + 1

let ans1 = xWidth*yWidth - data.Count

ans1

/// Part 2

// Below code outputs the answer from printf
[0..1_000] |> List.fold doRound data

let ans2 = data

ans2