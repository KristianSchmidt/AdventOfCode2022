#load "Helpers.fsx"

open System
open System.Collections
open Helpers

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data = Helpers.Web.getInput 5

let findLetter (arr : char array) =
    if (List.contains arr[1] ['A'..'Z']) then
        Some arr[1]
    else
        None

let makeStacks () =
    let stacks = Array.init 9 (fun _ -> new Stack())
    
    data[0..7]
    |> Array.rev
    |> Array.map (fun s -> s.ToCharArray() |> Array.chunkBySize 4 |> Array.map findLetter)
    |> Array.iter (fun arr -> arr 
                              |> Array.iteri
                                    (fun i lOpt -> 
                                        lOpt 
                                        |> Option.iter (fun l -> stacks[i].Push(l))))

    stacks
    
let moves =
    data[10..]
    |> Array.map (
        function
        | Regex "move (\d+) from (\d+) to (\d+)" [count;fromStack;toStack] -> (int count,int fromStack,int toStack))

let stacks1 = makeStacks ()

let execMove1 (count,fromStack,toStack) =
    let iFrom = fromStack - 1
    let iTo = toStack - 1
    let temp = new Stack()
    for i in 1 .. count do
        temp.Push(stacks1[iFrom].Pop())
        stacks1[iTo].Push(temp.Pop())

moves |> Array.iter execMove1

let ans1 =
    stacks1
    |> Array.map (fun s -> s.Pop() :?> char)
    |> (fun arr -> new String(arr))

ans1

/// Part 2

let stacks2 = makeStacks ()

let execMove2 (count,fromStack,toStack) =
    let iFrom = fromStack - 1
    let iTo = toStack - 1
    let temp = new Stack()
    for i in 1 .. count do
        temp.Push(stacks2[iFrom].Pop())
    for i in 1 .. count do
        stacks2[iTo].Push(temp.Pop())

moves |> Array.iter execMove2

let ans2 =
    stacks2
    |> Array.map (fun s -> s.Pop() :?> char)
    |> (fun arr -> new String(arr))

ans2