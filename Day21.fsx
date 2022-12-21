#load "Helpers.fsx"

open System
//open FSharp.Core.Operators.Checked
open Helpers

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

type Input = | Number of int64
             | Add of string * string
             | Mul of string * string
             | Div of string * string
             | Minus of string * string

let monkeys = function | Add (x,y) | Mul (x,y) | Div(x,y) | Minus (x,y) -> (x,y)

let test = [|
    "root: pppw + sjmn"
    "dbpl: 5"
    "cczh: sllz + lgvd"
    "zczc: 2"
    "ptdq: humn - dvpt"
    "dvpt: 3"
    "lfqf: 4"
    "humn: 5"
    "ljgn: 2"
    "sjmn: drzm * dbpl"
    "sllz: 4"
    "pppw: cczh / lfqf"
    "lgvd: ljgn * ptdq"
    "drzm: hmdt - zczc"
    "hmdt: 32"
    |]

let data =
    Helpers.Web.getInput 21
    //test
    |> Array.map (function
                  | Regex "(\w+): (\d+)" [name; num] -> name, Number (int64 num)
                  | Regex "(\w+): (\w+) \* (\w+)" [name1; name2; name3] -> name1, Mul (name2,name3)
                  | Regex "(\w+): (\w+) \/ (\w+)" [name1; name2; name3] -> name1, Div (name2,name3)
                  | Regex "(\w+): (\w+) \+ (\w+)" [name1; name2; name3] -> name1, Add (name2,name3)
                  | Regex "(\w+): (\w+) \- (\w+)" [name1; name2; name3] -> name1, Minus (name2,name3)
                  | s -> failwithf "what: %s" s
                  )
    |> Map.ofArray

//data |> Map.toArray |> Array.filter (snd >> function | Number _ -> true | _ -> false) |> Array.map snd |> Array.sortDescending

let solve map =
    let rec f (map : Map<string,Input>) =
        let isNumber key = match map[key] with | Number i -> Some i | _ -> None
        let newMap =
            map
            |> Map.map (fun k v ->
                match v with
                | Number i -> v
                | otherInput ->
                    let (mo1,mo2) = monkeys otherInput
                    match (isNumber mo1, isNumber mo2), otherInput with
                    | (Some i1, Some i2), Add _ ->
                        printfn "%s (%i) + %s (%i) = %s (%i)" mo1 i1 mo2 i2 k (i1+i2)
                        Number (i1+i2)
                    | (Some i1, Some i2), Mul _ ->
                        printfn "%s (%i) * %s (%i) = %s (%i)" mo1 i1 mo2 i2 k (i1*i2)
                        Number (i1*i2)
                    | (Some i1, Some i2), Div _ ->
                        printfn "%s (%i) / %s (%i) = %s (%i)" mo1 i1 mo2 i2 k (i1/i2)
                        Number (i1/i2)
                    | (Some i1, Some i2), Minus _ ->
                        printfn "%s (%i) + %s (%i) = %s (%i)" mo1 i1 mo2 i2 k (i1-i2)
                        Number (i1-i2)
                    | _ -> v
                )

        match newMap["root"] with
        | Number i -> i
        | _ -> f newMap

    f map

// 2749189173500 too low

let ans1 = solve data

ans1

/// Part 2

let solve2 map humn =
    let rec f (map : Map<string,Input>) =
        let isNumber key = match map[key] with | Number i -> Some i | _ -> None
        let newMap =
            map
            |> Map.map (fun k v ->
                match v with
                | Number i -> v
                | otherInput ->
                    let (mo1,mo2) = monkeys otherInput
                    match (isNumber mo1, isNumber mo2), otherInput with
                    | (Some i1, Some i2), Add _ ->
                        //printfn "%s (%i) + %s (%i) = %s (%i)" mo1 i1 mo2 i2 k (i1+i2)
                        Number (i1+i2)
                    | (Some i1, Some i2), Mul _ ->
                        //printfn "%s (%i) * %s (%i) = %s (%i)" mo1 i1 mo2 i2 k (i1*i2)
                        Number (i1*i2)
                    | (Some i1, Some i2), Div _ ->
                        //printfn "%s (%i) / %s (%i) = %s (%i)" mo1 i1 mo2 i2 k (i1/i2)
                        Number (i1/i2)
                    | (Some i1, Some i2), Minus _ ->
                        //printfn "%s (%i) + %s (%i) = %s (%i)" mo1 i1 mo2 i2 k (i1-i2)
                        Number (i1-i2)
                    | _ -> v
                )

        let (rootM1, rootM2) = monkeys newMap["root"]
        match newMap[rootM1], newMap[rootM2] with
        | Number i1, Number i2 when i1 = i2 -> Some humn
        | Number i1, Number i2 when i1 <> i2 -> None
        | _ -> f newMap
        
    f (map |> Map.add "humn" (Number humn))

[1L..40000L]
|> List.pick (fun i -> printfn "Calcing %i" i; solve2 data i)


let ans2 = data

ans2