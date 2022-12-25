#load "Helpers.fsx"

open System
open FSharp.Core.Operators.Checked
open Helpers

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let test =
 """1=-0-2
12111
2=0=
21
2=01
111
20012
112
1=-1=
1-12
12
1=
122""" |> split "\n"

let data = Helpers.Web.getInput 25

let symToDec = function | '=' -> -2. | '-' -> -1. | '0' -> 0 | '1' -> 1 | '2' -> 2

let toDec (s : string) =
    s.ToCharArray()
    |> Array.rev
    |> Array.mapi (fun i x -> (5. ** (float i))*(symToDec x))
    |> Array.sum

let ans1 =
    data
    |> Array.map toDec
    |> Array.sum
    |> int64

let isHigher snafu = int64 (toDec snafu) > ans1
let isLower snafu  = int64 (toDec snafu) < ans1

let t s =
    printfn "%A" ans1
    printfn "%A" (int64 (toDec s))
    printfn "Diff: %A" (ans1 - (int64 (toDec s)))
    isLower s, isHigher s

t "20=2-02-0---02=22=21"

ans1
