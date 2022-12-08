#load "Helpers.fsx"

open System
open Helpers

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

type FileSystemElement =
    | Dir of name : string
    | File of name : string * size : int

type Command =
    | Cd of dir : string
    | Ls

type InputType = | Command of Command | Fse of FileSystemElement

let parse =
    function
    | Regex "\$ cd (.+)" [dir] -> Command (Cd dir)
    | Regex "\$ ls" [] -> Command (Ls)
    | Regex "dir (.+)" [dir] -> Fse (Dir dir)
    | Regex "(\d+) (.+)" [size; name] -> Fse (File (name,int size))

let data = Helpers.Web.getInput 7 |> Array.map parse

let getSizes (lst : InputType list) =
    let rec f (dirsToAddTo : string list) (lst : InputType list) (sizes : Map<string,int>) =
        match lst with
        | Command (Cd "..") :: xs ->
            f (List.tail dirsToAddTo) xs sizes
        | Command (Cd dir) :: xs ->
            let fullname =
                (List.tryHead dirsToAddTo |> Option.defaultValue "") + "/" + dir
            f (fullname :: dirsToAddTo) xs sizes
        | Command Ls :: xs ->
            f dirsToAddTo xs sizes
        | Fse (Dir _) :: xs ->
            f dirsToAddTo xs sizes
        | Fse (File (name, size)) :: xs ->
            let newSizes =
                dirsToAddTo
                |> List.fold
                    (fun s dir ->
                        Map.change dir ((Option.defaultValue 0) >> ((+) size) >> Some) s) sizes
            f dirsToAddTo xs newSizes
        | [] -> sizes

    f [] lst Map.empty

let dirSizes =
    data
    |> List.ofArray
    |> getSizes
    |> Map.toArray
    
let ans1 =
    dirSizes
    |> Array.filter (fun (k,v) -> v <= 100_000)
    |> Array.sumBy snd

ans1

/// Part 2

let totalSpace = 70_000_000
let wantedSpace = 30_000_000

let currentlyUsed =
    data
    |> Array.sumBy (function | Fse (File (_,size)) -> size
                             | _ -> 0)

let availSpace = totalSpace - currentlyUsed
let neededToClearUp = wantedSpace - availSpace

let ans2 =
    dirSizes
    |> Array.filter (fun (k,v) -> v >= neededToClearUp)
    |> Array.minBy snd
    |> snd
    

ans2