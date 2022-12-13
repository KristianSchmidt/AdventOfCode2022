#load "Helpers.fsx"

open System
open Helpers

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

type Tree = | List of Tree list | Int of int

let parse (s : string) =
    let rec f idx elems = 
        let subStr = s.Substring(idx)
        //printfn "%s" subStr
        match subStr with
        | Regex "^([\d,]+)" [s'] ->
            let len = s'.Length
            //printfn "s' = %s" s'
            let nums = Helpers.split "," s' |> Array.filter ((<>)"") |> Array.map (int >> Int) |> List.ofArray
            f (idx + len) (List.append elems nums)
        | Regex "^\[" [] ->
            let (list, nextIdx) = f (idx+1) []
            f nextIdx (List.append elems [list])
        | Regex "^\]" [] ->
            List elems, idx+1
        | "" -> List.head elems, -1

    f 0 [] |> fst

let data =
    Helpers.Web.getInput 13
    |> Array.chunkBySize 3
    |> Array.map (fun arr -> [parse arr[0]], [parse arr[1]])

let rec compare (left : Tree list, right : Tree list) =
    //printfn "Left:  %A" left
    //printfn "Right: %A" right
    //printfn ""
    match left, right with
    | Int l :: xsl, Int r :: xsr ->
        if (l < r) then
            -1
        else if (l = r) then
            compare (xsl, xsr)
        else
            1
    | List listL :: xsl, List listR :: xsr ->
        match compare (listL, listR) with
        | 0 -> compare (xsl, xsr)
        | x -> x
    | Int l :: xsl, List listR :: xsr ->
        match compare (([List [Int l]]), [List listR]) with
        | 0 -> compare (xsl, xsr)
        | x -> x
    | List listL :: xsl, Int r :: xsr ->
        match compare ([List listL], ([List [Int r]])) with
        | 0 -> compare (xsl, xsr)
        | x -> x
    | (xsl, xsr) when xsl.Length = 0 && xsr.Length > 0 -> -1
    | (xsl, xsr) when xsl.Length > 0 && xsr.Length = 0 -> 1
    | (xsl, xsr) when xsl.Length = 0 && xsr.Length = 0 -> 0
    | a -> failwithf "%A" a

let ans1 =
    data
    |> Array.mapi (fun i (l,r) -> (i+1), compare (l,r))
    |> Array.filter (snd >> ((=)(-1)))
    |> Array.sumBy fst

ans1

/// Part 2

let (two, six) = parse "[[2]]", parse "[[6]]"

let all =
    data
    |> Array.collect (fun (l,r) -> [|l;r|])
    |> Array.append
        [|
            [two]
            [six]
        |]
    |> Array.sortWith (fun l r -> compare (l,r))

let idx2 = all |> Array.findIndex ((=)[two]) |> ((+)1)
let idx6 = all |> Array.findIndex ((=)[six]) |> ((+)1)

let ans2 = idx2 * idx6

ans2