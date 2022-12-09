#load "Helpers.fsx"

open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data =
    Helpers.Web.getInput 8
    |> Array.map (fun s -> s.ToCharArray() |> Array.map (string >> int))

let data2d = Array2D.init data.Length data[0].Length (fun i j -> data[i][j])

let checkIdx (i,j) =
    let e = data2d[i,j]

    let rowFromLeft = data2d[i,..j-1] |> Array.forall (fun i -> e > i)
    let rowFromRight = data2d[i,j+1..] |> Array.forall (fun i -> e > i)

    let colFromUp = data2d[..i-1,j] |> Array.forall (fun i -> e > i)
    let colFromDown = data2d[i+1..,j] |> Array.forall (fun i -> e > i)

    rowFromLeft || rowFromRight || colFromUp || colFromDown

let ans1 = 
    Array.allPairs [|1..data.Length-2|] [|1..data.Length-2|]
    |> Array.where checkIdx
    |> Array.length
    |> (fun l -> l + 2*data.Length + 2*(data.Length-2))

ans1

/// Part 2

let score (i,j) =
    let e = data2d[i,j]

    let seenTrees (slice : int array) =
        let tallerThan = slice |> Array.takeWhile (fun i -> e > i) |> Array.length
        if (tallerThan = slice.Length) then
            tallerThan
        else
            tallerThan + 1

    let rowFromLeft  = seenTrees (Array.rev data2d[i,..j-1])
    let rowFromRight = seenTrees data2d[i,j+1..]
    let colFromUp    = seenTrees (Array.rev data2d[..i-1,j])
    let colFromDown  = seenTrees data2d[i+1..,j]
        
    rowFromLeft * rowFromRight * colFromUp * colFromDown

let ans2 =
    Array.allPairs [|1..data.Length-2|] [|1..data.Length-2|]
    |> Array.maxBy score
    |> score

ans2