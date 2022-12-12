#load "Helpers.fsx"

open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data =
    Helpers.Web.getInput 9
    |> Array.map (Helpers.split " ")
    |> Array.map (fun [|c;i|] -> char c, int i)
    |> Array.collect (fun (c,i) -> Array.init i (fun _ -> c))

let moveDir =
    function
    | 'L' -> (-1,0)
    | 'R' -> (1,0)
    | 'U' -> (0,1)
    | 'D' -> (0,-1)

let tupAdd (t1x,t1y) (t2x,t2y) = (t1x+t2x,t1y+t2y)

let tailMove (tX,tY) (hX,hY) =
    let xDiff = abs (hX-tX)
    let yDiff = abs (hY-tY)
    if (xDiff > 1 || yDiff > 1) then
        (sign (hX-tX), sign (hY-tY))
    else
        (0,0)

let doMove (tPos, hPos) dir =
    let newH = tupAdd hPos (moveDir dir)
    let newT = tupAdd tPos (tailMove tPos newH)
    (newT,newH)

let ans1 =
    data
    |> Array.scan doMove ((0,0),(0,0))
    |> Array.map fst
    |> Array.distinct
    |> Array.length

ans1

/// Part 2

let doMove2 (pos : (int*int) array) dir =
    let newH = tupAdd pos[0] (moveDir dir)

    [|1..9|]
    |> Array.scan (fun p i -> tupAdd pos[i] (tailMove pos[i] p)) newH

let ans2 =
    data
    |> Array.scan doMove2 (Array.replicate 10 (0,0))
    |> Array.map (fun lst -> lst[9])
    |> Array.distinct
    |> Array.length

ans2