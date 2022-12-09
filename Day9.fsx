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
    //printfn "H: %A T: %A" newH newT
    (newT,newH)

//let doInstruction (dir,count) tPos hPos =
//    [1..count] |> List.scan (fun s _ -> doMove s dir) (tPos,hPos)

let res =
    data |> Array.scan doMove ((0,0),(0,0))
    |> Array.map fst
    |> Array.distinct
    |> Array.length

let ans1 = data

ans1

/// Part 2

let ans2 = data

ans2