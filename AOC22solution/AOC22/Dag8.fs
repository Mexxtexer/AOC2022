module Dag8
open Typer
open Common
open Input

let isVisible (tree : int) (tttr : int[]) (tttl : int[]) (tttup : int[]) (tttdown : int[]) =
    let vis_from_right = (tttr |> Array.max) < tree 
    let vis_from_left = (tttl |> Array.max) < tree 
    let vis_from_up = (tttup |> Array.max) < tree 
    let vis_from_down = (tttdown |> Array.max) < tree 
    vis_from_right || vis_from_left || vis_from_up || vis_from_down

let getScenicScore (tree : int) (tttr : int[]) (tttl : int[]) (tttup : int[]) (tttdown : int[]) =
    let vis_from_right = tttr |> Array.tryFindIndex (fun t -> t >= tree) |> (fun treeopt -> match treeopt with | Some idx -> idx + 1 | None -> tttr.Length)
    let vis_from_left = tttl |> Array.tryFindIndex (fun t -> t >= tree) |> (fun treeopt -> match treeopt with | Some idx -> idx + 1 | None -> tttl.Length)
    let vis_from_up = tttup |> Array.tryFindIndex (fun t -> t >= tree) |> (fun treeopt -> match treeopt with | Some idx -> idx + 1 | None -> tttup.Length)
    let vis_from_down = tttdown |> Array.tryFindIndex (fun t -> t >= tree) |> (fun treeopt -> match treeopt with | Some idx -> idx + 1 | None -> tttdown.Length)
    let score = vis_from_right * vis_from_left * vis_from_up * vis_from_down
    match score > 200000 with 
    | true -> score
    | false -> score
  
let run (dag: int) = 
    let data = dag |> Input.path |> Input.readLines |> Seq.toArray

    let nrows = (data |> Array.head |> String.length)
    let ncols = (data |> Array.length)

    let a = Array2D.init nrows ncols (fun i j -> data.[i].[j] |> string |> int)


    let mutable countTreesVisible = 0

    for i in [1..(nrows - 2)] do
        for j in [1..(ncols - 2)] do
            let tttr = a[i, (j+1)..]
            let tttl = a[i, 0..(j-1)]
            let tttup = a[0..(i-1), j]
            let tttdown = a[(i+1).., j]
            if isVisible a[i, j] tttr tttl tttup tttdown then countTreesVisible <- countTreesVisible + 1

    let mutable scScores = [||]

    for i in [1..(nrows - 2)] do
        for j in [1..(ncols - 2)] do
            let tttr = a[i, (j+1)..]
            let tttl = a[i, 0..(j-1)] |> Array.rev
            let tttup = a[0..(i-1), j] |> Array.rev
            let tttdown = a[(i+1).., j]
            scScores <- Array.append scScores [|getScenicScore a[i, j] tttr tttl tttup tttdown|]


    let res = countTreesVisible + 2*nrows + 2*ncols - 4
    let res2 = scScores |> Array.max
    
    printfn "Resultat af dag %i: %i" dag res
    printfn "Resultat af dag %i (del 2): %i" dag res2
    ()

