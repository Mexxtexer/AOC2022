module Dag2
open Typer

let run (dag: int) = 
    let data = dag |> Input.path |> Input.readLines

    let res = 
        data 
        |> Seq.map (rps.fromStringPair >> rps.getPoints >> rps.getPointFromPoints) 
        |> Seq.sum

    let modifyData (oppPlay : int, req : int) =
        oppPlay, rps.getPlay oppPlay req

    let res2 = 
        data
        |> Seq.map (rps.fromStringPair >> rps.getPoints >> modifyData >> rps.getPointFromPoints) 
        |> Seq.sum

    printfn "Resultat af dag %i: Du har fået %i point!" dag res
    printfn "Resultat af dag %i (del 2): Du har fået %i point!" dag res2
    ()