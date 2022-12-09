module Dag6
open Typer
open Common
open Input

    

let run (dag: int) = 
    let data = dag |> Input.path |> Input.readLines

    let strs = 
        data |> Seq.head |> Seq.toArray |> Array.toSeq |> Seq.map string |> (seqPairwiseN 4)
        |> Seq.map Seq.toList
    
    let strs2 = 
        data |> Seq.head |> Seq.toArray |> Array.toSeq |> Seq.map string |> (seqPairwiseN 14)
        |> Seq.map Seq.toList


    
    let res = Seq.findIndex allDifferent strs + 1 + 3// + 1 for index +3 for næste 3 chars
    let res2 = Seq.findIndex allDifferent strs2 + 1 + 13// + 1 for index +3 for næste 3 chars
    
    printfn "Resultat af dag %i: Du har fået %i point!" dag res
    printfn "Resultat af dag %i (del 2): Du har fået %i point!" dag res2
    ()