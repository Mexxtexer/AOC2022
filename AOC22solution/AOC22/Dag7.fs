module Dag7
open Typer
open Common
open Input



let run (dag: int) = 

    let root = filesystem.dir.getDirFromInput dag
    let sizes = filesystem.dir.getAllSizes root
    
    let usedSize = root |> filesystem.dir.size 
    let neededSize = 30000000
    let availableSize = 70000000
    let sizeToFree = neededSize - (availableSize - usedSize)
    let res = sizes |> Array.filter (fun x -> (snd x) < 100000) |> Array.sumBy snd 
    let res2 = sizes |> Array.filter (fun x -> (snd x) >= sizeToFree) |> Array.minBy snd |> snd
    
    printfn "Resultat af dag %i: %i" dag res
    printfn "Resultat af dag %i (del 2): %i" dag res2
    ()