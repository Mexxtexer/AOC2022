module DagTom
open Typer
open Common
open Input

  

let run (dag: int) = 
    let data = dag |> Input.path |> Input.readLines










    let res = 0
    let res2 = 0
    
    printfn "Resultat af dag %i: %i" dag res
    printfn "Resultat af dag %i (del 2): %i" dag res2
    ()