module ProgramBody

open Typer

let run (input : Kørselsparametre) =
    
    match input.Dag with
    | 99 -> Aktier.run()
    | 1 -> Dag1.run 1
    | 2 -> Dag2.run 2
    | 3 -> Dag3.run 3
    | 4 -> Dag4.run 4
    | 5 -> Dag5.run 5
    | 6 -> Dag6.run 6
    | 7 -> Dag7.run 7
    | 8 -> Dag8.run 8
    | 9 -> Dag9.run 9
    | _ -> failwith "Not implemented yet!"
    