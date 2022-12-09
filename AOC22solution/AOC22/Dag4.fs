module Dag4
open Typer
open Common


let getNumsFromStringWithDel  (del : char) (str : string) = 
    let split = str.Split del
    Array.head split |> int, 
    Array.tail split |> Array.exactlyOne |> int
    


let run (dag: int) = 
    let data = dag |> Input.path |> Input.readLines

    let res = 
        data 
        |> Seq.map (fun s -> 
            let s1, s2 = 
                let split = s.Split ','
                Array.head split, Array.tail split |> Array.exactlyOne
            s1 |> getNumsFromStringWithDel '-',
            s2 |> getNumsFromStringWithDel '-'
            )
        |> Seq.filter (fun ((min1, max1), (min2, max2)) -> 
            let twoIncInOne = min2 >= min1 && max2 <= max1 
            let oneIncInTwo = min1 >= min2 && max1 <= max2
            twoIncInOne || oneIncInTwo
            )
        |> Seq.length

    let res2 = 
        data 
        |> Seq.map (fun s -> 
            let s1, s2 = 
                let split = s.Split ','
                Array.head split, Array.tail split |> Array.exactlyOne
            s1 |> getNumsFromStringWithDel '-',
            s2 |> getNumsFromStringWithDel '-'
            )
        |> Seq.filter (fun ((min1, max1), (min2, max2)) -> 
            let twoIncInOne = 
                (min2 >= min1 && min2 <= max1 ) ||
                (max2 >= min1 && max2 <= max1 )
            let oneIncInTwo = 
                (min1 >= min2 && min1 <= max2 ) ||
                (max1 >= min2 && max1 <= max2 )
            twoIncInOne || oneIncInTwo
            )
        |> Seq.length

    printfn "Resultat af dag %i: Du har fået %i point!" dag res
    printfn "Resultat af dag %i (del 2): Du har fået %i point!" dag res2
    ()