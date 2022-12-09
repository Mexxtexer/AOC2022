module Dag3
open Typer
open Common


let getCommonChar (str : string) = 
    let s1, s2 = str |> splitStringInHalf
    let commonStr = s1 |> String.filter (fun c -> c |> s2.Contains) // kan være "LL"
    match commonStr |> allEqual with
    | true -> commonStr.[0]
    | false -> failwith "Flere forskellige ens! Bør ikke ske"

let getCommonChars (str2 : string) (str1 : string) = 
    str1 |> String.filter (fun c -> c |> str2.Contains) 

// kan laves med en fold for n.. 
let rec getCommonChar3 (strs : seq<string>) = 
    let strsArr = strs |> Seq.toArray
    let str1, str2, str3 = strsArr.[0], strsArr.[1], strsArr.[2]
    let commons = str1 |> getCommonChars str2 |> getCommonChars str3
    match commons |> allEqual with
    | true -> commons.[0]
    | false -> failwith "Flere forskellige ens! Bør ikke ske"

let run (dag: int) = 
    let data = dag |> Input.path |> Input.readLines

    let res = 
        data 
        |> Seq.map (getCommonChar >> Common.charNumber) 
        |> Seq.sum

    let res2 = 
        data 
        |> Input.groupByEveryNRows 3
        |> Seq.map ((fun (_, bags) -> bags |> Seq.map snd |> getCommonChar3 ) >> Common.charNumber)
        |> Seq.sum

    printfn "Resultat af dag %i: Du har fået %i point!" dag res
    printfn "Resultat af dag %i (del 2): Du har fået %i point!" dag res2
    ()