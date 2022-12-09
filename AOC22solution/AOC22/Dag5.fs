module Dag5
open Typer
open Common

type crateCollection = Map<int, char[]>

let addToCrates i c (crates : crateCollection) =
    match crates.ContainsKey i with
    | true -> crates.Add (i, (Array.append crates.[i] [|c|]))
    | false -> crates.Add (i, [|c|])

let getNumsFromStringWithDel  (del : char) (str : string) = 
    let split = str.Split del
    Array.head split |> int, 
    Array.tail split |> Array.exactlyOne |> int
  
let fillInitCrates (initCrates : seq<seq<string>>) = 
    
    let mutable crates : crateCollection = Map.empty

    initCrates 
    |> Seq.iter (fun r -> 
        r 
        |> Seq.iteri (fun i v ->
            match v.[1] with
            | ' ' -> ()// empty
            | c ->
                crates <- addToCrates i c crates
                ()
        )
       )
    crates 

let arrTakeFromTail n (arr : 'a[]) =
    arr |> Array.skip (arr.Length - n)

let arrRemoveFromTail n (arr : 'a[]) =
    arr |> Array.rev |> Array.skip n |> Array.rev

let moveCratesMut (crates : byref<crateCollection>) ((move, from, too) : int*int*int) =
    let cratesFromNew = crates.[from]
    let cratesToNew = Array.append crates.[too] (arrTakeFromTail move crates.[from])

    crates <- crates.Add (from, cratesFromNew)
    crates <- crates.Add (too, cratesToNew)


let moveCrates (crates : crateCollection) ((move, from, too) : int*int*int) (revOrder : bool)=
    let cratesFromNew = crates.[from] |> arrRemoveFromTail move
    let cratesToNew = 
        match revOrder with
        | true -> Array.append crates.[too] (arrTakeFromTail move crates.[from] |> Array.rev)
        | false -> Array.append crates.[too] (arrTakeFromTail move crates.[from])

    let crates1 = crates.Add (from, cratesFromNew)
    crates1.Add (too, cratesToNew)
    

let run (dag: int) = 
    let data = dag |> Input.path |> Input.readLines

    let datahead = data |> Seq.take 8
    let datarest = data |> Seq.skip 10

    let regepx = "(.{3}) (.{3}) (.{3}) (.{3}) (.{3}) (.{3}) (.{3}) (.{3}) (.{3})"
    // test
    let initCrates = datahead |> Seq.rev |> Seq.map (Common.re.getResults regepx)
    
    let mutable crates = fillInitCrates initCrates
    let mutable crates2 = fillInitCrates initCrates
    
    let regepxmoves = "move (.*?) from (.*?) to (.*)" // https://regex101.com/
    let moves = // move x from y to z
        datarest 
        |> Seq.map (Common.re.getResults regepxmoves >> (fun ss -> ss |> Seq.toArray |> (fun arr -> arr.[0] |> int, (arr.[1] |> int)-1, (arr.[2] |> int)-1)))

    moves |> Seq.iter (fun moves -> 
        crates <- moveCrates crates moves true
        crates2 <- moveCrates crates2 moves false
        )

    let res = crates.Values |> Seq.map Array.last |> Seq.toArray |> System.String 
    let res2 = crates2.Values |> Seq.map Array.last |> Seq.toArray |> System.String 

    printfn "Resultat af dag %i: Du ligger %s på toppen!" dag res
    printfn "Resultat af dag %i (del 2): Du ligger %s på toppen!" dag res2
    ()