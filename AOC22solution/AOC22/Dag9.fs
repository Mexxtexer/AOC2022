module Dag9
open Typer
open Common
open Input


type pos = int*int

type move = 
    | Up 
    | Down 
    | Left 
    | Right
    static member FromString (str : string) = 
        match str.ToLower() with 
        | "d" -> Down
        | "u" -> Up
        | "l" -> Left
        | "r" -> Right
        | _ -> failwith "FEJL"
    static member getCoordMove (move : move) = 
        match move with 
        | Up ->   (1, 0)
        | Down -> (-1, 0)
        | Left -> (0, -1)
        | Right-> (0, 1)
    static member FromCoord (coord : pos) = 
        match coord with 
        | (1, 0) -> Up   
        | (-1, 0) -> Down
        | (0, -1) -> Left 
        | (0, 1) -> Right
        | _ -> failwith "FEJL"

let addPosToMove (moveToAdd : pos) (moveToDo : move) = 
    let (m11, m12) = moveToAdd
    let (m21, m22) = moveToDo |> move.getCoordMove
    (m11 + m21, m12 + m22)

let addPosToPos (pos1 : pos) (pos2 : pos) = 
    let (m11, m12) = pos1
    let (m21, m22) = pos2
    (m11 + m21, m12 + m22)

let shouldMoveT (newH : pos) (T : pos) = 
    let (h1, h2) = newH
    let (t1, t2) = T

    abs(t1 - h1) > 1 || abs(t2 - h2) > 1

let getNewH (H : pos) (move : move) =
    addPosToMove H move

let getNewT (newH : pos) (T : pos) =
    match shouldMoveT newH T with
    | false -> T
    | true -> 
        let (h1, h2) = newH
        let (t1, t2) = T 
        let horDiff = h1 - t1
        let verDiff = h2 - t2
        let tPosMove = 
            match abs(horDiff), abs(verDiff) with // er enten 0, 1 eller 2
            | 0, _ -> 0, verDiff/2 // verDiff er -2 eller 2
            | _, 0 -> horDiff/2, 0 // horDiff er -2 eller 2
            | 1, 2 -> horDiff/1, verDiff/2 // diag
            | 2, 1 -> horDiff/2, verDiff/1 // diag
            | 2, 2 -> horDiff/2, verDiff/2 // diag v2
            | _ -> failwith "FEJL"

        addPosToPos T tPosMove


let run (dag: int) = 
    let data = dag |> Input.path |> Input.readLines |> Seq.toArray

    let moves = 
        data |> Array.collect (
            fun str -> 
                let spl = str.Split " "
                let moves = (spl.[0] |> move.FromString) |> Array.replicate (spl.[1] |> int)
                moves)
    let mutable H = (0, 0)
    let mutable T1 = (0, 0)
    let mutable T2 = (0, 0)
    let mutable T3 = (0, 0)
    let mutable T4 = (0, 0)
    let mutable T5 = (0, 0)
    let mutable T6 = (0, 0)
    let mutable T7 = (0, 0)
    let mutable T8 = (0, 0)
    let mutable T9 = (0, 0)
    let mutable Hpositions = [H]
    let mutable T1positions = [T1]
    let mutable T2positions = [T2]
    let mutable T3positions = [T3]
    let mutable T4positions = [T4]
    let mutable T5positions = [T5]
    let mutable T6positions = [T6]
    let mutable T7positions = [T7]
    let mutable T8positions = [T8]
    let mutable T9positions = [T9]

    moves
    |> Array.iteri (
        fun i m -> 
            H <- getNewH H m
            T1 <- getNewT H T1 
            T2 <- getNewT T1 T2 
            T3 <- getNewT T2 T3 
            T4 <- getNewT T3 T4 
            T5 <- getNewT T4 T5 
            T6 <- getNewT T5 T6 
            T7 <- getNewT T6 T7 
            T8 <- getNewT T7 T8 
            T9 <- getNewT T8 T9 

            Hpositions <- [H] |> List.append Hpositions
            T1positions <- [T1] |> List.append T1positions
            T2positions <- [T2] |> List.append T2positions
            T3positions <- [T3] |> List.append T3positions
            T4positions <- [T4] |> List.append T4positions
            T5positions <- [T5] |> List.append T5positions
            T6positions <- [T6] |> List.append T6positions
            T7positions <- [T7] |> List.append T7positions
            T8positions <- [T8] |> List.append T8positions
            T9positions <- [T9] |> List.append T9positions
        
        )




    let res = T1positions |> List.groupBy id |> Map |> (fun map -> map.Keys.Count)
    let res2 = T9positions |> List.groupBy id |> Map |> (fun map -> map.Keys.Count)
    
    printfn "Resultat af dag %i: %i" dag res
    printfn "Resultat af dag %i (del 2): %i" dag res2
    ()