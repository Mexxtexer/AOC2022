module Dag1

let run (dag: int) = 
    let data = dag |> Input.path |> Input.readLines

    let rec addElveNumber (res : array<int*int>) elve l =
        match l with
        | [] -> res
        | head::tail ->
            match head with
            | "" -> addElveNumber res (elve + 1) tail
            | strCal -> addElveNumber (Array.append res [|(elve, int strCal)|]) elve tail


    let numberedData = data |> Seq.toList |> addElveNumber Array.empty 1
    let elveCalsDesc = 
        numberedData 
        |> Array.groupBy fst 
        |> Array.map (fun (elve, cals) -> elve, cals |> Array.sumBy snd)
        |> Array.sortByDescending snd

    let res = elveCalsDesc |> Array.head
    let resdel2 = elveCalsDesc |> Array.take 3
    printfn "Resultat af dag %i: Elv nr. %i har %i kalorier!" dag (fst res) (snd res)
    printfn "Resultat af dag %i (del 2): Top 3 elve har %i kalorier!" dag (resdel2 |> Array.sumBy snd)
    ()