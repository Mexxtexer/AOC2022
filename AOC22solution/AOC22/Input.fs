module Input

open System.IO

let readLines (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

let path (dag : int) = __SOURCE_DIRECTORY__ + "\..\..\input\dag" + dag.ToString() + ".txt"

let groupByEveryNRows (n : int) (lines : seq<string>) = 
    let ids = 
        seq {
            for x in 0..(((lines |> Seq.length) - 1)/n) do
                yield! seq { for i in 1..n -> x}
        }
    lines |> Seq.zip ids |> Seq.groupBy fst

let seqPairwiseN (n : int) (lines : seq<'a>) = 
    let linesArr = lines |> Seq.toArray
    let res = 
        seq {
            for x in 0..(((lines |> Seq.length) - 1 - n)) do
                yield seq { for i in 1..n -> linesArr.[x + i-1]}
        }
    res

