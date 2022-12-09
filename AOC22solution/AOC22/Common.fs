module Common

open System.Text.RegularExpressions

/// DOK: map for tuple
let tupleMap (f : 'a->'b) (t : 'a*'a) = 
    let (t1, t2) = t
    (f t1, f t2)

let splitStringInHalf (str : string) = 
    str.[0..(str.Length/2-1)], str.[str.Length/2..str.Length]

let allEqual (s : string) = 
    s |> String.forall (fun c -> c = (s.[0] |> char))

let isLowerCase c = 
    let cStr = (c |> string)
    cStr.ToLower() = cStr

let charNumber (c : char) = 
    match isLowerCase c with 
    | true -> int c - int 'a' + 1
    | false -> 26 + int ((c |> string).ToLower() |> char) - int 'a' + 1

let arrTakeFromTail n (arr : 'a[]) =
    arr |> Array.skip (arr.Length - n)

let arrRemoveFromTail n (arr : 'a[]) =
    arr |> Array.rev |> Array.skip n |> Array.rev

let allDifferent (l : List<'a>) =
    (l |> List.distinct).Length = l.Length

let exactlyTwo (l : List<'a>) = 
    match l.Length = 2 with
    | true -> l
    | false -> failwith "Ikke 2!"

module re =
    // https://fsharpforfunandprofit.com/posts/convenience-active-patterns/
    let (|FirstRegexGroup|_|) pattern input =
       let m = Regex.Match(input,pattern)
       if (m.Success) then Some m.Groups.[1].Value else None

    let (|RegexGroups|_|) pattern input =
       let m = Regex.Match(input,pattern)
       if (m.Success) then Some m.Groups else None

    let getFromGroup startFrom (grp : GroupCollection) =
        seq { for i in startFrom .. grp.Count-1 do yield grp.[i].Value }

    let getResults regex str =
        match str with
        | RegexGroups regex host -> host |> getFromGroup 1
        | _ -> failwith "FEJL"
