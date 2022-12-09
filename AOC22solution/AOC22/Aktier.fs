module Aktier
open Typer
open Common
open Input

type handel = 
    {
        Instrument : string
        Antal : float // nemmere med float
        Bogførtbeløb : float
        Pris : float
        // Mangler dato
    }
    static member parser (str : string) = 
        let strsplit = str.Split ";"
        {
            Instrument = strsplit.[0]
            Antal = strsplit.[1] |> float
            Bogførtbeløb = strsplit.[2].Replace(",", ".") |> float
            Pris = (strsplit.[2].Replace(",", ".") |> float) / (strsplit.[1] |> float)
        }
    static member erSalg h = h.Antal < 0
    static member erSplit h = h.Bogførtbeløb = 0.0

type augmentedHandel = 
    {
        Handel : handel
        AntalTilbage : float // nemmere med float
        GAK : float
        RealiseretGevinst : float
        // Mangler dato
    }

let getSplitFaktor (splitSalg : handel) (splitKøb : handel) = 
    - splitKøb.Antal / splitSalg.Antal

let test h = 
    2

let getAntal (handler : seq<handel>) = 
    handler |> Seq.sumBy (fun h -> h.Antal)
  
let run() = 
    let data = 99 |> Input.path |> Input.readLines

    let aktier = data |> Seq.toArray |> Array.map handel.parser
            

    let aktgrp = aktier |> Array.groupBy (fun h -> h.Instrument) |> Map

    let a = aktgrp |> Map.map (fun k v -> 
        let mutable antalTilbage = 0.0
        let mutable GAK = 0.0
        let mutable splitHandler = []

        // du kan lave noget tmp mutable her til at holde information omkring split

        let a = 
            v |> Array.map (fun h -> 

                // Håndter split 
                match handel.erSplit h with
                | true -> 
                    // vent til der er to handler 
                    match splitHandler.Length with 
                    | 0 -> 
                        // Ikke ramt nogen splithandler endnu, tilføj og opdater ikke noget
                        splitHandler <- [h]
                        {
                            Handel = h
                            AntalTilbage = antalTilbage
                            GAK = GAK
                            RealiseretGevinst = 0
                        }
                    | 1 -> 
                        // Vi har den anden splithandel klar. Tilføj den anden i parret og sorter.
                        splitHandler <- splitHandler |> List.append [h] |> List.sortBy (fun h -> h.Antal) |> Common.exactlyTwo

                        let faktor = getSplitFaktor splitHandler.[0] splitHandler.[1]

                        antalTilbage <- antalTilbage * faktor
                        GAK <- GAK / faktor

                        // Husk at sætte splithandler tilbage
                        splitHandler <- []
                        {
                            Handel = h
                            AntalTilbage = antalTilbage
                            GAK = GAK
                            RealiseretGevinst = 0
                        }
                    | _ -> failwith "For mange split!"
                | false -> 
                    GAK <- 
                        match h.Antal < 0.0 with
                        | true -> GAK
                        | false -> 
                            match antalTilbage with 
                            | 0.0 -> h.Bogførtbeløb/(h.Antal)
                            | _ ->
                                (antalTilbage*GAK + h.Pris*h.Antal)/(antalTilbage + h.Antal)

                    antalTilbage <- antalTilbage + h.Antal 

                    let gevinst = 
                        match h.Antal < 0 && (not (handel.erSplit h)) with
                        | false -> 0.0
                        | true -> 
                            let købtTil = -h.Antal * GAK
                            let solgtTil = -h.Bogførtbeløb
                            solgtTil - købtTil

                    {
                        Handel = h
                        AntalTilbage = antalTilbage
                        GAK = GAK
                        RealiseretGevinst = gevinst
                    }
            )

        a
        )




    let res = a


    (*
    
    Mangler: 
    dato
    mere test

    *)
    ()