module Callparameters

open Argu
open System.IO
open Typer 


type CallArguments =
    | [<AltCommandLine("-n")>]     Dag of n : int
with
    interface IArgParserTemplate with
        member s.Usage =
            let appSettings = System.Configuration.ConfigurationManager.AppSettings
            match s with
            | Dag _            -> sprintf "Antal esg-stier. Default er %A. Overskrives hvis stinumre angives. " 1
            
type ExceptionExiter() =
        interface IExiter with
            member __.Name = "ArguException Exiter"
            member __.Exit(msg, errorCode) =
                do
                    printfn "%s" msg
                    printfn "Forkert syntaks! Brug --help for flere detaljer"
                    let a = System.Console.ReadKey()
                    ()
                exit (int errorCode)
    
let input argv : Kørselsparametre =
    let parser = ArgumentParser.Create<CallArguments>(errorHandler = ExceptionExiter())
    let args   = parser.Parse(argv, raiseOnUsage = false)

    [args.IsUsageRequested] |> List.filter id |> List.iter (fun x -> parser.PrintUsage() |> printfn "%s")
    let dag = args.TryGetResult<@Dag@> |> Option.defaultValue 1
    
    
    { Dag         = dag     }