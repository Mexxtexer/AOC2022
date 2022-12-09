[<EntryPoint>]
let main argv =
    let sw = System.Diagnostics.Stopwatch.StartNew()
    let input = Callparameters.input argv
    ProgramBody.run input
    sw.Stop()
    0 // return an integer exit code