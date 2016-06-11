[<AutoOpen>]
module TestInfrastructure

open System.IO

let saveCode code =
    let file = Path.ChangeExtension(Path.GetTempFileName(), "fsx")  
    File.WriteAllText(file, code)
    file
