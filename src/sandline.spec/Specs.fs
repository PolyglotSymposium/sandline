module Specs

open System.IO

open Fuchu
open Swensen.Unquote

open Sandline

let saveCode code =
    let file = Path.ChangeExtension(Path.GetTempFileName(), "fsx")  
    File.WriteAllText(file, code)
    file

[<Tests>]
let specs = 
    testList "Purity checking" [
        testCase "A simple let statement with unit is pure" <| fun _ ->
            let filepath = saveCode """
            module MyLibrary

            let foo = 0
            """
            test <@ checkPurity filepath = Pure @>
    ]
