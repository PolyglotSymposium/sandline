﻿module Specs

open System.IO

open Fuchu
open Swensen.Unquote

open Sandline

let saveCode code =
    let file = Path.ChangeExtension(Path.GetTempFileName(), "fsx")  
    File.WriteAllText(file, code)
    file

let mutabilityEvidenceName purity =
    match purity with
    | Impure (UsesMutability symbol) -> Some symbol.FullName
    | _ -> None

[<Tests>]
let specs = 
    testList "Purity checking" [
        testCase "A simple let statement with unit is pure" <| fun _ ->
            let filepath = saveCode """
            module MyLibrary

            let foo = ()
            """
            test <@ checkPurity filepath = Pure @>
        testCase "A simple let statement with zero is pure" <| fun _ ->
            let filepath = saveCode """
            module MyLibrary

            let foo = 0
            """
            test <@ checkPurity filepath = Pure @>
        testCase "Parametric identity function is pure" <| fun _ ->
            let filepath = saveCode """
            module MyLibrary

            let id x = x
            """
            test <@ checkPurity filepath = Pure @>
        testCase "A mutable let statement with zero is impure" <| fun _ ->
            let filepath = saveCode """
            module MyLibrary

            let mutable foo = 0
            """
            test <@ mutabilityEvidenceName <| checkPurity filepath = Some "MyLibrary.foo" @>
        testCase "A let statement with a ref is impure" <| fun _ ->
            let filepath = saveCode """
            module MyLibrary

            let foo = ref 0
            """
            test <@ mutabilityEvidenceName <| checkPurity filepath = Some "Microsoft.FSharp.Core.Operators.ref" @>
        testCase "A function returning a ref, not dependent on inputs is impure" <| fun _ ->
            let filepath = saveCode """
            module MyLibrary

            let foo () = ref 0
            """
            test <@ mutabilityEvidenceName <| checkPurity filepath = Some "Microsoft.FSharp.Core.Operators.ref" @>
    ]
