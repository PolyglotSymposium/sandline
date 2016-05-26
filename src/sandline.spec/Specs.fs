module Specs

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
    | Impure (Symbol symbol, UsesMutability) -> symbol.FullName
    | Impure (Name name, UsesMutability) -> name
    | Impure (_, reason) -> failwithf "Expected impure because of mutability; was impure because of: %A" reason
    | Pure -> failwith "Expected impure because of mutability; was pure"
    | Unknown reason -> failwithf "Expected impure because of mutability; was Unknown because of %s" reason

let exceptionsEvidenceName purity =
    match purity with
    | Impure (Symbol symbol, UsesExceptions) -> symbol.FullName
    | Impure (Name name, UsesExceptions) -> name
    | Impure (_, reason) -> failwithf "Expected impure because of exceptions; was impure because of: %A" reason
    | Pure -> failwith "Expected impure because of exceptions; was pure"
    | Unknown reason -> failwithf "Expected impure because of exceptions; was Unknown because of %s" reason

let bang = "Microsoft.FSharp.Core.Operators.( ! )"

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
        testCase "A simple let statement with addition is pure" <| fun _ ->
            let filepath = saveCode """
            module MyLibrary

            let foo = 3 + 5
            """
            test <@ checkPurity filepath = Pure @>
        testCase "An if-then-else expression is pure in and of itself" <| fun _ ->
            let filepath = saveCode """
            module MyLibrary

            let ifthenelse x y z = if x then y else z
            """
            test <@ checkPurity filepath = Pure @>
        testCase "An if-then-else expression is impure if the condition is condition is impure" <| fun _ ->
            let filepath = saveCode """
            module MyLibrary

            let ifthenelse x y z = if !x then y else z
            """
            test <@ mutabilityEvidenceName <| checkPurity filepath = bang @>
        testCase "An if-then-else expression is impure if the condition is consequent is impure" <| fun _ ->
            let filepath = saveCode """
            module MyLibrary

            let ifthenelse x y z = if x then !y else z
            """
            test <@ mutabilityEvidenceName <| checkPurity filepath = bang @>
        testCase "An if-then-else expression is impure if the condition is alternative is impure" <| fun _ ->
            let filepath = saveCode """
            module MyLibrary

            let ifthenelse x y z = if x then y else !z
            """
            test <@ mutabilityEvidenceName <| checkPurity filepath = bang @>
        testCase "An if-then-else expression is impure even if its impure alternate is never executed" <| fun _ ->
            let filepath = saveCode """
            module MyLibrary

            let ifthenelse x y z = if true then y else !z
            """
            test <@ mutabilityEvidenceName <| checkPurity filepath = bang @>
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
            test <@ mutabilityEvidenceName <| checkPurity filepath = "MyLibrary.foo" @>
        testCase "A let statement with a ref is impure" <| fun _ ->
            let filepath = saveCode """
            module MyLibrary

            let foo = ref 0
            """
            test <@ mutabilityEvidenceName <| checkPurity filepath = "Microsoft.FSharp.Core.Operators.ref" @>
        testCase "A function returning a ref, not dependent on inputs is impure" <| fun _ ->
            let filepath = saveCode """
            module MyLibrary

            let foo () = ref 0
            """
            test <@ mutabilityEvidenceName <| checkPurity filepath = "Microsoft.FSharp.Core.Operators.ref" @>
        testCase "A function dereferencing a ref is impure" <| fun _ ->
            let filepath = saveCode """
            module MyLibrary

            let foo x = 3 + !x
            """
            test <@ mutabilityEvidenceName <| checkPurity filepath = bang @>
        testCase "A function dereferencing a ref is impure" <| fun _ ->
            let filepath = saveCode """
            module MyLibrary

            let foo x =
                x := 3
            """
            test <@ mutabilityEvidenceName <| checkPurity filepath = "Microsoft.FSharp.Core.Operators.( := )" @>
        testCase "A function that raises an exception is impure" <| fun _ ->
            let filepath = saveCode """
            module MyLibrary

            let foo() =
                raise <| System.Exception()
            """
            test <@ exceptionsEvidenceName <| checkPurity filepath = "" @>
        testCase "A function that catches an exception is impure" <| fun _ ->
            let filepath = saveCode """
            module MyLibrary

            let foo bar =
                try
                    bar()
                with _ -> ()
            """
            test <@ exceptionsEvidenceName <| checkPurity filepath = "" @>
    ]
