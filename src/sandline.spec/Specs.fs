module Specs

open System.IO

open Fuchu
open Swensen.Unquote

open Sandline

let saveCode code =
    let file = Path.ChangeExtension(Path.GetTempFileName(), "fsx")  
    File.WriteAllText(file, code)
    file

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
        testCase "An let declaration with an if-then-else expression is impure if the condition is impure" <| fun _ ->
            let filepath = saveCode """
            module MyLibrary

            let ifthenelse x y z = if !x then y else z
            """
            test <@ checkPurity filepath = Impure("MyLibrary.ifthenelse", CallsImpureCode(bang, UsesMutability)) @>
        testCase "An let declaration with an if-then-else expression is impure if the consequent is impure" <| fun _ ->
            let filepath = saveCode """
            module MyLibrary

            let ifthenelse x y z = if x then !y else z
            """
            test <@ checkPurity filepath = Impure("MyLibrary.ifthenelse", CallsImpureCode(bang, UsesMutability)) @>
        testCase "An let declaration with an if-then-else expression is impure if the alternate is impure" <| fun _ ->
            let filepath = saveCode """
            module MyLibrary

            let ifthenelse x y z = if x then y else !z
            """
            test <@ checkPurity filepath = Impure("MyLibrary.ifthenelse", CallsImpureCode(bang, UsesMutability)) @>
        testCase "An if-then-else expression is impure even if its impure alternate is never executed" <| fun _ ->
            let filepath = saveCode """
            module MyLibrary

            let ifthenelse x y z = if true then y else !z
            """
            test <@ checkPurity filepath = Impure("MyLibrary.ifthenelse", CallsImpureCode (bang, UsesMutability)) @>
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
            test <@ checkPurity filepath = Impure("MyLibrary.foo", UsesMutability) @>
        testCase "A let declaration with a ref is impure" <| fun _ ->
            let filepath = saveCode """
            module MyLibrary

            let foo = ref 0
            """
            test <@ checkPurity filepath = Impure("MyLibrary.foo", CallsImpureCode ("Microsoft.FSharp.Core.Operators.ref",UsesMutability)) @>
        testCase "A function returning a ref, not dependent on inputs is impure" <| fun _ ->
            let filepath = saveCode """
            module MyLibrary

            let foo () = ref 0
            """
            test <@ checkPurity filepath = Impure("MyLibrary.foo", CallsImpureCode ("Microsoft.FSharp.Core.Operators.ref",UsesMutability)) @>
        testCase "A function dereferencing a ref is impure" <| fun _ ->
            let filepath = saveCode """
            module MyLibrary

            let foo x = 3 + !x
            """
            test <@ checkPurity filepath = Impure("MyLibrary.foo", CallsImpureCode (bang,UsesMutability)) @>
        testCase "A function mutating a ref is impure" <| fun _ ->
            let filepath = saveCode """
            module MyLibrary

            let foo x =
                x := 3
            """
            test <@ checkPurity filepath = Impure("MyLibrary.foo", CallsImpureCode("Microsoft.FSharp.Core.Operators.( := )",UsesMutability)) @>
        testCase "A function that raises an exception is impure" <| fun _ ->
            let filepath = saveCode """
            module MyLibrary

            let foo() =
                raise <| System.Exception()
            """
            test <@ checkPurity filepath = Impure("MyLibrary.foo", CallsImpureCode("Microsoft.FSharp.Core.Operators.raise", UsesExceptions)) @>
        testCase "A function that catches an exception is impure" <| fun _ ->
            let filepath = saveCode """
            module MyLibrary

            let foo bar =
                try
                    bar()
                with _ -> ()
            """
            test <@ checkPurity filepath = Impure ("MyLibrary.foo",CallsImpureCode ("try...with",UsesExceptions)) @>
    ]
