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
            test <@ checkPurity filepath = Impure("MyLibrary.foo",CallsImpureCode ("try...with",UsesExceptions)) @>
        testCase "A function with a try/finally is impure" <| fun _ ->
            let filepath = saveCode """
            module MyLibrary

            let foo bar =
                try
                    bar()
                finally
                    ()
            """
            test <@ checkPurity filepath = Impure("MyLibrary.foo",CallsImpureCode ("try...finally",UsesExceptions)) @>
        testCase "A value that is defined in terms of another pure value is pure" <| fun _ ->
            let filepath = saveCode """
            module MyLibrary

            let a = 40

            let b = a + 2
            """
            test <@ checkPurity filepath = Pure @>
        testCase "A function that is pure and defined in terms of a pure value is pure" <| fun _ ->
            let filepath = saveCode """
            module MyLibrary

            let a = 40

            let b c = a + c
            """
            test <@ checkPurity filepath = Pure @>
        testCase "When the main body of a module uses a try it is impure" <| fun _ ->
            let filepath = saveCode """
            module MyLibrary

            try
                let a = "I don't really care"
            with _ -> ()
            """
            test <@ checkPurity filepath = Impure("try...with", UsesExceptions) @>
        testCase "A tuple in and of itself is pure" <| fun _ ->
            let filepath = saveCode """
            module MyLibrary

            let a = (42, "I don't really care")
            """
            test <@ checkPurity filepath = Pure @>
        testCase "A tuple with an impure left value is impure" <| fun _ ->
            let filepath = saveCode """
            module MyLibrary

            let a x = (!x, "I don't really care")
            """
            test <@ checkPurity filepath = Impure("MyLibrary.a", CallsImpureCode (bang, UsesMutability)) @>
        testCase "A tuple with an impure right value is impure" <| fun _ ->
            let filepath = saveCode """
            module MyLibrary

            let a x = ("foo", !x)
            """
            test <@ checkPurity filepath = Impure("MyLibrary.a", CallsImpureCode (bang, UsesMutability)) @>
        testCase "Tuple fst function is pure" <| fun _ ->
            let filepath = saveCode """
            module MyLibrary

            let a = fst ("foo", 1)
            """
            test <@ checkPurity filepath = Pure @>
        testCase "A let expression in and of itself is pure" <| fun _ ->
            let filepath = saveCode """
            module MyLibrary

            let a =
                let x = 1337
                in x
            """
            test <@ checkPurity filepath = Pure @>
        testCase "Let mutable expression is impure" <| fun _ ->
            let filepath = saveCode """
            module MyLibrary

            let a =
                let mutable x = 1337
                in x
            """
            test <@ checkPurity filepath = Impure("MyLibrary.a",CallsImpureCode ("x",UsesMutability)) @>
        testCase "A let expression with the name bound to an impure value is impure" <| fun _ ->
            let filepath = saveCode """
            module MyLibrary

            let a x =
                let y = !x
                in y
            """
            test <@ checkPurity filepath = Impure("MyLibrary.a", CallsImpureCode(bang, UsesMutability)) @>
        testCase "A let expression with an impure body is impure" <| fun _ ->
            let filepath = saveCode """
            module MyLibrary

            let a x =
                let y = x
                in !y
            """
            test <@ checkPurity filepath = Impure("MyLibrary.a", CallsImpureCode(bang, UsesMutability)) @>
    ]
