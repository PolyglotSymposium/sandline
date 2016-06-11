module Tuple

open Fuchu
open Swensen.Unquote

open Sandline

[<Tests>]
let specs = 
    testList "Purity checking for tuples" [
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
    ]
