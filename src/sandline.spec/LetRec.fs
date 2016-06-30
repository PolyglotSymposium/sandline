module LetRec

open Fuchu
open Swensen.Unquote

open Sandline

[<Tests>]
let specs = 
    testList "Purity checking for recursive definitions" [
        testCase "A simple infinitely recursive function is pure" <| fun _ ->
            let filepath = saveCode """
            module MyLibrary

            let rec a x =
                a x
            """
            test <@ checkPurity filepath = Pure @>
        testCase "An application of a simple infinitely recursive function is pure" <| fun _ ->
            let filepath = saveCode """
            module MyLibrary

            let rec a x =
                a x

            let x : int = a 1
            """
            test <@ checkPurity filepath = Pure @>
    ]