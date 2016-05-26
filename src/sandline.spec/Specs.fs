module Specs

open Fuchu
open Swensen.Unquote

[<Tests>]
let specs = 
    testList "Bootstrap" [
        testCase "Bootstrap" <| fun _ ->
            test <@ true @>
    ]
